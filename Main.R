library(tigris)
library(gplots)
library(foreign)
library(car)
library(qwraps2)
library(knitr)
library(geosphere)
library(dplyr)
library(stargazer)

print('')
print('')

tax.data <- read.csv("../Assessor_Historical_Secured_Property_Tax_Rolls.csv")

affordable <- read.csv("../Residential_Projects_With_Inclusionary_Requirements.csv")

### DATA CLEANING

# Add census tract variable for affordable housing dataset
affordable$census_code <- apply(affordable, 1, function(row) call_geolocator_latlon(row['Latitude'], row['Longitude']))
affordable$census_tract <- as.numeric(substr(affordable$census_code, 6, 11))

# Add completion date variable for affordable housing dataset
affordable$completion_year <- as.numeric(substr(affordable$Actual.Estimated.Completion.Date, 7, 10))

# Simplify section 415 declaration data
affordable$simple415 <- as.character(affordable$Section.415.Declaration)

# Recode anything other than on-site or fee payment to 'other'
affordable$simple415[affordable$simple415 %in% 
                       c("Units for Off-site Project with On-site Obligation",
                         "Units for Off-site Project",
                         "Combination Project",
                         "Land Dedication",
                         "Land Dedication ",
                         "Off-site BMR Project",
                         "Off-site BMR Project/Fee Payment",
                         "Units for Off-site Project Units for Off-site Project with On-site Obligation")] <- "Other"

# On-site BMR project variable was messy and had space at end
affordable$simple415[affordable$simple415 %in% c("On-site BMR Project ", "On-site BMR Project/Fee Payment", "On-site BMR Project/Land Dedication")] <- "On-site BMR Project"

# Create latitude and longitude coordinates in the tax data
tax.data$latitude <- substr(tax.data$the_geom, 2, 9)
tax.data$longitude <- substr(tax.data$the_geom, regexpr(', ', tax.data$the_geom)+2,  regexpr(', ', tax.data$the_geom)+10)
tax.data$LocationKey <- paste("(", tax.data$latitude, ", ", tax.data$longitude, ")", sep="")

write.csv(tax.data, "tax_data.csv")

# Create latitude and longitude coordinates in the affordable data
affordable$latitude2 <- substr(affordable$Location, 2, 9)
affordable$longitude2 <- substr(affordable$Location, regexpr(', ', affordable$Location)+2,  regexpr(', ', affordable$Location)+10)
affordable$LocationKey <- paste("(", affordable$latitude2, ", ", affordable$longitude2, ")", sep="")

# Merge datasets
total <- merge(tax.data,affordable,by="LocationKey")

total2 <- total[order(total$Parcel.Number, total$Closed.Roll.Year),]

total.clean <- total2[c("LocationKey","Latitude", "Longitude", "Parcel.Number", "Property.Location", "Closed.Roll.Year", "Assessed.Land.Value", "Assessed.Improvement.Value", 
                        "simple415", "completion_year", "Use.Code", "Use.Definition", "Property.Class.Code", "Property.Class.Code.Definition",
                        "Year.Property.Built", "Property.Area", "Assessor.Neighborhood.District",
                        "Assessor.Neighborhood.Code", "Assessor.Neighborhood", "Supervisor.District.x", "Analysis.Neighborhood",
                        "Zip.Code", "census_code", "census_tract", "Project.ID", "Project.Status", "Housing.Tenure",
                        "Planning.Case.Number", "Section.415.Declaration",  "Actual.Estimated.Completion.Date", 
                        "Number.of.Bathrooms", "Number.of.Bedrooms", "Number.of.Rooms", "Number.of.Stories",
                        "Number.of.Units", "Project.Units", "Affordable.Units",
                        "Units.Subject.to.Section.415", "On.Site.Affordable.Units", "Off.Site.Affordable.Units", 
                        "Off.Site.Affordable.Units.at.This.Site", "SRO.Units", "Studio.Units","X1bd.Units", "X2bd.Units",
                        "X3bd.Units", "X4bd.Units","X30..AMI", "X50..AMI","X55..AMI", "X60..AMI","X80..AMI", "X90..AMI",
                        "X100..AMI", "X120..AMI","X150..AMI")]

# Recode messed up property names
total.clean$Property.Location <- as.character(total.clean$Property.Location)
total.clean[as.character(total.clean$"Property.Location") == "0000 0055 SITUS TO BE ASSIGNEDST0000",]$"Property.Location" <- "0000 0055 PAGE                ST0210"
total.clean[as.character(total.clean$"Property.Location") == "0000 0000 V                     0000",]$"Property.Location" <- "0000 0119 SEVENTH                ST0000"
total.clean[as.character(total.clean$"Property.Location") == "0000 0000 SITUS TO BE ASSIGNEDST0000",]$"Property.Location" <- "0000 1400 MISSION                ST1000"
total.clean[as.character(total.clean$"Property.Location") == "0000 0000 SITUS TO BE ASSIGNED  0000",]$"Property.Location" <- "0000 2121 THIRD                ST1000"
total.clean[as.character(total.clean$"Property.Location") == "0000 2559 SITUS TO BE ASSIGNEDAV0000",]$"Property.Location" <- "0000 1501 FILBERT                ST0000"
total.clean[as.character(total.clean$"Property.Location") == "0000 2395VSITUS TO BE ASSIGNEDST0000",]$"Property.Location" <- "0000 2395 LOMBARD                ST0000"
total.clean[as.character(total.clean$"Property.Location") == "0000 1461 SITUS TO BE ASSIGNEDST0000",]$"Property.Location" <- "0000 1461 PINE                ST0000"
total.clean[as.character(total.clean$"Parcel.Number") == "0785029",]$"Property.Location" <- "0000 0388 FULTON                ST0202"
total.clean[as.character(total.clean$"Parcel.Number") == "0808036",]$"Property.Location" <- "0401 0401 GROVE                0000"
total.clean[as.character(total.clean$"Parcel.Number") == "0808039",]$"Property.Location" <- "0450 0450 HAYES                ST0000"
total.clean[as.character(total.clean$"Parcel.Number") == "0831023",]$"Property.Location" <- "0000 0325 OCTAVIA                ST BNBAFBP3D"
total.clean[as.character(total.clean$"Parcel.Number") == "0857004",]$"Property.Location" <- "0000 0100 BUCHANAN ST                0000"
total.clean[as.character(total.clean$"Parcel.Number") == "3747320",]$"Property.Location" <- "0000 0399 FREMONT                ST2602"

# Can't figure out what's going on here
total.clean <- subset(total.clean, Parcel.Number!="0816067")

# Convert variable to factor
total.clean$simple415 <- as.factor(total.clean$simple415) 

write.csv(total.clean, "total_clean.csv")

### CONSTRUCT DISTANCE METRIC

# Construct new "unique" dataset with de-duplicated coordinates 
total.clean.unique <- total.clean[!duplicated(total.clean$LocationKey),]

# Add new empty columns for each unique entry of the total.clean dataset to the tax data
new_cols <- paste0("distance_", 1:length(total.clean.unique$LocationKey))
tax.data[new_cols] <- NA

# Make separate datasets for each year of tax data 
tax.data.split <- split(tax.data, tax.data$Closed.Roll.Year)
df_names <- paste0("tax.data.", 2007:2017)

for (i in 1:(length(tax.data.split))) {
  assign(df_names[i], tax.data.split[[i]])
}

# Populate distance columns in each of the tax year datasets

# Function to turn "distance_1" into just the number 1
distance_num <- function(x){
  as.numeric(substr(x, 10, nchar(x)))
}

# Helper function: takes in a row and the column number of interest (i.e. "distance_1" = 1)
# Outputs the distance from the lat/long coords in that row to the affordable housing entry we want
find_distances <- function (x,col) {
  if(total.clean.unique$Assessor.Neighborhood.Code[as.numeric(distance_num(i))] == x[38])
    distHaversine(c(as.numeric(x[45]), as.numeric(x[44])), c(total.clean.unique$Longitude[as.numeric(distance_num(i))],
                                                             total.clean.unique$Latitude[as.numeric(distance_num(i))]))
  else NA
}

# Main function: iterates through the columns and calls the helper function
for (i in new_cols) {
  tax.data.2016[i] <- apply(tax.data.2016, 1, find_distances, col=i)
}

# Write output to csv file
write.csv(tax.data.2016, "tax_data_2016.csv")

### AGGREGATE DISTANCE METRIC INTO SUMMARY STATISTICS

# Create new variable for asssesed+improved
tax.data.2007$Total.Assessed.Value <- tax.data.2007$Assessed.Land.Value + tax.data.2007$Assessed.Improvement.Value
tax.data.2008$Total.Assessed.Value <- tax.data.2008$Assessed.Land.Value + tax.data.2008$Assessed.Improvement.Value
tax.data.2009$Total.Assessed.Value <- tax.data.2009$Assessed.Land.Value + tax.data.2009$Assessed.Improvement.Value
tax.data.2010$Total.Assessed.Value <- tax.data.2010$Assessed.Land.Value + tax.data.2010$Assessed.Improvement.Value
tax.data.2011$Total.Assessed.Value <- tax.data.2011$Assessed.Land.Value + tax.data.2011$Assessed.Improvement.Value
tax.data.2012$Total.Assessed.Value <- tax.data.2012$Assessed.Land.Value + tax.data.2012$Assessed.Improvement.Value
tax.data.2013$Total.Assessed.Value <- tax.data.2013$Assessed.Land.Value + tax.data.2013$Assessed.Improvement.Value
tax.data.2014$Total.Assessed.Value <- tax.data.2014$Assessed.Land.Value + tax.data.2014$Assessed.Improvement.Value
tax.data.2015$Total.Assessed.Value <- tax.data.2015$Assessed.Land.Value + tax.data.2015$Assessed.Improvement.Value
tax.data.2016$Total.Assessed.Value <- tax.data.2016$Assessed.Land.Value + tax.data.2016$Assessed.Improvement.Value
tax.data.2017$Total.Assessed.Value <- tax.data.2017$Assessed.Land.Value + tax.data.2017$Assessed.Improvement.Value

# Create and populate "within x meters" variables
med_vals <- paste0("med_nearby_val_", rep(c(100, 200, 300),11), "m_", c(2007:2017))
total.clean.unique[med_vals] <- NA

# Create an index variable
for (i in 1:nrow(total.clean.unique)) {
  total.clean.unique$index[i] <- i 
}

# Helper function: takes in a name and extracts the #meters
extract_meters <- function(var_name) {
  as.numeric(substr(var_name, 16, 18))
}

# Helper function: takes in a name and extracts the year
extract_year <- function(var_name) {
  as.numeric(substr(var_name, 21, nchar(var_name)))
}

# Helper function: takes in a row and the column number of interest
# Outputs the calculated distance we want for the particular year
get_medians <- function(x, var_name) {
  row_num <- as.numeric(x[90])
  year <- extract_year(var_name)
  desired.df <- paste0("tax.data.", year)
  distance <- extract_meters(var_name)
  location <- x[1]
  desired.col <- new_cols[row_num]
  index_desired_col <- 46 + distance_num(desired.col)
  # 
  # #print(distance)
  # 
  #vector <- subset(tax.data.2016, !is.na(desired.col) & (desired.col < distance), select=LocationKey)
  vector <- tax.data.2016[ which( tax.data.2016[index_desired_col] < distance & 
                                     !is.na(tax.data.2016[index_desired_col])),]
  # print(vector)
  # 
  # d <- get(desired.df)
  # var_name <- median(merge(d[, c("LocationKey", "Total.Assessed.Value")], vector, by="LocationKey")$Total.Assessed.Value)
  # #print(var_name)
  #return(var_name)
}

# Main function for populating : iterates through the distance columns and calls the helper function
for (i in med_vals[1]) {
  total.clean.unique[i] <- apply(total.clean.unique, 1, get_medians, var_name = i)
}

### CLEAN FOR ZILLOW INPUT

# Filter out pre-2009 data
post.2009 <- total.clean[total.clean$completion_year > 2009,]

# Filter out non-complete properties
final.clean <- post.2009[post.2009$Project.Status=="(6) Complete",]

write.csv(final.clean, "final_clean.csv")


### VISUALIZATION AND ANALYSIS

# Generate counts of missing data
sapply(total.clean,function(x) sum(is.na(x)))

# Property values graph
plotmeans(Assessed.Improvement.Value ~ Closed.Roll.Year, main="Rising Property Values in San Francisco, 2007-Present",
          xlab = "Year", ylab="Assessed Land Value", data=tax.data)

# Trends in affordable units broken down by different characteristics

# Map of average home value by location, changes in home values by location

# Map of affordable developments

# Subset the properties that built vs paid fee for map
total.built <- subset(total.clean, simple415=="On-site BMR Project")
write.csv(total.built, "total_built.csv")

scatterplot(Assessed.Improvement.Value~Closed.Roll.Year|simple415, boxplots=TRUE, smooth=FALSE, reg.line=FALSE, data=total.clean)

by(total.clean$Closed.Roll.Year, total.clean$simple415, summary)

# Terrible preliminary model
model1 <- lm(log(Assessed.Improvement.Value+1) ~ Project.Units + Closed.Roll.Year, data=total.clean)
summary(model1)

# Diff in diff model



# Create dummy for "time when treatment started"


# Aggregate level model

# Verify parallel trends assumption

# Verify other diff-in-diff assumptions

