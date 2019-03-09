library(tigris)

print('')
print('')

tax.data <- read.csv("../Assessor_Historical_Secured_Property_Tax_Rolls.csv")

affordable <- read.csv("../Residential_Projects_With_Inclusionary_Requirements.csv")

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
                         "Land Dedication ",
                         "Off-site BMR Project",
                         "Off-site BMR Project/Fee Payment",
                         "Units for Off-site Project Units for Off-site Project with On-site Obligation")] <- "Other"

# On-site BMR project variable was messy and had space at end
affordable$simple415[affordable$simple415 %in% c("On-site BMR Project ", "On-site BMR Project/Fee Payment", "On-site BMR Project/Land Dedication")] <- "On-site BMR Project"
affordable$simple415 <- as.factor(affordable$simple415)

