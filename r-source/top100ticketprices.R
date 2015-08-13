# install and load the rvest package for scraping web tables
install.packages("rvest")
library(rvest)

# read in csv files in the r-source folder
RestaurantTicketTop100 <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/RestaurantTicketTop100.csv")
cityfoodexpenditures <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/cityfoodexpenditures.csv")

# scape the wikipedia page for list of US cities by population
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
wikipopulation <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[4]') %>%
  html_table()
wikipopulation <- wikipopulation[[1]]

# rename the columns of the table we want to keep
names(wikipopulation)[1]<-"poprank"
names(wikipopulation)[2]<-"city"
names(wikipopulation)[3]<-"state2"
names(wikipopulation)[4]<-"popest2014"
names(wikipopulation)[7]<-"landarea2014"

# remove commas from estimated population column, and set value type as numeric
wikipopulation$popest2014 <- as.numeric(gsub(",","", wikipopulation$popest2014))

# extract the land area value we want to use from the land area column
wikipopulation$landarea2014 <- regmatches(wikipopulation$landarea2014,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",wikipopulation$landarea2014))
class(wikipopulation$landarea2014)
wikipopulation$landarea2014 <- as.numeric(sapply(wikipopulation$landarea2014, "[[", 2))

# remove footnote indicators from the city column
wikipopulation$city <- gsub("\\[.*?\\]", "", wikipopulation$city)

# remove unwanted columns
population <- subset(wikipopulation, select=c(poprank,city,state2,popest2014,landarea2014))

# calculate estimated 2014 population density
population$density2014 <- population$popest2014 / population$landarea2014

# add state abbreviation column for matching state to region
make.state.abbreviation <- function(x) {
  switch(x,
         "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR",
         "Arizona" = "AZ",  "California" = "CA",
         "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC",
         "Delaware" = "DE", "Florida" = "FL",
         "Georgia" =  "GA", "Hawai\'i" = "HI", "Iowa" = "IA",
         "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN",
         "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA",
         "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME",
         "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO",
         "Mississippi" = "MS", "Montana" = "MT",
         "North Carolina" = "NC", "North Dakota" = "ND",
         "Nebraska" = "NE", "New Hampshire" = "NH", "New Jersey" = "NJ",
         "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY",
         "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR",
         "Pennsylvania" = "PA",
         "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
         "Tennessee" = "TN", "Texas" = "TX",
         "Utah" = "UT",  "Virginia" = "VA", "Vermont" = "VT",
         "Washington" = "WA", "Wisconsin" = "WI",
         "West Virginia" = "WV", "Wyoming" = "WY", "")
}

population$state <- rep("", length = nrow(population))
for(index.for.state in seq(along = population$state2))
  population$state[index.for.state] <- make.state.abbreviation(population$state2[index.for.state])

# add region column to city population info
make.region <- function(x) {
  if (is.element(x, c('ME', 'NH', 'VT', 'MA', 'NY', 'CT', 'RI', 'NJ', 'PA'))) { 'northeast' }
  else if (is.element(x, c('DE', 'MD', 'DC', 'WV', 'VA', 'NC', 'SC', 'GA', 'FL', 'AL', 'MS', 'TN', 'KY', 'AR', 'LA', 'OK', 'TX'))) { 'south' }
  else if (is.element(x, c('OH', 'MI', 'IN', 'WI', 'IL', 'MN', 'IA', 'MO', 'KS', 'NE', 'SD', 'ND'))) { 'midwest' }
  else if (is.element(x, c('MT', 'WY', 'CO', 'NM', 'AZ', 'UT', 'ID', 'NV', 'CA', 'OR', 'WA'))) { 'west' }
  else  'pacific'
}

population$region <- rep("", length = nrow(population))
for(index.for.state in seq(along = population$state))
  population$region[index.for.state] <- make.region(population$state[index.for.state])

# merge population info into restaurant ticket price info
restaurantticketbypop <- merge(RestaurantTicketTop100, population, c("city","state","region"))

# combine all available data for 18 major cities
major18cities <- merge(restaurantticketbypop, cityfoodexpenditures, c("city","region"))

# add column for percent of total average expenditures is food away from home
major18cities$perfoodawaytotalexp <- major18cities$foodaway / major18cities$aveannual

# add column for percent of average pretax income is food away from home expenditures
major18cities$perfoodawaypretaxinc <- major18cities$foodaway / major18cities$pretaxincome

# add column for estimated number of dinners out per year (the %/% returns only the integer, essentially rounding down)
major18cities$dinnersoutperyear <- major18cities$foodaway %/% major18cities$totalaveticket

# add column for estimated dinners out per week (using the round function rounds up)
major18cities$dinnersoutperweek <- round(major18cities$dinnersoutperyear / 52, 0)

