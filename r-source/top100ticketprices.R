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

# add column for total food expenditures
major18cities$totalfoodexp <- major18cities$foodaway + major18cities$foodathome

# add column for total food and alcohol expenditures
major18cities$totalfoodalc <- major18cities$totalfoodexp + major18cities$alcohol

#add column for food away plus alcohol
major18cities$foodawayalcohol <- major18cities$foodaway + major18cities$alcohol

#add column for percent of average pretax income is food away plus alcohol
major18cities$foodalcperinc <- major18cities$foodawayalcohol / major18cities$pretaxincome

major18cities$city[major18cities$foodalcperinc == min(major18cities$foodalcperinc)]
major18cities$city[major18cities$foodalcperinc == max(major18cities$foodalcperinc)]

major18cities$city[major18cities$dinnersoutperyear == min(major18cities$dinnersoutperyear)]
major18cities$city[major18cities$dinnersoutperyear == max(major18cities$dinnersoutperyear)]

major18cities$city[major18cities$perfoodawaypretaxinc == min(major18cities$perfoodawaypretaxinc)]
major18cities$city[major18cities$perfoodawaypretaxinc == max(major18cities$perfoodawaypretaxinc)]

major18cities$city[major18cities$foodalcperinc == min(major18cities$foodalcperinc)]
major18cities$city[major18cities$foodalcperinc == max(major18cities$foodalcperinc)]



# load libraries to plot things
library("ggplot2")
library("scales")
library("grid")


# bubble plot comparing ticket price and population rank, showing population density
plot1 <- ggplot(major18cities, aes(major18cities$ticketrank, major18cities$poprank)) + geom_point(aes(colour=major18cities$region, fill=major18cities$region, size=major18cities$density2014), shape=21) + theme_bw() + scale_x_continuous(name="Average Restaurant Ticket Rank") + scale_y_continuous(name="Population Rank") + scale_size("Pop. Density\nppl/sq.mile", range=c(4,20), breaks=c(0,5000,10000,15000,20000,25000,30000)) + geom_text(aes(label=major18cities$city), size=4, hjust=-.15, vjust=-.15) + guides(colour = guide_legend(override.aes = list(shape = 15, size = 10))) + scale_fill_brewer("Region", type="qual", palette=3) + scale_colour_brewer("Region", type="qual", palette=3) + geom_text(aes(label=sprintf("$%s",major18cities$totalaveticket)), size=4, hjust=-.15, vjust=1.05) + theme(panel.margin=unit(7, "in"), panel.border=element_blank())

png(filename="plot1.png", bg="transparent", width=1100, height=500, units="px")
plot1
dev.off()

# bar plot of average spent on food away from home in 18 major cities, colored by region
plot2 <- ggplot(major18cities, aes(reorder(major18cities$city,major18cities$foodaway), major18cities$foodaway, fill=major18cities$region)) + geom_bar(stat="identity") + labs(x="", y="")+theme(legend.title=element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.background = element_blank(), panel.grid.major.y = element_line(colour="#9f9c9c"), panel.grid.minor.y = element_line(colour="#9f9c9c")) + scale_x_discrete(breaks=NULL) + scale_y_continuous(labels=dollar) + geom_text(aes(label=major18cities$city, angle=90, hjust=1.1)) + scale_fill_brewer(type="qual", palette=3)

png(filename="plot2.png", bg="transparent", width=900, height=500, units="px")
plot2
dev.off()

# bar plot of food expenditures in 18 major cities, stacked, colored by expenditure type
plot3 <- ggplot(major18cities) +
  geom_bar(aes(reorder(major18cities$city,major18cities$totalfoodalc), major18cities$totalfoodalc, fill="food\nat home"), stat="identity", position="stack") +
  geom_bar(aes(reorder(major18cities$city,major18cities$totalfoodalc), major18cities$foodawayalcohol, fill="food away\nfrom home"), stat="identity", position="dodge") +
  geom_bar(aes(reorder(major18cities$city,major18cities$totalfoodalc), major18cities$alcohol, fill="alcohol"), stat="identity", position="dodge") +
  labs(x="", y="") +
  theme(legend.title=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), panel.background=element_blank(), panel.grid.major.y=element_line(colour="#9f9c9c"), panel.grid.minor.y=element_line(colour="#9f9c9c")) +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(labels=dollar) +
  geom_text(aes(reorder(major18cities$city,major18cities$totalfoodalc), major18cities$totalfoodalc, label=major18cities$city, angle=90, hjust=1.1)) +
  scale_fill_brewer(type="qual", palette=4)

png(filename="plot3.png", bg="transparent", width=900, height=500, units="px")
plot3
dev.off()

# plot average income and restaurant ticket price
cols <- brewer_pal(type="seq", palette=3)
myPalette <- gradient_n_pal(cols)(seq(0, 1, length = 30))
ggplot(major18cities) + geom_point(aes(major18cities$totalaveticket, major18cities$pretaxincome, fill=major18cities$foodalcperinc, colour=major18cities$foodalcperinc, size=major18cities$density2014), alpha=0.75)+ scale_x_continuous(name="Average Total Restaurant Ticket", labels=dollar) + scale_y_continuous(name="Average Pretax Income", labels=dollar) + scale_size("Pop. Density\nppl/sq.mile", range=c(4,20), breaks=c(0,5000,10000,15000,20000,25000,30000)) + geom_text(aes(major18cities$totalaveticket, major18cities$pretaxincome, label=major18cities$city), size=4, vjust=1.95) + scale_fill_gradientn("Percent of income\non food & alcohol",colours = myPalette, labels=percent) + scale_colour_gradientn("Percent of income\non food & alcohol",colours = myPalette, labels=percent) + theme(panel.margin=unit(7, "in"), panel.border=element_blank()) + theme_bw()

