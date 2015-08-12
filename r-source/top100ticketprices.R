RestaurantTicketTop100 <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/RestaurantTicketTop100.csv")
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
wikipopulation <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[4]') %>%
  html_table()
wikipopulation <- wikipopulation[[1]]
names(wikipopulation)[1]<-"poprank"
names(wikipopulation)[3]<-"state"
names(wikipopulation)[4]<-"popest2014"
names(wikipopulation)[7]<-"landarea2014"
wikipopulation$popest2014 <- as.numeric(gsub(",","", wikipopulation$popest2014))
wikipopulation$landarea2014 <- regmatches(wikipopulation$landarea2014,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",wikipopulation$landarea2014))
class(wikipopulation$landarea2014)
wikipopulation$landarea2014 <- as.numeric(sapply(wikipopulation$landarea2014, "[[", 2))
wikipopulation$City <- gsub("\\[.*?\\]", "", wikipopulation$City)
population <- subset(wikipopulation, select=c(poprank,City,state,popest2014,landarea2014))
population$density2014 <- population$popest2014 / population$landarea2014
