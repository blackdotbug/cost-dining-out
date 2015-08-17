rm(list=ls())
setwd("C:/Users/Nelson/Documents/Northwestern/PREDICT 455/Show/cost-dining-out")
EconExpenditures <- read.csv("EconomicExpendituresByCategory_Transformed.csv",header=TRUE)
library(plyr)
library(gcookbook)
library(ggplot2)
library(reshape2)
library(zoo)
library(scales)

Billions <- EconExpenditures[,1]
Months <- as.yearmon(EconExpenditures[,2])
Category <- EconExpenditures[,3]
MonthPercentage <- EconExpenditures[,4]

# EXPERIMENTAL CODE TO GET DATE LABELS TO WORK
#MonthLabel <- as.yearmon(Months)
#dateVec<-seq(from=as.Date("1992-01-01"), to=as.Date("2015-05-31"), by="months")
#Quarter <- as.yearqtr(Months)

#make_date <- function(x) {
#  year <- floor(x)
#  x <- year + (x - year)/0.4 - 0.125
#  as.Date(as.yearqtr(x))
#}

#format_quarters <- function(x) {
#  x <- as.yearqtr(x)
#  year <- as.integer(x)
#  quart <- as.integer(format(x, "%q"))
#
#  paste(c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart], 
#        year)
#}

# CHART FOR ABSOLUTE DOLLAR SALES

EconExpenditures$Category <- ordered( EconExpenditures$Category, levels = c('DiningIn', 'DiningOut', 'NonFood'))
ggplot(EconExpenditures, aes(x=as.Date(Months), y=Billions, fill=Category, order=(Category), position = 'stack')) +
	# add a area plot layer
	geom_area() +
	# change the colors and reserve the legend order (to match to stacking order)
	scale_fill_brewer(palette="Blues", breaks=levels(Category), labels=c("Eating at Home","Dining Out","Non-Food Sales")) +
	scale_y_continuous("Billions of $USD", limits = c(0,600)) +
	
#	scale_x_yearqtr("Year and Quarter", format = "%YQ%q")

	scale_x_date("Year and Month", breaks = "3 months", limits= c(as.Date("1992/1/1"), as.Date("2015/5/31")), labels = date_format("%m/%d/%Y")) +

#	scale_x_date("Year and Quarter", breaks = date_breaks("3 months"), limits=c(min(dateVec), max=max(dateVec)), labels = format_quarters) + 
	ggtitle("Retail Sales for Eating at Home, Dining In,and Non-Food Items Jan. 1992 - May 2015")

# CHART FOR PERCENTAGE OF MONTHLY SALES BY CATEGORY

EconExpenditures$Category <- ordered( EconExpenditures$Category, levels = c('DiningIn', 'DiningOut', 'NonFood'))
ggplot(EconExpenditures, aes(x=as.Date(Months), y=MonthPercentage, fill=Category, order=(Category), position = 'stack')) +
	# add a area plot layer
	geom_area() +
	# change the colors and reserve the legend order (to match to stacking order)
	scale_fill_brewer(palette="Blues", breaks=levels(Category), labels=c("Eating at Home","Dining Out","Non-Food Sales")) +
	scale_y_continuous("% of Total Retail Sales", limits = c(0,1)) +
	
#	scale_x_yearqtr("Year and Quarter", format = "%YQ%q")

	scale_x_date("Year and Month", breaks = "3 months", limits= c(as.Date("1992/1/1"), as.Date("2015/5/31")), labels = date_format("%m/%d/%Y")) +

#	scale_x_date("Year and Quarter", breaks = date_breaks("3 months"), limits=c(min(dateVec), max=max(dateVec)), labels = format_quarters) + 
	ggtitle("Proportion of Retail Spend on Eating at Home, Dining In,and Non-Food Items Jan. 1992 - May 2015")
