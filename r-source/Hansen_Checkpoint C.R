restaurant.sales <- read.csv("Food_Drink_Sales.csv")
head(restaurant.sales)
library(ggplot2)
library(Cairo)

# increase in food and drink sales 1970 to 2015
food.drink.sales <- ggplot(data=restaurant.sales, aes(x=year, y=sales)) + 
    geom_bar(stat="identity", fill="cornflowerblue", colour="black") +
    ggtitle("Restaurant Industry Food and Drink Sales 1970 - 2015") + 
    xlab("Year") + ylab("Sales (in Billions of $)")
print(food.drink.sales)

CairoPNG(file = "food_drink_sales_1970-2015.png", width = 792, height = 612)
print(food.drink.sales)
dev.off()

# alternative view of same graph as above 
widths <- c(rep(7, 4), rep(0.6, 8))
food.drink.sales2 <- ggplot(data=restaurant.sales, aes(x=year, y=sales, width=widths)) + 
    geom_bar(stat="identity", fill="cornflowerblue", colour="black") +
    ggtitle("Restaurant Industry Food and Drink Sales 1970 - 2015") + 
    xlab("Year") + ylab("Sales (in Billions of $)")
print(food.drink.sales2)
# wide bars are slightly misleading
# another option would be to change the distance between the decades on the
# x axis, if we could do it in a way that wasn't misleading

CairoPNG(file = "food_drink_sales_1970-2015(2).png", width = 792, height = 612)
print(food.drink.sales2)
dev.off()

grocery.restaurant.sales <- read.csv("grocery_vs_restaurant_sales.csv")
grocery.restaurant.sales$Date <- as.Date(grocery.restaurant.sales$Period, format="%m/%d/%Y")
grocery.restaurant.sales$Value <- grocery.restaurant.sales$Value/1000
grocery.vs.restaurant <- ggplot(data=grocery.restaurant.sales, aes(x=Date, y=Value, color=Store)) + 
    geom_line(size=1) + theme(legend.position=c(0.8,0.25)) +
    ggtitle("Spending at Restaurants has Overtaken Spending at Grocery Stores") +
    xlab("Date") + ylab('American Spending (in Billions of $') 
print(grocery.vs.restaurant)

CairoPNG(file = "grocery_vs_restaurant_sales.png", width = 792, height = 612)
print(grocery.vs.restaurant)
dev.off()

# change in restaurant ind. share of food dollar 1955 and 2014
year <- c(1955, 2014)
share <- c(25, 47)
food.dollar.share <- data.frame(year, share)
head(food.dollar.share)
share1 <- ggplot(data=food.dollar.share, aes(x=factor(year), y=share)) + 
    geom_bar(stat="identity", fill="cornflowerblue", color="black") + 
    geom_text(aes(label=paste(format(share), '%')), vjust=1.5, color="white") + 
    ggtitle("Restaurant Industry's Share of the Food Dollar 1955 and 2014") + 
    xlab("Year") + ylab("Percentage")
print(share1)

CairoPNG(file = "food_dollar_share.png", width = 792, height = 612)
print(share1)
dev.off()

# alternative view of same share of food dollar graph
year <- c(1955, 2014, 1955, 2014)
share <- c(25, 47, 75, 53)
id <- c(1, 1, 2, 2)
food.dollar.share <- data.frame(year, share, factor(id))
head(food.dollar.share)
share2 <- ggplot(data=food.dollar.share, aes(x=factor(year), y=share)) + 
    geom_bar(aes(fill=factor(id)), stat="identity", color="black") + 
    geom_text(aes(label=paste(format(share), '%')), vjust=1.5, color="white") + 
    ggtitle("Restaurant Industry's Share of the Food Dollar 1955 and 2014") + 
    xlab("Year") + ylab("Percentage") + guides(fill=FALSE) +
    scale_fill_manual(values=c("cornflowerblue", "white"))
print(share2)

CairoPNG(file = "food_dollar_share(2).png", width = 792, height = 612)
print(share2)
dev.off()
