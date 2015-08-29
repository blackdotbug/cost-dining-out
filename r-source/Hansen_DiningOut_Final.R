library(ggplot2)
library(Cairo)

my_theme <-  theme(
    panel.background = element_rect(fill = "white", colour = NA),  
    axis.title=element_text(size=16, lineheight=.9, family="Times", face="bold"), 
    plot.title=element_text(size=20, family="Times", face="bold"),
    axis.text=element_text(size=12, family="Times")) 

restaurant.sales <- read.csv("Food_Drink_Sales.csv")
head(restaurant.sales)
# increase in food and drink sales 1970 to 2015
food.drink.sales <- ggplot(data=restaurant.sales, aes(x=year, y=sales)) + 
    geom_bar(stat="identity", fill="#377eb8", colour="black") +
    ggtitle("Restaurant Industry Food and Drink Sales 1970 - 2015") + 
    xlab("Year") + ylab("Sales (in Billions of $)") + my_theme 
print(food.drink.sales)

CairoPNG(file = "food_drink_sales_1970-2015.png", width = 792, height = 612)
print(food.drink.sales)
dev.off()

# alternative view of same graph as above 
widths <- c(rep(7, 4), rep(0.6, 8))
food.drink.sales2 <- ggplot(data=restaurant.sales, aes(x=year, y=sales, width=widths)) + 
    geom_bar(stat="identity", fill="cornflowerblue", colour="black") +
    ggtitle("Restaurant Industry Food and Drink Sales 1970 - 2015") + 
    xlab("Year") + ylab("Sales (in Billions of $)") + my_theme
print(food.drink.sales2)
# wide bars are slightly misleading
# another option would be to change the distance between the decades on the
# x axis, if we could do it in a way that wasn't misleading

CairoPNG(file = "food_drink_sales_1970-2015(2).png", width = 792, height = 612)
print(food.drink.sales2)
dev.off()

restaurant.sales <- read.csv("Food_Drink_Sales.csv")
head(restaurant.sales)
# increase in food and drink sales 1970 to 2015
food.drink.sales3 <- ggplot(data=restaurant.sales, aes(x=year, y=sales)) + 
    geom_line(colour="cornflowerblue", size=2) +
    ggtitle("Restaurant Industry Food and Drink Sales 1970 - 2015") + 
    xlab("Year") + ylab("Sales (in Billions of $)") + my_theme
print(food.drink.sales3)

CairoPNG(file = "food_drink_sales_1970-2015.png", width = 792, height = 612)
print(food.drink.sales3)
dev.off()

grocery.restaurant.sales <- read.csv("grocery_vs_restaurant_sales.csv")
grocery.restaurant.sales$Date <- as.Date(grocery.restaurant.sales$Period, format="%m/%d/%Y")
grocery.restaurant.sales$Value <- grocery.restaurant.sales$Value/1000
grocery.vs.restaurant <- ggplot(data=grocery.restaurant.sales, aes(x=Date, y=Value, color=Store)) + 
    geom_line(size=1) + 
    theme(legend.position=c(0.8,0.4), legend.background=element_rect(fill = "gray95", color="black")) +
    ggtitle("American Spending at Grocery Stores Versus Restaurants") +
    xlab("Year") + ylab("Total Dollars (Billions)") + labs(color="Store Type") +
    scale_colour_brewer(palette="Set1") + ylim(0, 55) + my_theme 
print(grocery.vs.restaurant)

CairoPNG(file = "grocery_vs_restaurant_sales2.png", width = 792, height = 612)
print(grocery.vs.restaurant)
dev.off()

# change in restaurant ind. share of food dollar 1955 and 2014
year <- c(1955, 2014)
share <- c(25, 47)
food.dollar.share <- data.frame(year, share)
head(food.dollar.share)
share1 <- ggplot(data=food.dollar.share, aes(x=factor(year), y=share)) + 
    geom_bar(stat="identity", fill="#377eb8", color="black") + 
    geom_text(aes(label=paste(format(share), '%')), vjust=-0.2, color="black") + 
    ggtitle("Restaurant Industry's Share of the Food Dollar 1955 and 2014") + 
    xlab("Year") + ylab("Percentage") + 
    theme(
        panel.background = element_rect(fill = "white", colour = NA),  
        axis.title=element_text(size=12, lineheight=.9, family="Times", face="bold"), 
        plot.title=element_text(size=16, family="Times", face="bold"),
        axis.text.x=element_text(size=12, family="Times", face="bold")) 
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
    ggtitle("Restaurant Industry's Share of the Food Dollar\nin 1955 and 2014") + 
    xlab("Year") + ylab("Percentage") + guides(fill=FALSE) +
    scale_fill_manual(values=c("#377eb8", "white")) + my_theme  
print(share2)

CairoPNG(file = "food_dollar_share(2).png", width = 792, height = 612)
print(share2)
dev.off()

expenditures <- read.csv("food_expenditures.csv")
str(expenditures)
consumption <- ggplot(data=expenditures, aes(x=food.consumption, y=percentage, fill=year)) +  
    geom_bar(stat="identity", position="dodge") + #scale_fill_manual(values=c("cornflowerblue", "red"))
    scale_fill_brewer(palette="Set1") + ylab("Percentage") + xlab("Place of Meal") + 
    ggtitle("Places Where Americans are Consuming Their Meals in 1978 and 2008") +
    theme(legend.position=c(0.8,0.4), legend.background=element_rect(fill = "gray95", color="black")) + 
    labs(fill="Time Period") + my_theme
print(consumption)

CairoPNG(file = "consumption.png", width = 792, height = 612)
print(consumption)
dev.off()

category.expenditures <- read.csv("EconomicExpendituresByCategory_Transformed.csv")
category.expenditures$Month <- as.Date(category.expenditures$Month, format="%m/%d/%Y")
category <- ggplot(data=category.expenditures, aes(x=Month, y=Billions, fill=Category)) + 
    geom_area() + my_theme + scale_fill_brewer(palette="Set1", labels=c("Dining In", "Dining Out", "Non-Food")) + 
    xlab("Year") + ylab("Spending (Billions $USD)") +
    theme(legend.position=c(0.2,0.8), legend.background=element_rect(fill = "gray95", color="black")) +
    ggtitle("Sales for Eating at Home, Dining Out, and Non-Food Purchases\n 1992 to 2015")
print(category)

CairoPNG(file = "category.png", width = 792, height = 612)
print(category)
dev.off()

proportion <- ggplot(data=category.expenditures, aes(x=Month, y=Percentage2, fill=Category)) + 
    geom_area() + my_theme + scale_fill_brewer(palette="Set1", labels=c("Dining In", "Dining Out", "Non-Food")) + 
    xlab("Year") + ylab("Percentage of Retail Dollars Spent") + 
    theme(legend.position=c(0.25,0.7), legend.background=element_rect(fill = "gray95", color="black")) +
    ggtitle("Proportion of Spending for Eating at Home, Dining Out, and Non-Food Purchases\n 1992 to 2015")
print(proportion)

CairoPNG(file = "proportion.png", width = 792, height = 612)
print(proportion)
dev.off()

obesity <- read.csv("obesity_R.csv")
obesity$Date <- as.Date(obesity$Date, format="%m/%d/%Y")
obesity.trend <- ggplot(data=obesity, aes(x=Date, y=Percentage, color=Type)) + 
    geom_line(size=1) + scale_color_brewer(palette="Set1") + my_theme + 
    theme(legend.position=c(0.3,0.75), legend.background=element_rect(fill = "gray95", color="black")) + 
    xlab("Year of Study") + ylab("Percentage of Population") + 
    ggtitle("Percentage of American Population that is Obese")

CairoPNG(file = "obesity3.png", width = 792, height = 612)
print(obesity.trend)
dev.off()

body.type <- read.csv("bodytype_calories.csv")
bodytype <- ggplot(data=body.type, aes(x=Body, y=Calories)) + 
    geom_bar(stat="identity", fill="#377eb8") + my_theme + 
    ggtitle("Number of Additional Calories Consumed Per Meal\nAway From Home Based on Body Type") + 
    xlab("Body Type") + ylab("Additional Calories Consumed")
print(bodytype)

CairoPNG(file = "body_type.png", width = 792, height = 612)
print(bodytype)
dev.off()

calories.per.meal <- read.csv("additional_calories.csv")
calories.per.meal$Meal <- factor(calories.per.meal$Meal, as.character(calories.per.meal$Meal))
calories <- ggplot(data=calories.per.meal, aes(x=Meal, y=Calories)) + 
    geom_bar(stat="identity", fill="#377eb8") + my_theme + 
    ggtitle("Number of Additional Calories Consumed Per Meal Away From Home") + 
    xlab("Meal") + ylab("Additional Calories Consumed")
print(calories)

CairoPNG(file = "additional_calories.png", width = 792, height = 612)
print(calories)
dev.off()


