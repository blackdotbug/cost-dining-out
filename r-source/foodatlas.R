# load libraries
library("maps")
library("mapproj")
library("MASS")
library("RColorBrewer")

# load data sources
FoodEnvironmentAtlas_local <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/FoodEnvironmentAtlas_local.csv")
FoodEnvironmentAtlas_restaurants <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/FoodEnvironmentAtlas_restaurants.csv")
FoodEnvironmentAtlas_health <- read.csv("~/Sites/cost-dining-out/cost-dining-out/r-source/FoodEnvironmentAtlas_health.csv")

# load fips codes
data(county.fips)

# isolate column to map and matching fips code
farmersmarkets <- subset(FoodEnvironmentAtlas_local, select=c(FIPS, FMRKTPTH13))

# assess boundaries and breaks for scale
markets <- farmersmarkets$FMRKTPTH13
markets.freq = table(markets)
#markets.freq
#max(farmersmarkets$FMRKTPTH13)
#min(farmersmarkets$FMRKTPTH13)

# load a color palette
colors1 = brewer.pal(8,"Greens")

# match data breaks to scale breaks
farmersmarkets$colorBuckets <- as.numeric(cut(farmersmarkets$FMRKTPTH13, c(0, .1, .25, .5, .75, 1, 1.25, 1.5), right=FALSE))
colorsmatched1 <- farmersmarkets$colorBuckets[match(county.fips$fips, farmersmarkets$FIPS)]

# print map to file
png(filename="map1.png", bg="transparent", width=900, height=500, units="px")

# bottom map layour has county data
map("county", col = colors1[colorsmatched1], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")

# next layer adds state boundaries
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.3, projection = "polyconic")

# define legend
leg.txt1 <- c("<0.9%", "1-24%", "25-49%", "50-74%", "75-99%", "100-124%", "125-149%", ">150%")
legend("bottomleft", leg.txt1, horiz = FALSE, fill = colors1, bty = "n")

# add title
title("Farmers' markets/1,000 pop, 2013")

# close device for printing next map
dev.off()

# build fast food map
fastfood <- subset(FoodEnvironmentAtlas_restaurants, select=c(FIPS, FFRPTH12))
ff <- fastfood$FFRPTH12
ff.freq = table(ff)
#ff.freq
colors2 = brewer.pal(8, "Oranges")
fastfood$colorBuckets <- as.numeric(cut(fastfood$FFRPTH12, c(0, .25, .5, .75, 1, 1.25, 1.5, 6), right=FALSE))
colorsmatched2 <- fastfood$colorBuckets[match(county.fips$fips, fastfood$FIPS)]
png(filename="map2.png", bg="transparent", width=900, height=500, units="px")
map("county", col=colors2[colorsmatched2], fill=TRUE, resolution=0, lty=0, projection="polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.3, projection = "polyconic")
leg.txt2 <- c("<0.9%", "1-24%", "25-49%", "50-74%", "75-99%", "100-124%", "125-149%", ">150%")
legend("bottomleft", leg.txt2, horiz = FALSE, fill = colors2, bty = "n")
title("Fast-food restaurants/1,000 pop, 2012")
dev.off()

# build full service restaurant map
fullservice <- subset(FoodEnvironmentAtlas_restaurants, select=c(FIPS, FSRPTH12))
fs <- fullservice$FSRPTH12
fs.freq = table(fs)
#fs.freq
colors3 = brewer.pal(8, "Purples")
fullservice$colorBuckets <- as.numeric(cut(fullservice$FSRPTH12, c(0, .25, .5, .75, 1, 1.25, 1.5, 15), right=FALSE))
colorsmatched3 <- fullservice$colorBuckets[match(county.fips$fips, fullservice$FIPS)]
png(filename="map3.png", bg="transparent", width=900, height=500, units="px")
map("county", col=colors3[colorsmatched3], fill=TRUE, resolution=0, lty=0, projection="polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.3, projection = "polyconic")
leg.txt3 <- c("<1%", "1-24%", "25-49%", "50-74%", "75-99%", "100-124%", "125-149%", ">150%")
legend("bottomleft", leg.txt3, horiz = FALSE, fill = colors3, bty = "n")
title("Full-service restaurants/1,000 pop, 2012")
dev.off()

# build adult obesity map
obesity <- subset(FoodEnvironmentAtlas_health, select=c(FIPS, PCT_OBESE_ADULTS10))
ob <- obesity$PCT_OBESE_ADULTS10
ob.freq = table(ob)
#ob.freq
colors4 = brewer.pal(8, "Blues")
obesity$colorBuckets <- as.numeric(cut(obesity$PCT_OBESE_ADULTS10, c(13, 20, 25, 30, 35, 40, 45, 50), right=FALSE))
colorsmatched4 <- obesity$colorBuckets[match(county.fips$fips, obesity$FIPS)]
png(filename="map4.png", bg="transparent", width=900, height=500, units="px")
map("county", col=colors4[colorsmatched4], fill=TRUE, resolution=0, lty=0, projection="polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.3, projection = "polyconic")
leg.txt4 <- c("<14%", "15-20%", "21-25%", "26-30%", "31-35%", "36-40%", "41-45%", ">46%")
legend("bottomleft", leg.txt4, horiz = FALSE, fill = colors4, bty = "n")
title("Adult obesity rate (county), 2010")
dev.off()
