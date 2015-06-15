## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Question 1
aggEmi <- aggregate(Emissions ~ year,NEI, sum)

plot( aggEmi,pch=10, lwd=2, type="b",
      main="Total PM2.5 Emissions (All US Sources)" )

# From the above plot, we see a sharp decrease in total emissions of PM2.5 in United States between 1999 - 2008.
# As we can see from the plot, total emissions have decreased in the US from 1999 to 2008.

# Question 2
baltimoreNEI <- NEI[NEI$fips=="24510",]
aggEmiBaltimore <- aggregate(Emissions ~ year, baltimoreNEI,sum)
plot(
  aggEmiBaltimore,pch=7,
  lwd=2,
  type="b",
  main="Total PM2.5 Emissions (Baltimore City Sources)"
)
# Overall total emissions from PM2.5 have decreased in Baltimore City, Maryland from 1999 to 2008.
# From the above plot, we see an overall decrease in total emissions of PM2.5 in Baltimore 
# between 1999 to 2008, though there was an intermittent increase in emission between 2002 and 2005.

# Question 3
library(ggplot2)
library(plyr)
baltimore3 <- ddply(baltimoreNEI, .(type, year), summarize, Emissions = sum(Emissions))
ggplot(baltimore3,aes(year,Emissions,fill=type,color = type)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))


# Question 4
combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
combustion.coal.s <- SCC[combustion.coal,]

emissions <- NEI[(NEI$SCC %in% combustion.coal.s$SCC), ]

aggemissions.by.year <- aggregate(Emissions ~ year, data=emissions, FUN=sum)

# plot
library(ggplot2)

ggplot(aggemissions.by.year, aes(x=year, y=Emissions )) +
  geom_point(stat="identity") + geom_line(stat="identity")  +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions")) +
  ggtitle("Emissions from coal combustion-related sources")


# Question 5 
vehicles.s <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehicles.SCC <- SCC[vehicles.s,]$SCC
vehicles.NEI <- NEI[NEI$SCC %in% vehicles.SCC,]

baltimore.Vehicles.NEI <- vehicles.NEI[vehicles.NEI$fips==24510,]

aggbmore.emissions <- aggregate(Emissions ~ year, data=baltimore.Vehicles.NEI, FUN=sum)

library(ggplot2)

ggplot(aggbmore.emissions, aes(x=year, y=Emissions )) +
  geom_point(stat="identity") + geom_line(stat="identity")  + 
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions")) +
  ggtitle("Emissions from motor vehicle in Baltimore City")

# Question 6

vehicles.Baltimore.NEI <- vehicles.NEI[vehicles.NEI$fips == 24510,]
vehicles.Baltimore.NEI$city <- "Baltimore City"
vehicles.LA.NEI <- vehicles.NEI[vehicles.NEI$fips=="06037",]
vehicles.LA.NEI$city <- "Los Angeles County"
both.NEI <- rbind(vehicles.Baltimore.NEI,vehicles.LA.NEI)

both.NEI.agg <- ddply(both.NEI, .(city, year), summarize, Emissions = sum(Emissions))

ggplot(both.NEI.agg, aes(x=year, y=Emissions, fill=city, color = city)) +
  geom_point(stat="identity") + geom_line(stat="identity") +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))






