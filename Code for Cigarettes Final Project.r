#DS0102 LS10 Final Project

#Loading libraries
library("ggplot2")
library("dplyr")
install.packages("Ecdat")
library(Ecdat)
head(Cigarette)

View(Ecdat::Cigarette)

#Create a boxplot of the average # of packs per capita by state.

ggplot(Cigarette, aes(x = state, y = packpc, fill = state)) + geom_boxplot() + ggtitle("Number of Cigarette Packs Per Capita by State")

#Question: What is the average #of packs per capita by state. 106

mean(Cigarette$packpc)

StateMean <- Cigarette %>% group_by(state) %>% summarise(Mean = mean(packpc)) %>% arrange((Mean))
View(StateMean)

#Question: Which states have the highest #of packs? Kentucky 174, New Hampshire 166, N Carolina 135
#Question: Which states have the lowest #of packs? Utah 57, New Mexico 74, Californa 77

#Find median over all states of the # of packs per capita for each year. #105.9096
median(Cigarette$packpc)

View(Cigarette)
StateMedian <- Cigarette %>% group_by(year) %>% summarise(Median = median(packpc))
View(StateMedian)

#Plot this median value for years 1985-1995. What can you say about cigarette usage in these years?

ggplot(StateMedian, aes(x = year, y = Median, col = year)) + geom_point() + ggtitle("Cigarette Use Drops Over Ten Years")

#Create scatter plot of price per pack vs #of packs per capita for all states and all years

ggplot(Cigarette, aes(x = avgprs, y = packpc)) + geom_point() + geom_smooth(method = lm)

#Is price and per capita correlated? Yes, negatively correlated.
cor.test(Cigarette$avgprs, Cigarette$packpc, method = "pearson", use = "complete.obs")

#p-value: 2.2e-16
#Correlation -0.5854443
#Add color to scatter plot for year. #Does the relationship change over time? Yes-price increases=usage lessens
ggplot(Cigarette, aes(x = avgprs, y = packpc, color = year)) + geom_point() + geom_smooth(method = lm) +ggtitle("Cigarette Number of Packs by Price and Year") + xlab("Average Price During Fiscal Year") + ylab("Number of Packs Per Capita")

# Do a linear regression
regression <- lm(packpc~avgprs, Cigarette)
summary(regression)

#How much variability does the line explain? Adjusted R-Squared: 34% of the variability.
#The Packs per capita # is significantly affected (34%) by the average price during a fiscal year.

#Adjust price of pack of cigarettes for inflation by dividing avgprs by cpi.
AdjustedPrice <- Cigarette %>% mutate(AdjustedCost = avgprs/cpi)

#Create an adjusted price for each row then redo scatter plot
ggplot(AdjustedPrice, aes(x = AdjustedCost, y = packpc, color = year)) + geom_point() + theme_dark() + geom_smooth(method = lm) +ggtitle("Cigarette Number of Packs by Price and Year") + xlab("Adjusted for Inflation Price") + ylab("Number of Packs Per Capita")

#Do linear regression using this adjusted price
adjustedRegression <- lm(packpc~AdjustedCost, AdjustedPrice)
summary(adjustedRegression)

#Create data frame with just rows from 1985
df1985 <- Cigarette %>% filter(year == 1985)
View(df1985)
#Create data fram with just rows from 1995
df1995 <- Cigarette %>% filter(year == 1995)
View(df1995)

#Use paired t-test to see if number of packpc in 1995 was significantly different than # of packpc in 1985
t.test(df1995$packpc, df1985$packpc, paired = TRUE)
#Yes, significantly different # of packs per capita in 1995 than 1985.


