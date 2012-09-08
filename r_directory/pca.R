#What's the purpose of this file
#How to load stock data into R
#How to test the correlation of different stocks. 
#One line of code could help you get the correlations. Which is great. 
prices <- read.csv("/Users/apple/work_foot/ml4hackers/08-PCA/data/stock_prices.csv")
#Make date manipulation easier
library("lubridate")

#Format the date 
prices <- transform(prices, Date = ymd(Date))

#This will help us create matrix?
library("reshape")

#I guess it is to extract the data from old dataframe to construct a new matrix.
#Let's go ahead and see
#date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

#Remove illegal date.
#Shit, how could I know which row of data caused the error?
prices <- subset(prices, Date != ymd('2002-02-01'))

prices <- subset(prices, Stock != 'DDR')

#Use the value of Date and Stock as Column
#Use Close as the value of Date meet stock.
date.stock.matrix <- cast(prices, Date ~ Stock, value = "Close")

#What's the correlations among different column?
#I guess by default R treat the colmn as vectors
#Each stock is a vector, date is a dimension in the vector. 
#Calculate how much they are calculate with each other.
#Should we normalize it first, I mean the numeric value of the stock price?
cor.matrix <- cor(date.stock.matrix[,2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlation = correlations),
	aes(x = Correlation, fill = 1)) + geom_density() + opts(legend.position = 'none')
#This was to verify if vectors are strongly connected or what.

pca <- princomp(data.stock.matrix[,2:ncol(date.stock.matrix)])
#How to determine who contribute what to the total thing?
principal.component <- pca$loading[,1]
loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings), aes(x= Loading, fill = 1)) + geom_density() + opts(legend.position = "none")

#PCA can be used to predict value?
market.index <- predict(pca)[,1]
dji.prices <- read.csv("/Users/apple/work_foot/ml4hackers/08-PCA/data/DJI.csv")
dji.prices <- transform(dji.prices, Date = ymd(Date))

dji.prices <- subset(dji.prices, Date > ymd("2001-12-31"))
dji.prices <- subset(dji.prices, Date != ymd("2002-02-01"))
#It like transform(dji.prices, Close = rev(Close))?
#No it is not. It take the Close out and reverse it's order, that's all
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

comparison <- data.frame(Date = dates, MarketIndex = market.index, DJI = dji)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) + geom_point() +
	geom_smooth(method = 'lm')
	
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)
ggplot(comparison, aes(x = MarketIndex, y = DJI)) + geom_point() +
	geom_smooth(method = 'lm')

#How to compare our tracking performance on the same screen as DJI?
#The melt is to make one column into data, Why do we do this? Like
#  date marketindex dji
#  2008  192        193
#  2009  194        198
# to
#  date index price
#  2008 marketindex 192
#  2009 marketindex 194
#  2008 dji  193
#  2009 dji  198

comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, DJI = scale(DJI))
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')
#We can use color to differentiate the different index accordingly.
#Cool. 

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + geom_point + geom_line()