#IDC 6700 Final Project
#Name: Jasmine Balsalobre

#data_sources
#plot 1: 
#insulin price - https://insulin.substack.com/p/the-price-of-insulin-vs-the-price?s=r
#inflation - https://www.bls.gov/data/#prices

#https://healthcostinstitute.org/diabetes-and-insulin/price-of-insulin-prescription-doubled-between-2012-and-2016

#plot 2:
#insurance status - https://www.healthypeople.gov/2020/data-search/Search-the-Data?nid=4123

#plot 4:
#country comparison data - https://aspe.hhs.gov/sites/default/files/migrated_legacy_files/196281/Comparing-Insulin-Prices.pdf
# Libraries ---------------------------------------------------------------
library(plyr,quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(dplyr)
library(reshape2)
library(cluster)

# plot 1: price data ------------------------------------------------------

cost = read.csv('POS_prices.csv') 
ave_cost = data.frame(aggregate(cost[3], list(cost$year), FUN=mean) )

price = read.csv('substack_pricedata.csv')
colnames(price)[1] = "year"
range = 1:length(price$year)

#find percent increase
for (i in range) {
  price$percent[i] = (price$price[i]-price$price[1])/price$price[1]
}

#CPI data
cpi = read.csv('CPI.csv',
               skip = 11)
inflation = data.frame(cpi$Year, cpi$Annual)
range2 = 1:length(cpi$Year)

for (i in range2) {
  inflation$percent[i] = (inflation$cpi.Annual[i]-inflation$cpi.Annual[1])/inflation$cpi.Annual[1]
}

ggplot() +
  geom_line(data = price, aes(x = year, y = percent), color = "orange", size = 1) +
  geom_line(data = inflation, aes(x = cpi.Year, y = percent))+
  ggtitle("Increase in List Price for Insulin")


# plot 2 ------------------------------------------------------------------

#population with a1c over 9% by insurance status
df <- read.table('ins_status.txt',
                 sep = "\t", header = FALSE
                )

colnames(df) = c("status", "country", "2005-8", "StdErr1",	"LowerCI",	"UpperCI", "2009-12", 
                   "StdErr2",	"LowerCI",	"UpperCI", "2013-16", "StdErr3",	"LowerCI",	"UpperCI")

a1c9 = df[c(1,2), c(1, 3, 7, 11)]
a1c9 = melt(a1c9)
a1c9$upperCI = melt(df[c(1,2), c(6, 10, 14)])
a1c9$lowerCI = melt(df[c(1,2), c(5, 9, 13)])
a1c9 = a1c9[-c(4,6)]

ggplot(a1c9, aes(x = variable, y = value, fill = status))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Insurance Status for Diabetics with HbA1c >9%")

#maybe add facet with low a1c


# plot 3 ------------------------------------------------------------------

#population with a1c over 9% by race


# plot 4 ------------------------------------------------------------------

#comparison to other countries

countrydf <- read.table("pricebycountry.txt",
                        sep = "",
                        header = FALSE
                        )

colnames(countrydf) = countrydf[1,]
countrydf = countrydf[-1,]
countrydf[] <- lapply(countrydf, gsub, pattern='[$]', replacement='')
countrydf$Analog <- as.numeric(countrydf$Analog)

countrydf = reorder(countrydf$Country, countrydf$Analog)

ordered <- countrydf[order(countrydf$Analog, decreasing = TRUE),]

top10 =  head(ordered, 10)

ggplot(top10, aes(x = Country, y = Analog, fill = Country)) +
  geom_bar(stat = "identity")+
  coord_flip()


# plot 5 ------------------------------------------------------------------

#oop costs for insulin