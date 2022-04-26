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

#plot 5:
#OOP cost per year: https://healthcostinstitute.org/diabetes-and-insulin/price-of-insulin-prescription-doubled-between-2012-and-2016

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


# plot 2: a1c vs uninsured status ------------------------------------------------------------------

#value = percentage of people with diabetes

dfload <- read.csv('D-5.1-data.csv')
df <- data.frame(dfload[42:43,])

collabel = c("status", "country", "2005-8", "StdErr",	"LowerCI",	"UpperCI", "2009-12", 
                   "StdErr",	"LowerCI",	"UpperCI", "2013-16", "StdErr",	"LowerCI",	"UpperCI")

colnames(df) = collabel


df2load <- read.csv('D-5.2-data.csv')
df2 <- df2load[c(42,44),]
colnames(df2) = collabel

#insurance status a1c under 7
a1c7 <- df2[,c(1, 3, 7, 11)]
a1c7$a1c = c("<7", "<7")

#insurance status a1c above 9
a1c9 = df[,c(1, 3, 7, 11)]
a1c9$a1c = c(">9", ">9")

a1c <- rbind(a1c9, a1c7)

a1c = melt(a1c, id.vars = c('status', 'a1c') )
a1c$value = as.numeric(a1c$value)

a1c_names = list("<7" = "HbA1c under 7.0%", ">9" = "HbA1c above 9.0%")
a1clabeller <- function(variable,value){
  return(a1c_names[value])
}

ggplot(a1c, aes(x = variable, y = value, fill = status))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(a1c ~ ., labeller = a1clabeller) +
  ggtitle("Insurance Status by HbA1c")



# plot 3: a1c vs race ------------------------------------------------------------------

#a1c above 9
df_race9 <- dfload[c(8,11,12),]

collabel2 <- c("race", "country", "2005-8", "StdErr",	"LowerCI",	"UpperCI", "2009-12", 
             "StdErr",	"LowerCI",	"UpperCI", "2013-16", "StdErr",	"LowerCI",	"UpperCI")

colnames(df_race9) <- collabel2

r_a1c9 = df_race9[,c(1, 3, 7, 11)]
r_a1c9$a1c = c(">9", ">9", ">9")

#a1c below 7
df_race7 <- df2load[c(8,11,12),]
colnames(df_race7) <- collabel2

r_a1c7 = df_race9[,c(1, 3, 7, 11)]
r_a1c7$a1c = c("<7", "<7", "<7")

#bind and melt data
r_a1c <- rbind(r_a1c9, r_a1c7)

r_a1c = melt(r_a1c, id.vars = c('race', 'a1c') )
r_a1c$value = as.numeric(r_a1c$value)

a1c_names = list("<7" = "HbA1c under 7.0%", ">9" = "HbA1c above 9.0%")
a1clabeller <- function(variable,value){
  return(a1c_names[value])
}

ggplot(r_a1c, aes(x = variable, y = value, fill = race))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_grid(a1c ~ ., labeller = a1clabeller) +
  ggtitle("Race vs HbA1c")

# plot 4: countries with highest insulin prices ------------------------------------------------------------------

#comparison to other countries

countrydf <- read.table("pricebycountry.txt",
                        sep = "",
                        header = FALSE
                        )

colnames(countrydf) = countrydf[1,]
countrydf = countrydf[-1,]
countrydf[] <- lapply(countrydf, gsub, pattern='[$]', replacement='')
countrydf$Analog <- as.numeric(countrydf$Analog)


ordered <- countrydf[order(countrydf$Analog, decreasing = TRUE),]

top10 =  head(ordered, 10)
top10$Country = reorder(top10$Country, top10$Analog)


ggplot(top10, aes(x = Country, y = Analog, fill = Country))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=paste0("$", Analog)), position=position_dodge(width=0.9), hjust=0)+
  scale_fill_brewer(palette="Set3")+
  coord_flip()+
  ggtitle("Countries with Highest Analog Insulin Price")+
  ylab("Price per Standard Unit (USD)")+
  theme_bw()+
  theme(text=element_text(size=15, family="sans"), legend.position = "none")
        
        
        #plot.background = element_rect(fill = "pink2"))
  


#add dollar labels, remove legend, add titles

# plot 5: oop cost of insulin------------------------------------------------------------------

oop <- read.csv('POS_prices.csv')

yearly <- summarize(group_by(oop, year),
                    cost = mean(Average_Point.of.Sale_Price_per_Script))
yearly <- trunc(yearly)

ggplot(yearly, aes(x = year, y = cost))+
  geom_point(shape = 21)+
  geom_line()+
  geom_text(aes(label=paste0("$", cost)), vjust = -1)
