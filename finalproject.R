#IDC 6700 Final Project
#Name: Jasmine Balsalobre

#data_sources
#plot 1: 
#insulin price - https://insulin.substack.com/p/humalog-insulin-investigating-the?s=r
#inflation - https://www.bls.gov/data/#prices

#plot 2
#eli lilly finances - https://insulin.substack.com/p/humalog-insulin-investigating-the?s=r

#plot 3
#insurance status - https://www.healthypeople.gov/2020/data-search/Search-the-Data?nid=4123

#plot 4:
#country comparison data - https://aspe.hhs.gov/sites/default/files/migrated_legacy_files/196281/Comparing-Insulin-Prices.pdf

#plot 5:
#OOP cost per year: https://healthcostinstitute.org/diabetes-and-insulin/price-of-insulin-prescription-doubled-between-2012-and-2016

# Libraries---------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(reshape)
library(cluster)
library(ggrepel)

# plot 1: price data ------------------------------------------------------

#humalog list price increase
price <- read.table('humalog.txt', sep = "\t", header = TRUE)
colnames(price) <- c("Date", "Price", "PercentInc")
price$Price = as.numeric(gsub("[$]", "", price$Price))
price$PercentInc = as.numeric(gsub("[%]", "", price$PercentInc))
price$Date = as.Date(price$Date, "%Y/%m/%d")

range = 1:length(price$Date)

#CPI data
cpi <- read.csv('CPI.csv',
               skip = 11)
inflation = data.frame(cpi$Year, cpi$Annual)
range2 = 1:length(cpi$Year)

for (i in range2) {
  inflation$percent[i] = (inflation$cpi.Annual[i]-inflation$cpi.Annual[1])/inflation$cpi.Annual[1]
}
inflation$cpi.Year <- as.Date(paste0(inflation$cpi.Year, "-01-01"))

ggplot() +
  geom_point(data = price, aes(x = Date, y = PercentInc, ),color = "steelblue2", size = 3) +
  geom_line(data = price, aes(x = Date, y=PercentInc, color = "steelblue2"), size = 1)+
  geom_line(data = inflation, aes(x = cpi.Year, y = percent*100, color = "black"), size = 0.7)+
  geom_point(data = inflation, aes(x = cpi.Year, y = percent*100), size = 2)+
  ggtitle("Increase in Humalog Insulin List Price per Prescription")+
  xlab("Year")+ ylab("Percent Increase")+
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  annotate("text", x=as.Date("2016-07-05"), y=1203, hjust = 1.5,
           label = "$276.62", color = "steelblue", size = 5)+
  annotate("segment", x = as.Date("2015-05-05"), xend = as.Date("2016-05-05"), 
           y = 1203, yend = 1203, colour = "steelblue") +
  annotate("text", x=as.Date("1996-07-21"), y=0, hjust = 0.5, vjust = -2,
         label = "$21.23", color = "steelblue", size = 5)+
  annotate("segment", x = as.Date("1996-07-21"), xend = as.Date("1996-07-21"), 
           y = 85, yend = 15, colour = "steelblue") +
  annotate("text", x=as.Date("2016-06-01"), y=56, hjust = 0.3, vjust = -2,
           label = "$32.48", color = "black", size = 5)+
  annotate("segment", x = as.Date("2017-01-01"), xend = as.Date("2017-01-01"), 
           y = 130, yend = 60, colour = "black") +
  scale_color_identity(guide = "legend",
                       breaks=c('steelblue2', 'black'),
                       labels=c('Insulin List Price', 'Inflation'))+
  theme(text=element_text(size=15, family="serif"), legend.position = c(0.2,0.85), 
        legend.title = element_blank(),
        legend.text = element_text(family="sans", size=10),
        legend.background = element_rect(fill="grey95", size=0.7, 
                                         linetype="solid", colour ="grey69"))
        
  

# plot 2: eli lilly finances ----------------------------------------------

eldf <- read.table('eli_lilly.txt', sep = '\t', header = TRUE)
killy <- eldf[, c(2,3,5,7)]
killy[] <- lapply(killy, gsub, pattern=',', replacement='')
killy <- mutate_all(killy, function(x) as.numeric(as.character(x)))
colnames(killy) <- c("year", "revenue", "RD", "COS")

ggplot(killy)+
  geom_point(aes(x = year, y = revenue), color = "palegreen3", size = 2)+
  geom_line(aes(x = year, y = revenue, color = "palegreen3"), size = 1)+
  geom_point(aes(x = year, y = RD), color = "hotpink2", size = 2)+
  geom_line(aes(x = year, y = RD, color = "hotpink2"), size = 1)+
  geom_point(aes(x = year, y = COS), color = "steelblue2", size = 2)+
  geom_line(aes(x = year, y = COS, color = "steelblue2"), size = 1)+
  ylab("Amount in USD")+xlab("Year")+
  ggtitle("Eli Lilly Financial Data")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels=function(x) paste0("$",x)) +
  
  scale_color_identity(guide = "legend",
                       breaks=c('palegreen3', 'hotpink2', 'steelblue2'),
                       labels=c('Revenue', "R&D", "Cost of Sales"))+

  theme(text=element_text(size=15, family="serif"), legend.position = c(0.85,0.5), 
        legend.title = element_blank(),
        legend.text = element_text(family="sans", size=10),
        legend.background = element_rect(fill="grey95", size=0.7, 
                                         linetype="solid", colour ="grey69"))
  

# plot 3: a1c vs uninsured status ------------------------------------------------------------------

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
  labs(fill='Insurance Status') +
  ylab("Percentage of Diabetics")+xlab("Years")+
  scale_y_continuous(labels=function(x) paste0(x,"%"))+
  scale_fill_manual(values=c("#FF6666", "#9999CC"))+
  theme(text=element_text(size=14, family="serif"),
        legend.title = element_text(family = "sans", size = 12),
        legend.text = element_text(family="sans", size=10))+
  scale_x_discrete(labels=c("2005-8" = "2005-2008", "2009-12" = "2009-2012",
                            "2013-16" = "2013-2016"))


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
  ylab("Price per Standard Quantity (USD)")+
  scale_y_continuous(labels=function(x) paste0("$",x)) +
  theme_bw()+
  theme(text=element_text(size=15, family="serif"), legend.position = "none")
  
# plot 5: oop cost of insulin------------------------------------------------------------------

oop <- read.csv('POS_prices.csv')
ave_cost = data.frame(aggregate(oop[3], list(oop$year), FUN=mean) )

yearly <- summarize(group_by(oop, year),
                    cost = mean(Average_Point.of.Sale_Price_per_Script))
yearly <- trunc(yearly)

ggplot(yearly, aes(x = year, y = cost))+
  geom_point(shape = 21, fill = "steelblue2", color = "black", size = 15)+
  geom_line(color = "steelblue2", size = 2.5)+
  geom_text(aes(label=paste0("$", cost)), vjust = 0.12)+
  ylab("Cost (USD)")+xlab("Year")+
  ggtitle("Average Insulin Point of Sale Price per Month")+
  scale_y_continuous(labels=function(x) paste0("$",x)) +
  theme_minimal()+
  theme(text=element_text(size=15, family="serif"))

