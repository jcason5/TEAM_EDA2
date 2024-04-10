#import Tidyverse for data analysis
library(tidyverse)
#Loading in the data set
watchdata=read_csv("ebay_watches.csv", show_col_types=FALSE)

#basic Exploring of the data set
#view(watchdata) #overlook of the data
tail(watchdata) #Last 6 entries in the data 
length(watchdata) # number of columns 
str(watchdata) # Checking data type for each of the columns
#Changing price to a numerical value
watchdata$priceWithCurrency <- as.integer(watchdata$priceWithCurrency)
str(watchdata$priceWithCurrency) #verifying change 
summary(watchdata)
names(watchdata) # Names of each of the columns 


#Missing data imputation
paste("Number of missing values: ", sum(is.na(data)))
# Number of missing entries for each column of data
colSums(is.na(watchdata))
#Filling in all of the missing data slots
sold_median=median(watchdata$sold, na.rm=TRUE) #sold imputations
paste(sold_median)
watchdata$sold=ifelse(is.na(watchdata$sold),
                   sold_median,
                   watchdata$sold)
sum(is.na(watchdata$sold))#checking to see if it worked
price_median=median(watchdata$priceWithCurrency, na.rm=TRUE)#Price imputations
paste(price_median)
watchdata$priceWithCurrency=ifelse(is.na(watchdata$priceWithCurrency),
                                   price_median,
                                   watchdata$priceWithCurrency)
watchdata$availableText=ifelse(is.na(watchdata$availableText),#Available text imputation
                   "unknown",
                   watchdata$availableText)
sum(is.na(watchdata$availableText))#checking to see if it worked
watchdata$itemLocation=ifelse(is.na(watchdata$itemLocation),#itemLocation imputation
                               "unknown",
                               watchdata$itemLocation)
watchdata$lastUpdated=ifelse(is.na(watchdata$lastUpdated),#lastUpdated imputation
                               "unknown",
                               watchdata$lastUpdated)
watchdata$subTitle=ifelse(is.na(watchdata$subTitle),#subTitle imputation
                               "unknown",
                               watchdata$subTitle)
watchdata$type=ifelse(is.na(watchdata$type),#type imputation
                               "unknown",
                               watchdata$type)
colSums(is.na(watchdata))#showing that data is all imputed

#More basic data description

#Price descriptive data
paste("minimum height", min(watchdata$priceWithCurrency))
paste("maximum height", max(watchdata$priceWithCurrency))
paste("Range", range(watchdata$priceWithCurrency))
paste("IQR", IQR(watchdata$priceWithCurrency))
paste("Variance", var(watchdata$priceWithCurrency))
paste("SD", sd(watchdata$priceWithCurrency))
summary(watchdata$priceWithCurrency)
summary(watchdata$priceWithCurrency)

#Sold descriptive data
paste("minimum height", min(watchdata$sold))
paste("maximum height", max(watchdata$sold))
paste("Range", range(watchdata$sold))
paste("IQR", IQR(watchdata$sold))
paste("Variance", var(watchdata$sold))
paste("SD", sd(watchdata$sold))
summary(watchdata$sold)
summary(watchdata$sold)

#Summary states table for easy export
library(psych)
describe(select_if(watchdata, is.numeric))
summary_watchdata=describe(select_if(watchdata, is.numeric))
#view(summary_watchdata)
write.csv(summary_watchdata, "summary_watchdata.csv")



#Creating a pie chart for types of watches

#checking to see for the unique types of watches being produced
unique(watchdata$type)

#pie chart

watchtype_count <- watchdata %>% #setting up percentages
  count(type) %>% 
  mutate(percent=n/nrow(watchdata),
         percent_label=paste0(round(percent*100),"%"))
png("Piechart.png")
watchtype_count %>% #the pie chart and settings
  ggplot(aes(x='',y=percent,
             fill=type,
             label = percent_label,
             position ='dodge')) +
  geom_col(color="black") +
  geom_text(color ='black',
            position = position_stack(vjust = 0.5),vjust=0.5)+
  coord_polar(theta='y', start=0) +
  theme_void() + 
  guides(fill = guide_legend(reverse = TRUE))+
  labs(fill='Type of Watch',title='Pie Chart of Types of Watches on Ebay Market')+
  theme(plot.title=element_text(hjust=0.5,size=14,face="bold"))
dev.off()
# Findings from the pie chart on what kinds of watches are sold
# 83% of watches are wristwatches
# 14% are just the watches
# less than 0.1% are womens and mens watches only
# 0.2% are value watches
# findings - value watches and gendered watches seem to have no presence in market
# making a wristwatch is most likely to be purchased second to a  watch



#raw correlation data
corr<-cor(watchdata[sapply(watchdata, is.numeric)])
corr

#Heat map
png("Headmap.png")
heatmap(corr, Colv = NA, Rowv = NA, scale="column", 
        main="Heatmap of Ebay Watch Data")
dev.off()
#findings - low correlations between variables 


#histogram
png('Histogram.png')
watchdata %>% 
ggplot(aes(x=priceWithCurrency))+
  geom_histogram(binwidth = 100,
                 fill = "darkgreen",
                 color='lightslateblue')+
  xlim(249,8000)+
  labs(title="Histrogram of Price of Watches",x='Price($)')+
  theme(plot.title=element_text(hjust=0.5,size=14,face="bold"))
dev.off()
#findings - The data is skewed right and there seems to be the most watches sold 
# in the 500 - 1000 dollar range


#Bar chat of top 5 companies by revenue

#creating data frame for calculation of total revenues
companyrev <- data.frame(company=watchdata$seller,
                         watchcost=watchdata$priceWithCurrency,
                         numsold=watchdata$sold)
companyrev$totalrev <- watchdata$priceWithCurrency * watchdata$sold#total rev column
head(companyrev)#checking work

#creating table so I can graph by revenue
top5<- companyrev %>% 
  group_by(company) %>% 
  summarise(
   total_value = sum(totalrev)
  )
#making it so the matrix is only a 5X2 
top5$total_value <- sort(top5$total_value, decreasing=TRUE)
top5 <- head(top5)
top5
#creating the bar graph
png("Barchart of Top 5 companies by Revenue.png")
top5 %>% 
  ggplot(aes(x=company,y=total_value))+
  geom_col(fill='darkred',
           color='black')+
  labs(y='Total Revenue (in Millions)',title='Top 5 Companies by Revenue')+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(size=6),
        plot.title=element_text(hjust=0.5,size=14,face="bold"))
dev.off()

# top 5 companies by revenue is as follows BRAND OFF TOKYO Hong Kong
# Breguetcamera, Citiwide Store, Direct Luxury, Direct Source G&D,
# and Sigmatime

#Scatter of price of watch sold by Brand Off Tokyo
#names(watchdata)
png('Scatter Plot of Top5.png')
watchdata %>% 
  filter(seller %in% c("BRAND OFF TOKYO Hong Kong",'Breguetcamera','Citiwide Store',
                       'Direct Luxury','Direct Source G&D','Sigmatime')) %>% 
  ggplot(aes(x=sold,y=priceWithCurrency,
             color=seller))+geom_point()+
  labs(title='Scatter Plot of top 5 Sales and items sold',x='Number of Watches Sold',
       y='Price of Watches')+
  theme(plot.title=element_text(hjust=0.5,size=14,face="bold"),
        )+
  xlim(0,40)+ ylim(0,200000)
dev.off()
#it seems that selling watches for a really high price is
#what is going to make you the most amount of money 



