# Clean environment
rm(list= ls())

getwd()
# setwd("your/path/to/data")

# Open the data 
data <- read.csv(file = "marketing_mix_assignment.csv",header = TRUE)
str(data)

# Retrieve variables
tiktok<-data$TikTok
facebook<-data$Facebook 
google_ads<-data$Google.Ads
sales<-data$Sales
date <- data$Date

summary(data)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
summary(data$Date)

# Now we make the data time series data in R
# Frequency=52 for weekly time series, starts in year 2018 in week 1 of the year


tiktok <- ts(tiktok,frequency = 52, start=c(2018,1))
facebook <- ts(facebook,frequency = 52, start=c(2018,1))
google_ads <- ts(google_ads,frequency = 52, start=c(2018,1))
sales <- ts(sales,frequency = 52, start=c(2018,1))


online_total<-tiktok + facebook + google_ads


### Let's get a feel of data
plot(sales, col="blue", main="Sales")
plot(tiktok, col="darkgreen", main="TikTok")
plot(facebook, col="red", main="Facebook")
plot(google_ads, col="purple", main="Google Ads")

# Is there a trend in data? Co-variance of series? Is there seasonality? 


sum_tiktok<-sum(tiktok)
sum_facebook<-sum(facebook)
sum_google_ads<-sum(google_ads)

total_online<-sum(google_ads) + sum(facebook) + sum(tiktok)

tiktok_share<-sum_tiktok/total_online
facebook_share<-sum_facebook/total_online
google_ads_share<-sum_google_ads/total_online

### Pie-Chart for Media Spending Share 
slices<-c(tiktok_share, facebook_share, google_ads_share)
lbls<-c("TikTok", "Facebook", "Google Ads")
pct<-round(slices*100)
lbls<-paste(lbls, pct)   # add percent data to labels
lbls<-paste(lbls, "%", sep="") # add % sign to labels
par(mfrow=c(1,1))
pie(slices, labels=lbls, col=rainbow(length(lbls)), main="Ad Spending Share")

########### Linear regression
## Allow non scientific format
options(scipen=999)

### Assume linear relationship between ad spending and sales

regression1 <- lm(sales~tiktok+facebook+google_ads)
summary(regression1)

#### Dynamic effects

m <- 1   # one lag 
#number of observations
n <- length(sales)
#Build Lag
L1.sales <- c(rep(NA,m), sales[1:(n-m)])

## Allow non scientific format
options(scipen=999)


regression2 <- lm(sales~L1.sales+tiktok+facebook+google_ads)
summary(regression2)

#### Diminishing returns to scale

lntiktok<-log(tiktok+1)
lnfacebook<-log(facebook+1)
lngoogle_ads<-log(google_ads+1)
lnsales<-log(sales+1)

# We add 1 to avoid undefined values of logarithm (log (0)  does not exist)

# Lag for ln
L1.lnsales <- c(rep(NA,m), lnsales[1:(n-m)])


regression3 <- lm(lnsales~L1.lnsales+lntiktok+lnfacebook+lngoogle_ads)
summary(regression3)

m1 <- 1
m2 <- 2
n  <- length(sales)

# level lags
L1.sales <- c(rep(NA, m1), sales[1:(n-m1)])
L2.sales <- c(rep(NA, m2), sales[1:(n-m2)])

# log lags
L1.lnsales <- c(rep(NA, m1), lnsales[1:(n-m1)])
L2.lnsales <- c(rep(NA, m2), lnsales[1:(n-m2)])

# regressions (drop the NA rows)
regression2b <- lm(sales ~ L1.sales + L2.sales + tiktok + facebook + google_ads,
                   na.action = na.omit)
summary(regression2b)

regression3b <- lm(lnsales ~ L1.lnsales + L2.lnsales + lntiktok + lnfacebook + lngoogle_ads,
                   na.action = na.omit)
summary(regression3b)







# Model fit plot
fitted_sales<-ts(regression3$fitted.values, frequency = 52, 
                   start=c(2018,1))
plot(lnsales, type="l", col="blue", main="Sales",lwd=2)
lines(fitted_sales, type="b", col="red")
legend("topleft", lty=1, col=c("blue", "red"),
       c("Logged Sales Data","Fitted"))






#QUESTION 2 

#Retrieve each model coefficient from regression output
# Recall - Log-log model is 'stored' in regression3

beta_tiktok<-summary(regression3)$coefficients[3,1]

beta_facebook<-summary(regression3)$coefficients[4,1]

beta_google_ads<-summary(regression3)$coefficients[5,1]

#Calculate the baseline (average) sales:

average_sales<-mean(sales)

#Calculate the baseline (average) advertising spending for each media:

average_tiktok<-mean(tiktok)

average_facebook<-mean(facebook)

average_google_ads<-mean(google_ads)

### Finally, calculate the unit effects:
# For the formula - see the Appendix of Chapter 3

theta_tiktok<-beta_tiktok*(average_sales/average_tiktok)

theta_facebook<-beta_facebook*(average_sales/average_facebook)

theta_google_ads<-beta_google_ads*(average_sales/average_google_ads)

#How much sales we got thanks to tiktok etc.?
# If we know one unit effect, we need to multiply with total spending.

sum_tiktok<-sum(tiktok)

sum_facebook<-sum(facebook)

sum_google_ads<-sum(google_ads)

#Each media's contribution to sales

tiktok_contribution<-theta_tiktok*sum_tiktok

facebook_contribution<-theta_facebook*sum_facebook

google_ads_contribution<-theta_google_ads*sum_google_ads


### Plot each media contribution in a nice bar plot


media_contribution<-c(tiktok_contribution,facebook_contribution,
                      
                      google_ads_contribution)

media_contribution=round(media_contribution, digits=0)

media_names<-c("TikTok","Facebook","GoogleAds")

df<-data.frame(media_names,media_contribution)

head(df)

library(ggplot2)

barp_plot1<-ggplot(data=df,aes(x=media_names,y=media_contribution)) +
  
  geom_bar(stat="identity", color="black",
           
           fill=c("Red","Orange","Blue")) +
  
  geom_text(aes(label=media_contribution), vjust=-0.3, size=3.5)+
  
  labs(title="Contribution to Sales", x="Media", y="Contribution") +
  
  theme_minimal()

#bar_plot1

# change the order of the bars

barp_plot1 + scale_x_discrete(limits=c("TikTok","Facebook","GoogleAds"))

##############


#### Return on Marketing Investment


#Calculate the cost of each media

cost_tiktok<-sum(tiktok)

cost_facebook<-sum(facebook)

cost_google_ads<-sum(google_ads)

cost_total<-cost_tiktok+cost_facebook+cost_google_ads

cost<-c(cost_tiktok,cost_facebook,cost_google_ads)

cost=round(cost, digits=0)

# Sales Contribution vs. Cost

df3<-data.frame(traf_cost=rep(c("Sales","Cost"), each=3),
                
                media_names=rep(c("TikTok","Facebook","GoogleAds"),2),
                
                values=c(media_contribution, cost))

head(df3)


barp_plot3<-ggplot(data = df3, aes(x = media_names,y = values,fill = traf_cost)) +
  
  geom_bar(stat = "identity", color = "black",
           
           position = position_dodge()) +
  
  labs(title = "Sales vs. Media Cost", x = "Media", y = "Sales and Cost") +
  
  theme_minimal()

bar_plot3<-barp_plot3 + scale_x_discrete(limits = c("TikTok", "Facebook", "GoogleAds"))

bar_plot3

### Instead of Showing cost vs. return, lets calculate ROI

# Calculate the traffic return for each media

roi_tiktok=tiktok_contribution/cost_tiktok

roi_facebook=facebook_contribution/cost_facebook

roi_google_ads=google_ads_contribution/cost_google_ads


# SROMI plot input

roi<-c(roi_tiktok, roi_facebook, roi_google_ads)

# Round off the numbers.

roi=round(roi, digits=4)

media_names<-c("TikTok", "Facebook","GoogleAds")

df4<-data.frame(media_names,roi)

head(df4)


# SROMI bar plot

bar_plot4<-ggplot(data = df4, aes(x = media_names,y = roi)) +
  
  geom_bar(stat = "identity", color = "black",
           
           fill = c("Red","Orange","Blue")) +
  
  geom_text(aes(label = roi), vjust = -0.3, size = 3.5) +
  
  labs(title = "Sales Return on Marketing Investment", x = "Media", y = "SROMI") +
  
  theme_minimal()

bar_plot4<-bar_plot4 + scale_x_discrete(limits=c("TikTok", "Facebook", "GoogleAds"))

bar_plot4

##### Marketing Budget Allocation

#Actual Budget Spending

costshare_tiktok<-cost_tiktok/cost_total

costshare_facebook<-cost_facebook/cost_total

costshare_google_ads<-cost_google_ads/cost_total


# Input for the pie-chart

slices_actual<-c(costshare_tiktok, costshare_facebook,costshare_google_ads)

lbls_actual<-c("TikTok", "Facebook", "GoogleAds")

pct_actual<-round(slices_actual*100)

lbls_actual<-paste(lbls_actual, pct_actual)

# add data to labels

lbls_actual<-paste(lbls_actual, "%", sep= "")

# add % sign to labels

# Get the pie-chart

pie(slices_actual, labels = lbls_actual, col = rainbow(length(lbls_actual)), main = "Actual Ad Spending")


#The sum of all elasticities

beta_allmedia<-beta_tiktok+beta_facebook+beta_google_ads

#Optimal resource allocation

optim_tiktok<-beta_tiktok/beta_allmedia

optim_facebook<-beta_facebook/beta_allmedia

optim_google_ads<-beta_google_ads/beta_allmedia


## Pie-chart ingredients

optimal_spend<-c(optim_tiktok,optim_facebook,optim_google_ads)

optimal_spend=round(optimal_spend, digits=2)

optimal_spend

slices_optim<-c(optim_tiktok, optim_facebook,optim_google_ads)

lbls_optim<-c("TikTok", "Facebook", "GoogleAds")

pct_optim<-round(slices_optim*100)

lbls_optim<-paste(lbls_optim, pct_optim)

# paste variable names to data labels

lbls_optim<-paste(lbls_optim, "%", sep="") # add % sign to labels

# Get the pie-chart

pie(slices_optim, labels=lbls_optim, col=rainbow(length(lbls_optim)), main="Optimal Budget Allocation" )

### Interactions
regression4 <- lm(lnsales~L1.lnsales+lntiktok+lnfacebook+lngoogle_ads + lntiktok:lnfacebook + lntiktok:lngoogle_ads + lngoogle_ads:lnfacebook)
summary(regression4)

# Model fit plot
fitted_tra<-ts(regression4$fitted.values, frequency = 52, 
                   start=c(2018,1))
plot(lnsales, type="l", col="blue", main="Web Sales",lwd=2)
lines(fitted_sales, type="b", col="red")
legend("topleft", lty=1, col=c("blue", "red"),
       c("Logged Sales Data","Fitted"))










