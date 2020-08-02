setwd=("/Users/gurpreetsingh/Downloads")
install.packages("fitdistrplus")
install.packages("e1071")
install.packages("mice")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("PerformanceAnalytics")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("tm")
install.packages("syuzhet")
#loading library
library("fitdistrplus")
library(e1071)
library(ggplot2)
library(ggthemes)
library(scales)
library(Hmisc)
library(viridis)
library(hrbrthemes)
library(magrittr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(tm)
library(mice)
library(tidyverse)


## ********** NCT DATA ANALYSIS **************

#Read file
nct2018 <- read.csv("Nct data 2018.csv")
View(nct2018)
names(nct2018)


# Creating a smaller dataset,only using the cars with more than 2000 in number. 
nct <-subset(nct2018, Total>2000)
nct
summary(nct)
str(nct)
any(is.na(nct))

#top 20 cars by highest nct pass percentage
nctpass <- top_n(nct, 20, Pass_Percent)
nctpass
attach(nctpass)
pass <- nctpass[order(-Pass_Percent),] 
pass
detach(nctpass)

#ggplot showing car makes with highest NCT pass rate 
pass %>%
  ggplot(aes(x = Vehicle.Make, y = Pass_Percent)) +
  geom_col() +
  coord_flip() +
  labs(x = "Car Make",
       y = "Pass Rate",
       title = "Top car makes with highest pass rate")


##Checking Corelatin between Engine/Exhaust Problems and Emissions 
library("ggpubr")
ggscatter(nct, x = "Engine.Noise.and.Exhaust.Percentage", y = "Emissions_Percentage", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = " Nct Failed due to Engine & Exhaust Problems", ylab = "Nct Failed due to Emissions")

# Shapiro-Wilk normality test for Engine Problem Rate
shapiro.test(nct$Engine.Noise.and.Exhaust.Percentage)

# Shapiro-Wilk normality test for Emissions Rate
shapiro.test(nct$Emissions_Percentage)


# Graphical representtion to check data normality
library("ggpubr")

ggqqplot(nct$Engine.Noise.and.Exhaust.Percentage, ylab = "Engine & Exhaust Issues")

ggqqplot(nct$Emissions_Percentage, ylab = "Emissions")

## Correlation Test 
cor.test(nct$Engine.Noise.and.Exhaust.Percentage, nct$Emissions_Percentage, method = "pearson")


##  Test to check the Correlation between Fail Percebtage and causes of NCT Failure
names(nct)
newdata <- subset(nct[c("Fail_Percent", "Emissions_Percentage","Engine.Noise.and.Exhaust.Percentage","Steering.and.Suspension.Percentage","Wheels.and.Tyres.Percentage","Braking.Equipment.Percentage")])
view(newdata)
cor(newdata, method = c("pearson", "kendall", "spearman"))





#*********************************** CAR SALES DATA ANALYSIS*************************

cars<- read.csv('Car sales data.csv')
view(cars)

head(cars)

colnames(cars)

summary(cars)
str(cars)
any(is.na(cars))

md.pattern(cars)



##Creating a subset of cars with count over 2000 
Sales <- subset(cars, Sales.2018>2000)
Sales
view(Sales)


##Visulaise 2018 sales data on graph
ggplot(Sales, aes(x=Vehicle.Make, y=Sales.2018, group=Vehicle.Make)) +
  geom_col()


Sales19 <- subset(cars, Sales.2019>2000)
Sales19

##Visulaise 2019 sales data on graph by model
ggplot(Sales19, aes(x=Vehicle.Model, y=Sales.2019, group=Vehicle.Model)) +
  geom_col()

##Visulaise 2019 sales data on graph by make
ggplot(Sales19, aes(x=Vehicle.Make, y=Sales.2019, group=Vehicle.Make)) +
  geom_col()


#*************** REGRESSION MODELS - CAR SALES DATA *******************************************  


##Simple Linear Regeression###
# scatterplot
scatter.smooth(x=cars$Sales.2018, y=cars$Sales.2019, main="Sales.2019 ~ Sales.2018") 

# boxplot to check outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$Sales.2018, main="Sales.2018", sub=paste("Outlier rows: ", boxplot.stats(cars$Sales.2018)$out))  # box plot for 'Sales in 2018'
boxplot(cars$Sales.2019, main="Sales.2019", sub=paste("Outlier rows: ", boxplot.stats(cars$Sales.2019)$out))  # box plot for 'Sales in 2019'

# check correlation
cor(cars$Sales.2019, cars$Sales.2018)

# build linear model
mod <- lm(cars$Sales.2019 ~ cars$Sales.2018)
print(mod)
summary(mod)

#Predict using linear model
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(Sales.2019 ~ Sales.2018, data=trainingData)  # build the model
salesPred <- predict(lmMod, testData)  # predict sales
salesPred
summary(lmMod)

# Prediction
actuals_preds <- data.frame(cbind(actuals=testData$Sales.2019, predicteds=salesPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

# check model accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy



##Mulitple Regression
names(cars)
cor(cars[c("Sales.2017", "Sales.2018", "Sales.2019")])
pairs(cars[c("Sales.2017", "Sales.2018", "Sales.2019")])


library("PerformanceAnalytics")
my_data <- cars[, c(6,7,8)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

Model <- lm( cars$Sales.2019 ~ cars$Sales.2018+ cars$Sales.2017, data=my_data)
Model
summary(Model)

##Check Residuals
cars.stdRes = rstandard(Model)
plot( cars.stdRes, col="blue")
abline(0,0)


################### DATA MINING - CLUSTERING - CAR SALES DATA #################


library(dplyr)
library(cluster) # clustering
library(factoextra) # clustering & cluster visualisation
library(gridExtra) # for plotting multiple visualisations in a grid
install.packages("NbClust")
library(NbClust)

mydata <- Sales19[c("Vehicle.Model","Sales.2016","Sales.2017","Sales.2018","Sales.2019")]
mydata

## Deleting the first column and making Vehicle model columns as Header
rownames(mydata) <- as.matrix(mydata[,1 ])
mydata <- mydata[order(as.numeric(row.names(mydata)), decreasing=FALSE),]
mydata <- mydata[,-1]
as.matrix(mydata)
mydata
mydata <- scale(mydata)

head(mydata)

#calculate euclidian distance
distance <- get_dist(mydata)
##visualizing distance matrix
fviz_dist(
  distance, gradient = list(low = "blue", mid = "white", high = "red"))

k2 <- kmeans(mydata, centers = 2, nstart = 50)
k2 # print clustering output

fviz_cluster(k2, data = mydata)

k3 <- kmeans(mydata, centers = 3, nstart = 50)
k4 <- kmeans(mydata, centers = 4, nstart = 50)
k5 <- kmeans(mydata, centers = 5, nstart = 50)

###

p1 <- fviz_cluster(k2, geom = "point", data = mydata) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = mydata) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = mydata) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = mydata) + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Plot to determine the optimal number of clusters using elbow method.
wssplot <- function(data, max_clusters=15) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (k in 2:max_clusters){
    wss[k] <- sum(kmeans(data, centers=k)$withinss)
  }
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}
set.seed(123)
wssplot(mydata,12)









#************** TWITTER DATA & SENTIMENT ANALYSIS OF CARS (CAR MAKES USED - AUDI , BMW AND CITROEN) ***********************

##twitter 1 --- !! Using method 2 below
install.packages("devtools")
install.packages("githubinstall")
library(devtools) 
install_github("geoffjentry/twitteR")
library(twitteR)
install.packages("base64enc")
library(base64enc)
api_key <- "ZTdE2unvnU3EfS34MFftN4OrJ"
api_secret <- "bauA1EV9CfQkysaYtxhudhlnRIZqxwXgWyFWR4Lm2vF3DTVgEI"
access_token <- "774237811679584256-rL6RNv1P2PKhrCRkXGg0EqvxOVaZgxU" 
access_token_secret <- "D8xCkBgDc6p84GpFlRlxzt6rgisgN4iqIZ3hIMjJ1p1Mq"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##**************************twitter 2 - Using retrieve tweets method****************************
install.packages("rtweet")
install.packages("tidytext")
library(rtweet)

library(tidytext)
# whatever name you assigned to your created app
appname <- "Private Car Analysis in Ireland"

key <- "ZTdE2unvnU3EfS34MFftN4OrJ"
secret <- "bauA1EV9CfQkysaYtxhudhlnRIZqxwXgWyFWR4Lm2vF3DTVgEI"
access_token <- "774237811679584256-rL6RNv1P2PKhrCRkXGg0EqvxOVaZgxU" 
access_secret <- "D8xCkBgDc6p84GpFlRlxzt6rgisgN4iqIZ3hIMjJ1p1Mq"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)
get_token()

##### Fetching Tweets by using @ or # 
rstats_tweets <- search_tweets(q = "@ford",  ####--Downloads 500 tweets related to Ford
                               n = 500)
head(rstats_tweets)
names(rstats_tweets)
twtext <- rstats_tweets$text
View(twtext)
summary(twtext)
str(twtext)
summary(rstats_tweets)
rstats_tweets$location <- factor(rstats_tweets$location)
counts=table(rstats_tweets$location)
barplot(counts)


### Fetching Audi Tweets
audi<- search_tweets(q = "#audi",
                        n = 1000)
View(audi$text)
names(audi)
summary(audi)


### Fetching Citoren Tweets
citroen<- search_tweets(q = "#citroen",
                     n = 1000)

View(citroen$text)
names(citroen)
summary(citroen)


### Fetching BMW Tweets
bmw <- search_tweets(q = "#bmw",
                        n = 1000)

head(bmw)
names(bmw)
View(bmw$text)




##*************MOST FREQUENTLY USED WORDS IN TWEETS (Audi - EXAMPLE)*************************

##Cleaning the tweet text by removing any punctuations, hyperlinks, @mentions 
audi$text <-  gsub("https\\S*", "", audi$text)
audi$text <-  gsub("@\\S*", "", audi$text) 
audi$text  <-  gsub("amp", "", audi$text) 
audi$text  <-  gsub("[\r\n]", "", audi$text)
audi$text  <-  gsub("[[:punct:]]", "", audi$text)
auditweets <- audi %>%select(text) %>%unnest_tokens(word, text)
auditweets <- auditweets %>% anti_join(stop_words)

# Bar chart of the most frequent words found in the tweets
auditweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets about Audi")



##******Most frequently used hashtags in tweets  - Audi Example ************

audi$hashtags <- as.character(audi$hashtags)
audi$hashtags <- gsub("c\\(", "", audi$hashtags)
set.seed(1234)
wordcloud(audi$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



##Sentiment Analysis - BMW 
 
library(syuzhet)
bmw_tweets <- bmw$text
# Converting tweets to ASCII to trackle strange characters
bmw_tweets<- iconv(bmw_tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
#tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
bmw_tweets <-gsub("@\\w+","",bmw_tweets)
ew_sentiment<-get_nrc_sentiment((bmw_tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"

##sentiment scores dataframe contains the count of sentiments shown in the tweets
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores

##Graphical Representation of sentiment scores for Bmw
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("BMW sentiment scores")+
  theme_minimal()



####Sentiment Analysis for AUDI

library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
audi_tweets <- audi$text
View(audi_tweets)
audi_tweets <- iconv(audi_tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
#audi <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",audi)
# removing mentions, in case needed
audi_tweets <-gsub("@\\w+","",audi_tweets)
audi_sentiment<-get_nrc_sentiment((audi_tweets))
audisentimentscores<-data.frame(colSums(audi_sentiment[,]))
names(audisentimentscores) <- "Score"

audisentimentscores <- cbind("sentiment"=rownames(audisentimentscores),audisentimentscores)
rownames(audisentimentscores) <- NULL
audisentimentscores

ggplot(data=audisentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Audi sentiment scores")+
  theme_minimal()



##Sentiment Analysis - CITROEN

citroen_tweets<- citroen$text
View(citroen_tweets)
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
citroen_tweets <- iconv(citroen_tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
#audi <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",audi)
# removing mentions, in case needed
citroen_tweets <-gsub("@\\w+","",citroen_tweets)
cit_sentiment<-get_nrc_sentiment((citroen_tweets))
citsentimentscores<-data.frame(colSums(cit_sentiment[,]))
names(citsentimentscores) <- "Score"
citsentimentscores <- cbind("sentiment"=rownames(citsentimentscores),citsentimentscores)
rownames(citsentimentscores) <- NULL
citsentimentscores

ggplot(data=citsentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Citroen sentiment scores")+
  theme_minimal()


summary(sentimentscores$Score)  
summary(audisentimentscores)
summary(citsentimentscores)



## Creating a dataframe Writing the sentiment scores into a csv file
df <- cbind(sentimentscores, audisentimentscores, citsentimentscores)
write.csv(df,"/Users/gurpreetsingh/Downloads/sentiment_scores.csv", row.names = FALSE)




#****Test to check if there issignificant difference between the sentiment scores of Audi, Bmw and Citroen*********

## Reading the sentiment score csv file 
scores <-read.csv ("sentiment_scores.csv")
scores

# Tests for Normality
shapiro.test(scores$Bmw)
shapiro.test(scores$Audi)
shapiro.test(scores$Citroen)

# qq plots to check normality visually
qqnorm(scores$Bmw, pch = 1)
qqline(scores$Bmw, col = "steelblue", lwd = 2)

qqnorm(scores$Audi, pch = 1)
qqline(scores$Audi, col = "steelblue", lwd = 2)

qqnorm(scores$Citroen, pch = 1)
qqline(scores$Citroen, col = "steelblue", lwd = 2)



names(scores)

#
# Visualise
#
boxplot(scores)
hist(scores$Bmw)
hist(scores$Audi)

hist(scores$Citroen)
#
#kruskal wallis test 
kwTest = kruskal.test(scores)  
kwTest 



##***** chi square test to check the relation between favourite_count and followers_count for Audi Tweets******

View(audi$favorite_count)
View(audi$followers_count)

summary(audi$favorite_count)
summary(audi$followers_count)

boxplot(audi$followers_count)

qqplot(audi$followers_count, audi$favorite_count)
qqline(audi$followers_count, audi$favorite_count)

chisq.test( audi$followers_count, audi$favorite_count)




##******LOCATION BASED MODEL FOR AUDI , BMW AND CITROEN - SHOWS TOP N LOCATIONS OF ORIGIN OF THE TWEETS*****************************

##Checking and removing missing loactions values
loc<-audi$location
loc
any(is.na(audi$location) || audi$location == "")
audi_n<-audi[!(is.na(audi$location) | audi$location == ""),]
audi_n$location
##Plot for locations of twitter users talking about Audi
audin %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "count",
       title = "Location of users tweeting about Audi ")


##Plot for locations of twitter users talking about Bmw 
bmw$location
bmw_n<-bmw[!(is.na(bmw$location) | bmw$location == ""),]
bmw_n$location

bmw_n %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "count",
       title = "Location of users tweeting about Bmw ")


##PLot for locations of twitter users talking about Citroen
citroen$location
citroen_n<-citroen[!(is.na(citroen$location) | citroen$location == ""),]
citroen_n$location

citroen_n %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "count",
       title = "Location of users tweeting about Citroen ")
