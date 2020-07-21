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
library("tidyverse")


## ********** NCT DATA ANALYSIS **************

#Read file
nct2018 <- read.csv("Make Model data 2018 (1).csv")
View(nct2018)
names(nct2018)

# Creating a smaller dataset,only using the cars with more than 2000 in number. 
nct <-subset(nct2018, Total>2000)
nct

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
  labs(x = "Car Model",
       y = "Pass Rate",
       title = "Top car makes with highest pass percentage")


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

cars<- read.csv('cars2.csv')
view(cars)

head(cars)

colnames(cars)

summary(cars)
str(cars)
any(is.na(cars))

md.pattern(cars)
ggplot(cars, aes(y=cars$Vehicle.Make)) + 
  geom_line(aes(x = cars$Sales.2014), color = "darkred")


##Creating a subset of cars with count over 2000 
Sales <- subset(cars, Sales.2018>2000)
Sales
view(Sales)

##Visulaise sales data on graph()
ggplot(Sales, aes(x=Vehicle.Make, y=Sales.2018, group=Vehicle.Make)) +
  geom_col()


Sales19 <- subset(cars, Sales.2019>2000)
Sales19


##Visulaise sales data on graph
ggplot(Sales19, aes(x=Vehicle.Make, y=Sales.2019, group=Vehicle.Make)) +
  geom_col()


#*************** REGRESSION MODELS - CAR SALES DATA *******************************************  


##Simple Linear Regeression###
plot(cars$Sales.2014, cars$Sales.2015, main ="Scatterplot")
cor(cars$Sales.2014, cars$Sales.2015)
mod <- lm(cars$Sales.2015 ~ cars$Sales.2014)
summary(mod)
abline(mod)

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

mydata <- Sales19[c("Sales.2016","Sales.2017","Sales.2018","Sales.2019")]
mydata
mydata <- scale(mydata, center = TRUE, scale =  TRUE)

head(mydata)

distance <- get_dist(mydata)

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


wssplot <- function(data, max_clusters=15) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (k in 2:max_clusters){
    wss[k] <- sum(kmeans(data, centers=k)$withinss)
  }
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}


set.seed(42)
wssplot(mydata,10)









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
                        n = 500)
View(audi$text)
names(audi)
summary(audi)


### Fetching Citoren Tweets
citroen<- search_tweets(q = "#citroen",
                     n = 500)

View(citroen$text)
names(citroen)
summary(citroen)


### Fetching BMW Tweets
bmw <- search_tweets(q = "#bmw",
                        n = 500)

head(bmw)
names(bmw)
View(bmw$text)




##*************MOST FREQUENTLY USED WORDS IN TWEETS (BMW - EXAMPLE)*************************

##Cleaning the tweet text by removing any punctuations, hyperlinks, @mentions 
bmw$text <-  gsub("https\\S*", "", bmw$text)
bmw$text <-  gsub("@\\S*", "", bmw$text) 
bmw$text  <-  gsub("amp", "", bmw$text) 
bmw$text  <-  gsub("[\r\n]", "", bmw$text)
bmw$text  <-  gsub("[[:punct:]]", "", bmw$text)
bmwtweets <- bmw %>%select(text) %>%unnest_tokens(word, text)
bmwtweets <- bmwtweets %>% anti_join(stop_words)

# Bar chart of the most frequent words found in the tweets
bmwtweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets about BMW",
       subtitle = "Stop words removed from the list")



##******Most frequently used hashtags in tweets  - BMW Example ************

bmw$hashtags <- as.character(bmw$hashtags)
bmw$hashtags <- gsub("c\\(", "", bmw$hashtags)
set.seed(1234)
wordcloud(bmw$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
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

chisq.test(audi$favorite_count, audi$followers_count)




##******LOCATION BASED MODEL FOR AUDI , BMW AND CITROEN - SHOWS TOP N LOCATIONS OF ORIGIN OF THE TWEETS*****************************



##Plot for locations of twitter users talking about Audi
audi %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Location of users tweeting about Audi ")


##Plot for locations of twitter users talking about Bmw 
bmw %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Location of users tweeting about Bmw ")


##PLot for locations of twitter users talking about Citroen
citroen %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Location of users tweeting about Citroen ")
