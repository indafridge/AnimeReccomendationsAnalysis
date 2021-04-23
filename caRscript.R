
library()

install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
install.packages("Amelia")
install.packages(c("reshape"))
install.packages("devtools")
install.packages("ggpubr")

library(readr)
library(Amelia)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(mice)
library(reshape)
library(ggpubr)

setwd("C:/Users/eoinm/Documents/school/College/data and web minining/ca")

anime<-read.csv("anime.csv",header=T, na.strings=c(""),stringsAsFactors = T)
animeRating<-read.csv("rating.csv",header=T, na.strings=c(""),stringsAsFactors = T)

merged_Anime <-merge(anime,animeRating[c('user_id','rating','anime_id')],by="anime_id")

ggplot(merged_Anime, aes(x = episodes, y = rating.x)) +
  geom_point() 
  
ggplot(merged_Anime, aes(x = rating.x, y = episodes)) +
  geom_point() 
  
ggplot(merged_Anime, aes(x = rating.y, y = episodes)) +
  geom_point() 

ggplot(merged_Anime, aes(x = rating.y, y = episodes)) +
  geom_point() 

ggplot(merged_Anime, aes(x = rating.x, y = genre)) +
    geom_point() 

ggplot(merged_Anime, aes(x = genre, y = rating.y)) +
  geom_point() 
  
  #Simple Scatterplot
  attach(merged_Anime)
plot(rating.x, episodes, main="episode numbers by rating",
     xlab="rating ", ylab="number of episodes ", pch=19)

# Add fit lines
abline(lm(episodes~rating.x), col="red") # regression line (y~x)
lines(lowess(rating.x,episodes), col="blue") # lowess line (x,y)

ggplot(merged_Anime, aes(x = name, y = episodes)) +
  geom_point() +
  
  #Simple Scatterplot
  attach(merged_Anime)
plot(rating.y, genre, main="rating by genre",
     xlab="rating ", ylab="genre ", pch=19)

# Add fit lines
abline(lm(genre~rating.y), col="red") # regression line (y~x)
lines(lowess(rating.y,genre), col="blue") # lowess line (x,y)

#Simple Scatterplot
attach(merged_Anime)
plot(genre, rating.y, main="rating by genre",
     xlab="rating ", ylab="genre ", pch=19)

# Add fit lines
abline(lm(rating.y~genre), col="red") # regression line (y~x)
lines(lowess(genre,rating.y), col="blue") # lowess line (x,y)

cor(merged_Anime$members,merged_Anime$rating.y)
wilcox.test(merged_Anime$members,merged_Anime$rating.y, alternative="two.sided")
kruskal.test(merged_Anime$members,merged_Anime$rating.y, data = merged_Data)
ks.test(merged_Anime$members,merged_Anime$rating.y)

boxplot(merged_Anime)

ggplot(merged_Anime) +
  aes(x = episodes, y = rating.y, color = episodes) +
  geom_jitter() +
  theme(legend.position = "none")

ggscatter(merged_Anime, x="episodes", y="rating.y",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab="no. of episodes", ylab = "rating")

ggscatter(merged_Anime, x="genre", y="rating.y",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab="genre", ylab = "rating")


qplot(data = merged_Anime,episodes,fill = type,bins = 30)

ggplot(merged_Anime,aes(x=episodes,y=rating.y))+
  geom_point(aes(size=members,col=type))

qplot(data = merged_Anime,episodes,fill = genre,bins = 30)

ggplot(merged_Anime,aes(x=episodes,y=rating.y))+
  geom_point(aes(size=members,col=genre))

qplot(data = merged_Anime,episodes,fill = members,bins = 30)

ggplot(merged_Anime,aes(x=episodes,y=rating.y))+
  geom_point(aes(size=members,col=members))


#random forest
library(C50) # for the C5.0 model
library(rpart) # for the recursive partitioning model
library(rpart.plot) # for plotting recursive partitioning models
library(RColorBrewer) # for colour palettes
library(caret) # for the confusion matrix and model metrics
library(ROCR) # for the ROC curve
library(rattle) # for fancy RPart plot
install.packages("adabag")
library(adabag)

data(merged_Anime)
summary(merged_Anime)

ggplot(merged_Anime, aes(episodes)) + geom_bar()

merged_Anime.train.index <- createDataPartition(
  merged_Anime$episodes,
  p = .75,
  list = FALSE
)
# create the train dataset using the indexes from the partitioning above
merged_Anime.train <- merged_Anime[merged_Anime.train.index,]
# create the test dataset using all but the indexes from the partitioning above
merged_Anime.test <- merged_Anime[-merged_Anime.train.index,]

ggplot(merged_Anime.train, aes(rating.x)) + geom_bar()

fit.rpart <- rpart( # train a recursive partitioning model
  rating.x ~ ., # with type as the response variable, all others as explanatory
  data = merged_Anime.train # and fgl.train as the training dataset
)

prp( # plot the fitted model
  fit.rpart, # the fitted model
  faclen = 0, # the length of the factor level names. 0 = full
  cex = 0.6, # text size
  extra = 1 # extra information printed in the nodes
  # 1 = the number of observations per class that in the node
)

rpart.predict <- predict( # predict using an recursive partitioning model
  fit.rpart, # our rpart model
  newdata = merged_Anime.test[,-10], # the test dataset
  type="class" # the type of prediction (classification)
)

rpartConfusionMatrix <- confusionMatrix(
  rpart.predict, # the predicted classes
  merged_Anime.test[,10] # the actual classes
)
rpartConfusionMatrix


fit.bagged <- bagging( # train a bagged model
  type ~ ., # with type as the response variable, all others as explanatory
  data = merged_Anime.train # and fgl.train as the training dataset
)

bagged.predict <- as.factor( # convert the predictions to a factor variable
  predict(
    fit.bagged, # the bagged model
    newdata=merged_Anime.test[,-10], # the test dataset
    type="class")$class # the type of prediction
)

baggedConfusionMatrix <- confusionMatrix(
  bagged.predict, # the predicted values
  merged_Anime.test[,10] # the actual values
)
baggedConfusionMatrix

fit.boosted <- boosting( # train a boosted model
  type ~ ., # with type as the response variable, all others as explanatory
  data = merged_Anime.train # and fgl.train as the training dataset
)

boosted.predict <- as.factor( # convert the predictions to a factor variable
  predict(
    fit.boosted, # the boosted model
    newdata=merged_Anime.test[,-10], # the test dataset
    type="class")$class # the type of prediction
)

boostedConfusionMatrix <- confusionMatrix(
  boosted.predict, # the predicted values
  merged_Anime.test[,10] # the actual values
)
boostedConfusionMatrix

fit.rf <- randomForest( # train a random forest model
  type ~ ., # with type as the response variable, all others as explanatory
  data = merged_Anime.train # and fgl.train as the training dataset
)

rf.predict <- predict(
  fit.rf, # the fitted random forest model
  newdata = merged_Anime.test, # the test dataset
  type="class" # the type of prediction
)

rfConfusionMatrix <- confusionMatrix(
  rf.predict, # the predicted classes
  merged_Anime.test[,10] # the actual classes.
)
rfConfusionMatrix


