# NLP 
# Import data set
dataset1 <- read.delim("Restaurant_Reviews.tsv",header = TRUE, quote = "",
                       stringsAsFactors = FALSE)
# Cleaning text
# Subtracting only Review column from the dataset for text analysis
dataset2 <- dataset1$Review
# Text mining package
#install.packages("tm")
#install.packages("SnowballC")
library(tm)
library(SnowballC)
corpus <-VCorpus(VectorSource(dataset2))
corpus <- tm_map(corpus , content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus,removeWords, stopwords("english"))
as.character(corpus[[3]])
as.character(corpus[[841]])
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)
# Creating bag of word model
# coverting corpus into sparse matrix all the review in the rows and terms in the column
dtm <- DocumentTermMatrix(corpus)
dtm
# remove less frequent words from the data.
dtm <- removeSparseTerms(dtm , 0.999)
dtm
# coverting sparse matrix into dataframe, row represents the review and column represent the words
# are in the data.

dataset <- as.data.frame(as.matrix(dtm))
# add dependent column Review to this data frame and build a classification model to train the model
dataset$Liked <- dataset1$Liked
# We can use Naive bayes or Random forest or Decision tree classification algorithm to build the model 
# and predict the test data set.

## Random Forest ALgorithm
library(caTools)
dataset$Liked <- factor(dataset$Liked, levels = c(0,1))
# spilitting dataset into training and test data set
split <- sample.split(dataset$Liked, SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split== FALSE)
#install.packages("randomForest")
library(randomForest)
classifier <- randomForest(x = training_set[-692], y = training_set$Liked, ntree = 100)
y_pred <- predict(classifier, newdata = test_set[-692])
# Confusion matrix
cm <- table(test_set[,692], y_pred)
# Out of 200 test data sets there are 157 are correct predictions and 43 are incorrect. Since test data was small 
cm
# Accuracy 78.5% not bad
(82+75)/200
#Precision = TP/(TP+FP)
Precision = 80 /(80+20)
Precision
Recall = TP/TP+FN
Recall = 80/(80+31)
Recall
F1Score = 2*Precision*Recall/(Precision+ Recall)
F1Score
# Naive Bayes Alogorithm
library(naivebayes)
library(e1071)
classifier2 <-naiveBayes(x= training_set[-692], y = training_set$Liked)
y_pred <- predict(classifier2, newdata = test_set[-692])
y_pred                
# Confusion matrix
cm <- table(test_set[,692], y_pred)
cm
# Accuracy 53.5%
# Accuracy  = (TP+TN)/(TP+TN+FP+FN)
Accuracy = (20+87)/200
Accuracy
#Precision = TP/(TP+FP)
Precision = 20+87/20+87+80
Precision
# Recall = TP/(TP+FN)
Recall = 20+87/20+87+13
Recall

 F1Score = 2*Precision*Recall/(Precision+ Recall)
F1Score

# Decision Tree 
library(rpart)
classifier3 <- rpart(formula = Liked ~., data = training_set)
classifier
y_predict <- predict(classifier3, newdata = test_set[-692], type = 'class')
cm <- table( test_set[,692], y_pred)
cm
Accuracy = (20+87)/200
#Accuracy 53.5%

# Cart model
library(rpart)
classifier4 <- rpart(Liked ~., data = training_set, control = rpart.control(cp = 0.0001))
y_pred <- predict(classifier4, newdata = test_set[-692], method = 'class')
cm <- table(test_set[,692], y_pred)
