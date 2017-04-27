library(stackr)
library(tm)
library(stringr)
library(dplyr)
library(wordcloud2)
library(e1071)
library(stats)
library(C50)

# We load the datasets

load(file = "closed questions.Rdata")
load(file = "noclosed questions.Rdata")

# We do the text mining preprocesing

 all.questions <- c(closed.questions$body,noclosed.questions$body)
 
 closed_corpus <- Corpus(VectorSource(all.questions))
 
 closed_corpus = tm_map(closed_corpus, removeNumbers)
 closed_corpus = tm_map(closed_corpus, removePunctuation)
 closed_corpus = tm_map(closed_corpus, removeWords, c(stopwords("english")))
 closed_corpus =  tm_map(closed_corpus, stripWhitespace)
 
 
 closed_dtm <- DocumentTermMatrix(closed_corpus,control = list(weighting = weightTfIdf))
 closed_dtm = removeSparseTerms(closed_dtm, 0.95)
 
 inspect(closed_dtm)
 
 

 
 # We build the model
 vector.class <- c(rep(T,189),rep(F,1000))

 train.sample <- sample(1:1189,832)
 test.sample <- sample(c(1:1189)[-train.sample],357)
 
 closed.training <- as.matrix(closed_dtm)[train.sample,]
 closed.testing <- as.matrix(closed_dtm)[test.sample,]
 
 
# a svm model:
 
 closed.svm <- svm(closed.training, y= vector.class[train.sample], kernel = "polynomial",type= "one-classification", cross = 10, cachesize = 200, nu= 0.4, degree = 7)
 

 pred.test = predict(closed.svm, closed.testing)
 

 table(pred.test,vector.class[test.sample])
 
# A tree based model:  

 tree.model <- C5.0(x= closed.training, y = as.factor(vector.class[train.sample]), trials = 20)
 p <- predict(tree.model,closed.testing)
 table(p ,as.factor(vector.class[test.sample]))

# A naive bayes model  
 
 vector.class.2 <- c(rep("T",189),rep("F",1000))
closed.test <- cbind(closed.testing,vector.class.2[test.sample])
closed.train <- cbind(closed.training, vector.class.2[train.sample])
colnames(closed.train)[57] <- "funcion"
colnames(closed.test)[57] <- "funcion"
naive.closed.model <- naiveBayes(x = closed.training , y = vector.class[train.sample] , data = closed.train)
p1 <- predict(naive.closed.model,closed.testing) 
table(p1, vector.class.2[test.sample]) 
 










