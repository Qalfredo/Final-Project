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

 train <- sample(1:1189,832)
 test <- sample(c(1:1189)[-train],357)
 
 closed.training <- as.matrix(closed_dtm)[train,]
 closed.testing <- as.matrix(closed_dtm)[test,]
 
 
# a svm model:
 
 closed.svm <- svm(closed.training, y= vector.class[train], kernel = "polynomial",type= "one-classification", cross = 10, cachesize = 200, nu= 0.4, degree = 7)
 

 pred.test = predict(closed.svm, closed.testing)
 

 table(pred.test,vector.class[test])
 
# A tree based model:  
 
 tree.model <- C5.0(x= closed.training, y = as.factor(vector.class[train]), trials = 5)
 p <- predict(tree.model,closed.testing)
 table(p ,as.factor(vector.class[test]))

# A naive bayes model  
 
 vector.class.2 <- c(rep("T",189),rep("F",1000))
closed.test <- cbind(closed.testing,vector.class.2[test])
closed.train <- cbind(closed.training, vector.class.2[train])
colnames(closed.train)[57] <- "funcion"
colnames(closed.test)[57] <- "funcion"
naive.closed.model <- naiveBayes(x = closed.training , y = vector.class[train] , data = closed.train)
p1 <- predict(naive.closed.model,closed.testing) 
table(p1, vector.class.2[test]) 
 




# we want to know what means the svm model

w <- t(model.svm$coefs) %*% closed.training[model.svm$index,]

w <- as.data.frame(w)
df <- data.frame(term = colnames(w), weight = as.numeric(w[1,1:125 ]))

ggplot(df, aes(x = term , y = weight)) + geom_bar(stat = "identity",  width = 0.3) + coord_flip() + xlab("Tags") + ylab("Frecuency") 

df <- filter(df, weight >= 4)

ggplot(df)
