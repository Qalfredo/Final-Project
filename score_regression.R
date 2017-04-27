library(stackr)
library(tm)
library(stringr)
library(dplyr)
library(wordcloud2)
library(e1071)
library(stats)


load(file = "questions 2000.Rdata")



# We are going to apply preprocesing text mining functions 

questions_2000_corpus <- Corpus(VectorSource(questions_2000$body))

questions_2000_corpus = tm_map(questions_2000_corpus, removeNumbers)
questions_2000_corpus = tm_map(questions_2000_corpus, removePunctuation)
questions_2000_corpus = tm_map(questions_2000_corpus, removeWords, c(stopwords("english")))
questions_2000_corpus =  tm_map(questions_2000_corpus, stripWhitespace)


questions_2000_dtm <- DocumentTermMatrix(questions_2000_corpus,control = list(weighting = weightTfIdf))
questions_2000_dtm = removeSparseTerms(questions_2000_dtm, 0.95)

inspect(questions_2000_dtm)


# We build the model

questions_2000_mining <- cbind(questions_2000$score,as.matrix(questions_2000_dtm))
questions_2000_mining <- as.data.frame(questions_2000_mining)
colnames(questions_2000_mining)[1] <- "score"
colnames(questions_2000_mining)[2:134] <- paste0("c",2:134)


train.2000 <- sample(1:2000,1400)
test.2000 <- sample(c(1:2000)[-train.2000],600)

questions.2000.training <- questions_2000_mining[train.2000,]
question.2000.testing <- questions_2000_mining[test.2000,]


fmla <- as.formula(paste("score ~ ", paste(colnames(questions_2000_mining)[2:134], collapse= "+")))





model.svr <- svm( fmla , data = questions.2000.training, cost = 0.01,cross = 10)


pred.svr <- predict(model.svr, question.2000.testing)

error <- question.2000.testing$score - pred.svr



count = 0
for(i in 1:600){
  if (abs(error[i]) <= 1.5){count = count + 1}else{count = count}
}


rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(error)











