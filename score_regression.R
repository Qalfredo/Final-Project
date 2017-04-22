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

questions_2000_mining$score <- as.factor(questions_2000_mining$score)
train <- sample(1:2000,1400)
test <- sample(c(1:2000)[-train],600)

questions.training <- questions_mining[train,]
question.testing <- questions_mining[test,]


fmla <- as.formula(paste("score ~ ", paste(colnames(questions.training)[2:134], collapse= "+")))






model.svr <- svm( score ~ c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c17+c18+c19+c20+c21+c22+c23+c24+c25+c26+c27+c28+c29+c30+c31+c32+c33+c34+c35+c36+c37+c38+c39+c40+c41+c42+c43+c44+c45+c46+c47+c48+c49+c50+c51+c52+c53+c54+c55+c56+c57+c58+c59+c60+c61+c62+c63+c64+c65+c66+c67+c68+c69+c70+c71+c72+c73+c74+c75+c76+c77+c78+c79+c80+c81+c82+c83+c84+c85+c86+c87+c88+c89+c90+c91+c92+c93+c94+c95+c96+c97+c98+c99+c100+c101+c102+c103+c104+c105+c106+c107+c108+c109+c110+c111+c112+c113+c114+c115+c116+c117+c118+c119+c120+c121+c122+c123+c124+c125+c126+c127+c128+c129+c130+c131+c132+c133+c134 , data = questions.training, cost = 0.01,cross = 10)

model.svr <- svm( fmla , data = questions.training, cost = 0.01,cross = 10)


pred.svm = predict(model.svr, question.testing)

error <- question.testing$score - pred.svm



count = 0
for(i in 1:600){
  if (abs(error[i]) <= 1){count = count + 1}else{count = count}
}
















