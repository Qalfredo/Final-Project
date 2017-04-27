library(ggplot2)
library(tm)
library(stringr)
library(wordcloud2)
library(dplyr)

load(file = "tags.Rdata")
load( file = "questions_2015.Rdata")
load(file = "closed questions.Rdata")
load(file = "noclosed questions.Rdata")

df <- select(tags, name, count)
nube <- wordcloud2(df, size = 0.8, shape = "circle")
grafico.1 <- ggplot(df[1:20, ], aes(x = name, y = count)) + geom_bar(stat = "identity", width = 0.5) + coord_flip()

#  Exploratrory analysis
# We want to see most frequent tags and its relation with variable "is_answered"
# we are going to do the text mining treatment:

tag_corpus <- Corpus(VectorSource(questions$tags))


tag_corpus = tm_map(tag_corpus, removeNumbers)
tag_corpus = tm_map(tag_corpus, removePunctuation)
tag_corpus = tm_map(tag_corpus, removeWords, c("then","hence","cdot","cdots","number","mathbb","mathbbr","blockquote","doesnt","therefore","for","forall","int","like","such","let","be","the", "and", stopwords("english")))
tag_corpus =  tm_map(tag_corpus, stripWhitespace)



tag_dtm <- DocumentTermMatrix(tag_corpus)
inspect(tag_dtm)

tag.dataframe <- as.matrix(tag_dtm)

# search for most frequent terms with minimal frequency = 10

freq_terms <- findFreqTerms(tag_dtm,lowfreq = 50)

# we build a dataframe with terms and frequency

f <- c()
for (t in freq_terms){
  f <- c(f,colSums(as.matrix(tag.dataframe[ ,t])))
}

freq.terms.dataframe <- data.frame(freq_terms,f)

# we plot the histogram:


grafico.2 <- ggplot(freq.terms.dataframe, aes(x = freq_terms,y = f)) + geom_bar(stat = "identity",  width = 0.5) + coord_flip() + xlab("Tags") + ylab("Frecuency") 

# we want the "is_answered" histogram :

as.data.frame(table(questions$is_answered))
grafico.3 <- ggplot(questions,aes(x = is_answered)) + geom_histogram(stat = "count", width = 0.5) + xlab("Is answered") + ylab("Frequency")

## we are interested to know what is the most frequent no answered tag

no_answered <- filter(questions, questions$is_answered == F)

# we are going to do the text mining treatment again: 

tag_noa_corpus <- Corpus(VectorSource(no_answered$tags))

tag_noa_corpus = tm_map(tag_noa_corpus, removeNumbers)
tag_noa_corpus = tm_map(tag_noa_corpus, removePunctuation)
tag_noa_corpus = tm_map(tag_noa_corpus, removeWords, c("then","hence","cdot","cdots","number","mathbb","mathbbr","blockquote","doesnt","therefore","for","forall","int","like","such","let","be","the", "and", stopwords("english")))
tag_noa_corpus =  tm_map(tag_noa_corpus, stripWhitespace)



tag_noa_dtm <- DocumentTermMatrix(tag_noa_corpus)

inspect(tag_noa_dtm)

tag.noa.dataframe <- as.data.frame(as.matrix(tag_noa_dtm))

# search for most frequent terms with minimal frequency = 10

freq_noa_terms <- findFreqTerms(tag_noa_dtm,lowfreq = 10)

# we build a dataframe with terms and frequency

f_noa <- c()
for (t in freq_noa_terms){
  f_noa <- c(f_noa,colSums(as.matrix(tag.noa.dataframe[ ,t])))
}

freq.terms.noadataframe <- data.frame(freq_noa_terms,f_noa)

# we plot the histogram:


grafico.4 <- ggplot(freq.terms.noadataframe, aes(x = freq_noa_terms,y = f_noa)) + geom_bar(stat = "identity",  width = 0.3) + coord_flip() + xlab("Tags") + ylab("Frecuency") 



# we build the scatter plots

ggplot(questions, aes(x = questions$score, y = questions$view_count)) + geom_point(size = 0.9, aes(colour = factor(questions$is_answered)) ) + xlab("Score") + ylab("Visualizaciones") + scale_color_discrete(name= "Respuesta válida")

ggplot(questions, aes(x = questions$answer_count, y = questions$score)) + geom_point(size = 0.9, aes(colour = factor(questions$is_answered)) ) + xlab("Respuestas") + ylab("Score") + scale_color_discrete(name = "Respuesta válida")



clo <- select(closed.questions, is_answered, score, view_count, answer_count)
clo2 <- select(noclosed.questions, is_answered, score, view_count, answer_count)
cloo <- rbind(clo, clo2)
cloo$"is_closed" <- c(rep(T,189),rep(F,1000))

ggplot(cloo, aes(x = cloo$score, y = cloo$view_count)) + geom_point(size = 3, aes(colour = factor(cloo$is_closed)) , alpha =0.3) + ylab ("Visualizaciones") + xlab("Score") + scale_color_discrete(name= "Cerrada")






