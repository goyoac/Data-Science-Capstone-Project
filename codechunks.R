

setwd("~/workspace/coursera/Johns Hopkins Data Science/10 Capstone Project/Data-Science-Capstone-Project")

### Loading needed packages
# p_load = install.packages + library

if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
pacman::p_load(stringi, tm, RWeka, ggplot2, magrittr, wordcloud, Rgraphviz)

### Loading Dataset

destFile <- "Coursera-SwiftKey.zip"
fileURL <- paste("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/", destFile, sep="")
if (!file.exists(destFile)) download.file(fileURL ,destFile)    

corpora.path       <- "final/en_US/"
corpora.samplePath <- "sample/"
corpora.fileNames  <- list.files(corpora.path)
corpora.files      <- paste(corpora.path, corpora.fileNames, sep="") 

if (!all(file.exists(corpora.files))) unzip(destFile, list = TRUE ) 

### File size
list.files(corpora.path)
file.info(corpora.files)$size / 1024^2

### Reading data
#blogs   <- readLines(corpora.files[1], encoding="UTF-8", skipNul = TRUE)
#news    <- readLines(corpora.files[2], encoding="UTF-8", skipNul = TRUE)
#twitter <- readLines(corpora.files[3], encoding="UTF-8", skipNul = TRUE)

corpora.data <- vector("list",3)
names(corpora.data) <- corpora.fileNames

con <- file(corpora.files[1], open="rb")
corpora.data[[1]] <- readLines(con, encoding="UTF-8", skipNul = TRUE)
close(con)

con <- file(corpora.files[2], open="rb")
corpora.data[[2]] <- readLines(con, encoding="UTF-8", skipNul = TRUE)
close(con)

con <- file(corpora.files[3], open="rb")
corpora.data[[3]] <- readLines(con, encoding="UTF-8", skipNul = TRUE)
close(con)

rm(con)

# save the data to a .RData file
#save (corpora.data, file="corpora.data.Rdata")

### Statistics
# Number of lines
# Number of non-empty lines
# Number of characters
# Number of non-white characters
# Number of words
corpora.stats <- data.frame(t(rbind(sapply(corpora.data,stri_stats_general),
                                    sapply(corpora.data,stri_stats_latex)[4,]
                                    )
                              )
                            )
colnames(corpora.stats)[5] <- "Words"
print(corpora.stats)

### Sampling data
set.seed(1000)

corpora.sample <- vector("list",3)
names(corpora.sample) <- corpora.fileNames

corpora.sample[[1]] <- corpora.data[[1]][sample(1:length(corpora.data[[1]]),10000)]
corpora.sample[[2]] <- corpora.data[[2]][sample(1:length(corpora.data[[2]]),10000)]
corpora.sample[[3]] <- corpora.data[[3]][sample(1:length(corpora.data[[3]]),10000)]

writeLines(unlist(corpora.sample,recursive=FALSE,use.names=FALSE), paste(corpora.samplePath,"sampleCorpus.txt"))

### Text mining

# Loading complete corpora
#corpora.tmCorpus <- Corpus(DirSource(file.path(".", corpora.path)))

# Loading sample corpora
corpora.tmCorpus <- Corpus(DirSource(file.path(".", corpora.samplePath)))


# Removing punctuation
corpora.tmCorpus <- tm_map(corpora.tmCorpus, removePunctuation)

# Removing numbers
corpora.tmCorpus <- tm_map(corpora.tmCorpus, removeNumbers)

# Converting to lowercase
corpora.tmCorpus <- tm_map(corpora.tmCorpus, content_transformer(tolower))

# Removing english stop words
corpora.tmCorpus <- tm_map(corpora.tmCorpus, removeWords, stopwords("english"))

# Setting content_transformer to change a pattern to space character
f <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# Removing URL
corpora.tmCorpus <- tm_map(corpora.tmCorpus, f, "http[[:alnum:]]*")

# Removing some non-needed characters
corpora.tmCorpus <- tm_map(corpora.tmCorpus, f, "/|@|\\|")

# Removing common word endings (e.g., “ing”, “es”, “s”), i.e. stemming words
corpora.tmCorpus <- tm_map(corpora.tmCorpus, stemDocument)

# Stripping unnecesary whitespace
corpora.tmCorpus  <- tm_map(corpora.tmCorpus, stripWhitespace)

# Telling R to treat your preprocessed documents as text documents
corpora.tmCorpus <- tm_map(corpora.tmCorpus, PlainTextDocument)

### Exploratory analysis

### Tokenization

unigram.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram.tokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigrams <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = unigram.tokenizer))
bigrams  <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = bigram.tokenizer))
trigrams <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = trigram.tokenizer))

unigrams.frequency <- sort(colSums(as.matrix(unigrams)),decreasing = TRUE)[1:30]
bigrams.frequency  <- sort(colSums(as.matrix(bigrams)),decreasing = TRUE)[1:30]
trigrams.frequency <- sort(colSums(as.matrix(trigrams)),decreasing = TRUE)[1:30]

unigrams.frequency.df <- data.frame(word = names(unigrams.frequency), frequency = unigrams.frequency)
bigrams.frequency.df  <- data.frame(word = names(bigrams.frequency), frequency = bigrams.frequency)
trigrams.frequency.df <- data.frame(word = names(trigrams.frequency), frequency = trigrams.frequency)

ggplot(unigrams.frequency.df, aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Unigrams") + 
        ylab("Frequency") +
        coord_flip()

ggplot(bigrams.frequency.df, aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Bigrams") + 
        ylab("Frequency") +
        coord_flip()

ggplot(trigrams.frequency.df, aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Trigrams") + 
        ylab("Frequency") +
        coord_flip()

### Word cloud

par(mfrow = c(1,3))

wordcloud(words = names(unigrams.frequency), freq = unigrams.frequency, min.freq = 3,
          scale=c(3, .25),
          #colors=brewer.pal(9, "YlOrRd"),
          rot.per=0.35,
          random.order = F)

wordcloud(words = names(bigrams.frequency), freq = bigrams.frequency, min.freq = 3,
          scale=c(3, .25),
          #colors=brewer.pal(9, "YlOrRd"),
          rot.per=0.35,
          random.order = F)

wordcloud(words = names(trigrams.frequency), freq = trigrams.frequency, min.freq = 3,
          scale=c(3, .25),
          #colors=brewer.pal(9, "YlOrRd"),
          rot.per=0.35,
          random.order = F)



# ### Number of lines
# length(blogs)
# length(news)
# length(twitter)
# 
# ### Number of non-empty lines
# stats_blogs   <- stri_stats_general(blogs)
# stats_news    <- stri_stats_general(news)
# stats_twitter <- stri_stats_general(twitter)
# 
# ### number of words
# words_blogs   <- stri_count_words(blogs)
# words_news    <- stri_count_words(news)
# words_twitter <- stri_count_words(twitter)
# 
# summary( words_blogs   )
# summary( words_news    )
# summary( words_twitter )



# ### distribution of words
# tdm <- TermDocumentMatrix(corpora.tmCorpus, control = list(wordLengths = c(1, Inf)))
# # tdm
# freq.terms <- findFreqTerms(tdm, lowfreq=15)[1:20]
# plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)
# 
# # remove sparse terms
# tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
# m2 <- as.matrix(tdm2)
# # cluster terms
# distMatrix <- dist(scale(m2))
# fit <- hclust(distMatrix, method = "ward.D")
# 
# plot(fit)
# rect.hclust(fit, k = 6) # cut tree into 6 clusters 

