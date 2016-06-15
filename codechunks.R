#' ---
#' title: "Johns Hopkins Data Science Casptone Project - Milestone Report"
#' author: "Gregorio Ambrosio Cestero"
#' date: "June 4, 2016"
#' ---
#' 
#' Last updated: `r Sys.time()`
#' 
#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)

#' ## Introduction 
#' Natural language processing (NLP) is a field of computer
#' science, artificial intelligence, and computational linguistics concerned with
#' the interactions between computers and human (natural) languages. As such, NLP
#' is related to the area of human–computer interaction. Many challenges in NLP
#' involve: natural language understanding, enabling computers to derive meaning
#' from human or natural language input; and others involve natural language
#' generation.
#' 
#' Around the world, people are spending an increasing amount of time on their
#' mobile devices for email, social networking, banking and a whole range of
#' other activities. But typing on mobile devices can be a serious pain. SwiftKey
#' company builds a smart keyboard that makes it easier for people to type on
#' their mobile devices. One cornerstone of their smart keyboard is predictive
#' text models.
#' 
#' This report is part of the Coursera John Hopkins Data Science Capstone
#' Project. The course starts with the basics, analyzing a large corpus of text
#' documents to discover the structure in the data and how words are put
#' together. It covers cleaning and analyzing text data, then building and
#' sampling from a predictive text model. Finally, a predictive text product will
#' be built.
#' 
#' This milestone report provides a first stage in the project providing data
#' acquisition and cleaning, and exploratory analysis of the course data set.
#' 
#' ## Executive summary
#' 
#' Exploratory Data Analysis techniques are typically applied before formal
#' modeling commences and can help inform the development of more complex
#' statistical models. Exploratory techniques are also important for eliminating
#' or sharpening potential hypotheses about the world that can be addressed by
#' the data. To get started Coursera Swiftkey Dataset is downloaded and then some
#' statistics are collected. Since the size of the data set is very big it is
#' randomly sampled to a reduced data set to address the analysis. This sample
#' data set is preprocessed and tokenized for extracting contiguous sequence of
#' items. Concretely unigrams, bigrams and trigrams are extracted. Finally, word
#' clouds for n-grams are showed.
#' 
#' ## Loading data set
#' 
#' The data set comes from [HC Corpora](http://www.corpora.heliohost.org) and is
#' composed of four language corpus: en_US, de_DE, ru_RU and fi_FI.  See the
#' [readme file](http://www.corpora.heliohost.org/aboutcorpus.html) for details
#' on the corpora available. The files have been language filtered but may still
#' contain some foreign text. In this report the english one in chosen to be
#' analyzed.


#' Setting working directory
#+ Setting working directory, ----------------------------------------------------------------------
setwd("~/workspace/coursera/Johns Hopkins Data Science/10 Capstone Project/Data-Science-Capstone-Project")

#' Loading needed packages
#+ Loading needed packages, ----------------------------------------------------------------------
# p_load = install.packages + library
if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
pacman::p_load(stringi, tm, RWeka, ggplot2, magrittr, SnowballC, wordcloud, Rgraphviz)

#' Loading Dataset
#+ Loading Dataset, ----------------------------------------------------------------------
destFile <- "Coursera-SwiftKey.zip"
fileURL <- paste("http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/", destFile, sep="")
if (!file.exists(destFile)) download.file(fileURL ,destFile)    

# Unzziping and setting directories

# Checking if it was previously unzipped
corpora.path       <- "final/en_US"
if (!(file.exists(corpora.path))) unzip(destFile) 
corpora.files      <- file.path(corpora.path, list.files(corpora.path)) 

# Creating sample directory 
corpora.samplePath <- "sample"
if (!file.exists(corpora.samplePath)) dir.create(corpora.samplePath)

#' Files sizes are extracted in megabytes
#+ File size, ----------------------------------------------------------------------
### File size
list.files(corpora.path)
file.info(corpora.files)$size / 1024^2

#' Now files are read. As the corpus is composed of three files, a list is used to store them
#+ Reading data alternative, echo = FALSE, ----------------------------------------------------------------------
### Reading data
#blogs   <- readLines(corpora.files[1], encoding="UTF-8", skipNul = TRUE)
#news    <- readLines(corpora.files[2], encoding="UTF-8", skipNul = TRUE)
#twitter <- readLines(corpora.files[3], encoding="UTF-8", skipNul = TRUE)

#+ Reading data, ----------------------------------------------------------------------
corpora.data <- vector("list",3)
names(corpora.data) <- basename(corpora.files)

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

#+ save the data to a .RData file, echo = FALSE, ----------------------------------------------------------------------
#save (corpora.data, file="corpora.data.Rdata")

#' ## Getting some statistics
#'
#' To have an idea of how the data set looks like, some statistics are
#' extracted. Pure R can be used for that but, in this case, stringi package is
#' used for simplicity and not reinvent the wheel.

#+ Calculating statistics, ----------------------------------------------------------------------
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

#' ## Sampling data
#' 
#' This dataset is fairly large. A smaller subset of the data is used by reading
#' in a random subset of the original data and writing it out to a separate
#' file.
#' 
#+ Sampling data, ----------------------------------------------------------------------
### Sampling data
corpora.sample.percentage <- 0.05

corpora.sample <- vector("list",3)
names(corpora.sample) <- basename(corpora.files)

set.seed(1000)
corpora.sample[[1]] <- corpora.data[[1]][sample(1:length(corpora.data[[1]]), corpora.stats$Lines[[1]]*corpora.sample.percentage)]
corpora.sample[[2]] <- corpora.data[[2]][sample(1:length(corpora.data[[2]]), corpora.stats$Lines[[2]]*corpora.sample.percentage)]
corpora.sample[[3]] <- corpora.data[[3]][sample(1:length(corpora.data[[3]]), corpora.stats$Lines[[3]]*corpora.sample.percentage)]

corpora.sample.stats <- data.frame(t(rbind(sapply(corpora.sample,stri_stats_general),
                                    sapply(corpora.sample,stri_stats_latex)[4,]
                                    )
                              )
                            )
colnames(corpora.sample.stats)[5] <- "Words"
print(corpora.sample.stats)

writeLines(unlist(corpora.sample,recursive=FALSE,use.names=FALSE), file.path(corpora.samplePath,"sampleCorpus.txt"))

#' ## Text mining
#' 
#' Analyzing the text data refers to the process of deriving high-quality
#' information from text. This task is referred as text mining. The best way to
#' do text mining in r is using tm package that provides a text mining
#' framework. The main structure for managing documents in tm is a so-called
#' Corpus, representing a collection of text documents. Once loaded, a
#' preprocessing is done removing punctuation, numbers, stopwords, urls,
#' non-needed characters, common word endings in english and unnecesary
#' whitespaces
#' 


#+ Loading complete corpora, echo = FALSE, ----------------------------------------------------------------------
#corpora.tmCorpus <- Corpus(DirSource(file.path(".", corpora.path)))

#+ Loading sample corpora, ----------------------------------------------------------------------
### Text mining
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

#' ## Exploratory analysis
#' 
#' Now is time to exploratory analysis through the understanding of the
#' distribution of words and relationship between the words in the corpora.
#' Sample data set is tokenized for extracting contiguous sequence of items.
#' Concretely unigrams, bigrams and trigrams are extracted and their frequencies
#' are obtained and plotted.
#' 
#+ Tokenization and frequency extraction, ----------------------------------------------------------------------
unigram.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigram.tokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram.tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigrams <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = unigram.tokenizer))
bigrams  <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = bigram.tokenizer))
trigrams <- DocumentTermMatrix(corpora.tmCorpus, control = list(tokenize = trigram.tokenizer))

unigrams.frequency <- sort(colSums(as.matrix(unigrams)),decreasing = TRUE)#[1:30]
bigrams.frequency  <- sort(colSums(as.matrix(bigrams)),decreasing = TRUE)#[1:30]
trigrams.frequency <- sort(colSums(as.matrix(trigrams)),decreasing = TRUE)#[1:30]

unigrams.frequency.df <- data.frame(word = names(unigrams.frequency), frequency = unigrams.frequency)
bigrams.frequency.df  <- data.frame(word = names(bigrams.frequency), frequency = bigrams.frequency)
trigrams.frequency.df <- data.frame(word = names(trigrams.frequency), frequency = trigrams.frequency)

#+ Frequency analysis,  ----------------------------------------------------------------------
#' Check out the frequency of frequencies:
#' Next, the resulting output is two rows of numbers. The top number is the frequency
#' with which words appear and the bottom number reflects how many words appear
#' that frequently.
head(table(unigrams.frequency),20)
tail(table(unigrams.frequency),20)
#' For a less, fine-grained look at term frequency
head (unigrams.frequency,20)
#' An alternate view of term frequency: this will identify all terms that appear frequently
head(unigrams.frequency,20)
#' Yet another way to do this:
head(unigrams.frequency.df,20)

head(table(bigrams.frequency),20)
tail(table(bigrams.frequency),20)
head (bigrams.frequency,20)
head(bigrams.frequency,20)
head(bigrams.frequency.df,20)

head(table(trigrams.frequency),20)
tail(table(trigrams.frequency),20)
head (trigrams.frequency,20)
head(trigrams.frequency,20)
head(trigrams.frequency.df,20)

#' ### Plotting n-grams
#' 
#+ r Plotting n-grams, ----------------------------------------------------------------------
ggplot(unigrams.frequency.df[1:30,], aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Unigrams") + 
        ylab("Frequency") +
        coord_flip()

ggplot(bigrams.frequency.df[1:30,], aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Bigrams") + 
        ylab("Frequency") +
        coord_flip()

ggplot(trigrams.frequency.df[1:30,], aes(reorder(word,frequency), frequency)) + 
        geom_bar(stat = "identity") + 
        xlab("Trigrams") + 
        ylab("Frequency") +
        coord_flip()

#' ### Word cloud
#' A more human way of frequencies observation is through word clouds. The wordcloud package is user to plot n-grams in a cloudy way

#+ Word cloud, warning = FALSE, ----------------------------------------------------------------------
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

#' ## Next step
#' 
#' To continue the project n-grams will be used to probabilities calculation.
#' Next steps include a n-gram probabilistic based model to next word prediction
#' based on the previous 1, 2 or 3 words. Finally the predcition model will be
#' used trough a shiny app as the goal of the project is defined.
#' 
#' ## Session info
#+ show sessionInfo
sessionInfo()

