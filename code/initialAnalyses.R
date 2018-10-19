#######################
### Initial Goofing ###
#######################

library(ca)

library(dplyr)

library(ggplot2)

library(sentimentr)

library(qdap)

library(quanteda.dictionaries)

kavTrans = read.csv("data/parsedTranscript.csv", stringsAsFactors = FALSE, 
                   fileEncoding = "UTF-8")

genderData = read.csv("data/genderData.csv")

kavTrans = left_join(kavTrans, genderData, by = "name")

kavTrans$party[is.na(kavTrans$party)] = "witness"

rm(genderData)

## quanteda test

testNRC = liwcalike(kavTrans$commentFull, data_dictionary_NRC)

testRID = liwcalike(kavTrans$commentFull, data_dictionary_RID)

testMFD = liwcalike(kavTrans$commentFull, data_dictionary_MFD)

testNRC = select(testNRC, anger:trust)

testRID = select(testRID, contains("primary"), 
                 contains("secondary"), 
                 contains("emotions"))

testMFD = select(testMFD, harmvirtue:moralitygeneral)

kavTrans = bind_cols(kavTrans, testNRC, testRID, testMFD)

## Person Formality

personFormality = formality(kavTrans$commentFull, kavTrans$name, parallel = TRUE)

plot(personFormality)

## Wordclouds by person

topTalkers = kavTrans %>% 
  filter(name == "KAVANAUGH" | name == "FORD" |
           name == "GRASSLEY" | name == "MITCHELL")

stopWordList = unlist(c(tm::stopwords("SMART"), tm::stopwords("en"), "I"))

trans_cloud(text.var = kavTrans$commentFull, grouping.var = kavTrans$name, 
            stem = TRUE, stopwords = stopWordList, 
            min.freq = 10, proportional = TRUE)

## Conversation Flow

gantt_plot(text.var = kavTrans$commentFull,
           grouping.var = list(kavTrans$name, kavTrans$party, kavTrans$gender), size = 4)

## Sentence Splitting

splitTrans = sentSplit(kavTrans, "commentFull")

wordStat = word_stats(splitTrans$commentFull, 
                      grouping.var = list(splitTrans$name, splitTrans$gender),
                      tot = splitTrans$tot)

plot(wordStat)

## Sentiment

load(url("https://github.com/saberry/courses/blob/master/hash_sentiment_vadar.RData?raw=true"))

kavTrans = kavTrans %>% 
  mutate(rowID = 1:nrow(.))

allSentiment = sentimentr::sentiment(as.character(kavTrans$commentFull), hash_sentiment_vadar)

allSentiment = kavTrans %>% 
  # select(name, party, rowID) %>% 
  left_join(., allSentiment, by = c("rowID" = "element_id")) %>% 
  mutate(sentenceOrder = 1:nrow(.))

ggplot(allSentiment, aes(sentenceOrder, name, color = sentiment)) + 
  geom_point(shape = "|", size = 5, alpha = .5) + 
  scale_color_gradient2(low = "#a50026", mid = "#f7f7f7", high = "#313695") +
  theme_dark()

ggplot(allSentiment, aes(sentenceOrder, name, color = authorityvirtue)) + 
  geom_point(shape = "|", size = 5, alpha = .5) + 
  scale_color_gradient2(low = "#a50026", mid = "#f7f7f7", high = "#313695") +
  theme_dark()

## Question Type

questionTypes = question_type(splitTrans$commentFull, splitTrans$name, neg.cont = TRUE)


## Word Differences

wordDiffs = word_diff_list(kavTrans$commentFull, grouping.var = kavTrans$party)

ltruncdf(unlist(wordDiffs, recursive = FALSE), n = 5)

## Correspondence Analysis

caTest = wfm(kavTrans$commentFull, grouping.var = list(kavTrans$party, kavTrans$gender))

caFit = ca(caTest)

plot(caFit)
