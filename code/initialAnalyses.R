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

kavTrans$commentFull = replace_abbreviation(kavTrans$commentFull)

kavTrans$commentFull = gsub("(?<=[A-Z])\\.|(?<=[0-9])\\.", "", kavTrans$commentFull, perl = TRUE)

genderData = read.csv("data/genderData.csv")

kavTrans = left_join(kavTrans, genderData, by = "name")

kavTrans$party[is.na(kavTrans$party)] = "witness"

rm(genderData)

splitTrans = sentSplit(kavTrans, "commentFull")

splitTrans = na.omit(splitTrans)

## quanteda test

testNRC = liwcalike(splitTrans$commentFull, data_dictionary_NRC)

testRID = liwcalike(splitTrans$commentFull, data_dictionary_RID)

testMFD = liwcalike(splitTrans$commentFull, data_dictionary_MFD)

testNRC = select(testNRC, anger:trust)

testRID = select(testRID, contains("primary"), 
                 contains("secondary"), 
                 contains("emotions"))

testMFD = select(testMFD, harmvirtue:moralitygeneral)

kavTrans = bind_cols(splitTrans, testNRC, testRID, testMFD)

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

## Word Stats

wordStat = word_stats(splitTrans$commentFull, 
                      grouping.var = list(splitTrans$name, splitTrans$gender),
                      tot = splitTrans$tot)

plot(wordStat)

## Sentiment

load(url("https://github.com/saberry/courses/blob/master/hash_sentiment_vadar.RData?raw=true"))

kavTrans = kavTrans %>% 
  mutate(rowID = 1:nrow(.), 
         sentenceOrder = 1:nrow(.))

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

ggplot(allSentiment, aes(sentenceOrder, name, color = anger)) + 
  geom_point(shape = "|", size = 5, alpha = .5) + 
  scale_color_gradient2(low = "#a50026", mid = "#f7f7f7", high = "#313695") +
  theme_dark()

ggplot(allSentiment, aes(sentenceOrder, name, color = disgust)) + 
  geom_point(shape = "|", size = 5, alpha = .5) + 
  scale_color_gradient2(low = "#a50026", mid = "#f7f7f7", high = "#313695") +
  theme_dark()

ggplot(allSentiment, aes(sentenceOrder, name, color = fear)) + 
  geom_point(shape = "|", size = 5, alpha = .5) + 
  scale_color_gradient2(low = "#a50026", mid = "#f7f7f7", high = "#313695") +
  theme_dark()

ggplot(allSentiment, aes(sentenceOrder, name, color = sadness)) + 
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
