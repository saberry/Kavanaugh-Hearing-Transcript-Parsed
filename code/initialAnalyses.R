#######################
### Initial Goofing ###
#######################

library(ca)

library(dplyr)

library(ggplot2)

library(sentimentr)

library(qdap)

kavTrans = read.transcript("data/qdapTranscript.csv", 
                           col.names = c("name", "commentFull"), 
                           text.var = "commentFull", header = TRUE)

allData = read.csv("data/parsedTranscript.csv", stringsAsFactors = FALSE)

genderData = read.csv("data/genderData.csv")

allData = allData %>% 
  select(name, party) %>% 
  distinct()

kavTrans = left_join(kavTrans, allData, by = "name") %>% 
  left_join(., genderData, by = "name")

kavTrans$party[is.na(kavTrans$party)] = "witness"

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

allSentiment = sentimentr::sentiment(kavTrans$commentFull, hash_sentiment_vadar)

allSentiment = kavTrans %>% 
  select(name, party, rowID) %>% 
  left_join(., allSentiment, by = c("rowID" = "element_id")) %>% 
  mutate(sentenceOrder = 1:nrow(.))

ggplot(allSentiment, aes(sentenceOrder, name, color = sentiment)) + 
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
