##########################
### Parsing Transcript ###
##########################

library(dplyr)

# transLines = data.frame(comments = readLines("data/transcriptLines.txt", encoding = "UTF-8"), 
#                         stringsAsFactors = FALSE)

transLines = read.csv("data/transcriptLines.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

senatorParty = transLines$comments[which(grepl("^SPEAKERS", transLines$comments))[[1]]:(which(grepl("^WITNESSES", transLines$comments))[[1]] - 1)]

senatorParty = data.frame(name = stringr::str_extract(senatorParty, "[?>A-Z]+,"),
                        party = stringr::str_extract(senatorParty, "(?>[RD])-")) %>% 
  mutate(name = stringr::str_replace(.$name, ",", ""),
         party = stringr::str_replace(.$party, "-", "")) %>% 
  na.omit()

speakers = transLines$comments[which(grepl("^SPEAKERS", transLines$comments))[[1]]:(which(grepl("^WITNESSES", transLines$comments))[[1]] - 1)]

speakers = stringr::str_extract(speakers, "[?>A-Z]+,")

speakers = gsub(",", "", speakers)

witnesses = transLines$comments[which(grepl("^WITNESSES", transLines$comments))[[1]]:(which(grepl("^\\[\\*\\]", transLines$comments))[[1]] - 1)]

witnesses = stringr::str_extract(witnesses, "[?>A-Z]+,")

witnesses = gsub(",", "", witnesses)

dropIntroRows = 1:(which(grepl("^\\[\\*\\]", transLines$comments))[[1]] - 1)

dropBreakRows = which(grepl("SENATE JUDICIARY COMMITTEE HEARING", transLines$comments))[[2]]:which(grepl("^WITNESSES", transLines$comments))[[2]]

dropEndLines = (which(grepl("Hearing adjourned.", transLines$comments)) + 1):nrow(transLines)

transLines = data.frame(comments = transLines[-c(dropIntroRows, dropBreakRows, dropEndLines), ])

transLines$comments = gsub("^\\[\\*\\]\\s", "", transLines$comments)

transLines = transLines %>% 
  mutate(name = ifelse(grepl("^[A-Z]+:", comments), 
                       stringr::str_extract(comments, "^[A-Z]+:"), 
                       ifelse(grepl("\\(UNKNOWN\\): ", comments), "UNKNOWN", "")), 
         name = stringr::str_replace(name, ":", ""), 
         role = ifelse(name %in% speakers, "speaker", 
                       ifelse(name %in% witnesses, "witness", "")), 
         comments = stringr::str_replace_all(comments, "^\\(*[A-Z]+\\)*:\\s", ""))

transLines$commentSpeakerID[transLines$name != ""] = 1:nrow(transLines)

transLines = transLines %>% 
  tidyr::fill(., commentSpeakerID, .direction = "down") %>% 
  group_by(commentSpeakerID) %>% 
  mutate(commentFull = paste(comments, collapse = " ")) %>% 
  slice(., 1L) %>% 
  ungroup() %>% 
  select(name, role, commentFull)

transLines = transLines %>% 
  mutate(nameLead = lag(name), 
         nameLead = ifelse(is.na(nameLead), "Empty", nameLead),
         collapser = ifelse(name == nameLead, NA, 1:nrow(.))) %>% 
  select(-nameLead) %>% 
  tidyr::fill(., collapser, .direction = "down") %>% 
  group_by(collapser) %>% 
  mutate(commentFull = paste(commentFull, collapse = " ")) %>% 
  slice(., 1L) %>% 
  ungroup() %>% 
  select(name, role, commentFull) %>% 
  distinct()


transLines = left_join(transLines, senatorParty, by = "name")

write.csv(transLines, "data/parsedTranscript.csv", row.names = FALSE, 
          fileEncoding = "UTF-8")

transLines %>% select(-role, - party) %>% write.csv(., "data/qdapTranscript.csv", row.names = FALSE, 
                                                    fileEncoding = "UTF-8")
