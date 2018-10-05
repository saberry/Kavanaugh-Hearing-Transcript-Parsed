###########################
### Pull Raw Transcript ###
###########################

library(rvest)

rawTrans = read_html("https://www.washingtonpost.com/news/national/wp/2018/09/27/kavanaugh-hearing-transcript/?noredirect=on&utm_term=.3c81a9c94798") %>% 
  html_nodes("p") %>% 
  html_text()

writeLines(rawTrans, "transcriptLines.txt")
