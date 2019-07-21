library(readxl)
library(NLP)
library(httr)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(xml2)
library(XML)
library(rvest)
library(bitops)
library(RCurl)

xdir = "D:/Annie/Documents/GitHub/GitHubForRClass/RClassRepository/example/test"
fnames = list.files(path=xdir,pattern="*.csv")

xtext = NULL
for (f in fnames) {
  fpath = paste(xdir,f,sep="/")
  t0 = readLines(fpath)
  t1 = paste(t0,collapse=" ")
  xtext = c(xtext,t1)
}

library(jiebaR)

xseg = worker() # ¤@¯ëÂ_µü
xtext2 = NULL
for (i in 1:length(xtext)){
  t0 = xtext[i]
  t1 = xseg <= t0
  xtext2 = c(xtext2,paste0(t1,collapse=" "))
}

require(dplyr)

text_df = data_frame(doc_id = 1:length(xtext2), text = xtext2)
head(text_df)

library(tidytext)
