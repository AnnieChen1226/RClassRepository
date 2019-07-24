library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(dplyr)

setwd("D:/GitHub Files/Extradition-Bill/PttData")

days.processed <- 5
data.processing <- NULL
date.processing <- NULL
pttData <- NULL

for (days.processed in 5 : 51) {
  print(days.processed)
  if((days.processed / 30) < 1) {
    month.d <- 'Jun'
  }else{
    month.d <- 'Jul'
  }
  date.d <- days.processed %% 30
  if(month.d == 'Jul') date.d = date.d + 1
  date.processing <- paste(month.d, date.d, sep = '_')
  data.processing <- paste0('D:/GitHub Files/Extradition-Bill/PttData/data_', month.d, '_', date.d, '.csv') 
  data.processing
  pttData <- read.csv(data.processing, header=T, sep=",")
  if(!file.exists(paste0('./titles/', date.processing,' title.txt'))){file.create(paste0('./titles/', date.processing,' title.txt'))}
  if(!file.exists(paste0('./texts/', date.processing,' texts.txt'))){file.create(paste0('./texts/', date.processing,' texts.txt'))}
  write(character(0), paste0('./titles/', date.processing,' title.txt'), append = FALSE)
  write(character(0), paste0('./texts/', date.processing,' texts.txt'), append = FALSE)
  doc <- function(d){
    write(d['titles'], paste0('./titles/', date.processing,' title.txt'), append = TRUE)
    write(d['texts'], paste0('./texts/', date.processing,' texts.txt'), append = TRUE)    
  }
  apply(pttData, 1, doc)
}

folder <- './titles/'
d.corpus <- Corpus(DirSource(folder))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
  })
xseg = worker()
new_user_word(xseg,'柯文哲',"n")
new_user_word(xseg,'柯P',"n")
new_user_word(xseg,'柯p',"n")
new_user_word(xseg,'蔡英文',"n")
new_user_word(xseg,'民進黨',"n")
new_user_word(xseg,'國民黨',"n")
new_user_word(xseg,'韓國瑜',"n")
new_user_word(xseg,'郭台銘',"n")
new_user_word(xseg,'中天',"n")
new_user_word(xseg,'柯市長',"n")
new_user_word(xseg,'蔡總統',"n")
new_user_word(xseg,'韓導',"n")
new_user_word(xseg,'韓草包',"n")
new_user_word(xseg,'韓總機',"n")
new_user_word(xseg,'高雄',"n")
new_user_word(xseg,'發大財',"n")
new_user_word(xseg,'蘇貞昌',"n")
new_user_word(xseg,'柯粉',"n")
new_user_word(xseg,'反送中',"n")
new_user_word(xseg,'逃犯條例',"n")
new_user_word(xseg,'問卦',"n")
new_user_word(xseg,'一個中國',"n")
new_user_word(xseg,'一國兩制',"n")
new_user_word(xseg,'出來',"n")
new_user_word(xseg,'罷工',"n")
new_user_word(xseg,'長榮',"n")
new_user_word(xseg,'台獨',"n")
new_user_word(xseg,'台獨分子',"n")
new_user_word(xseg,'台獨份子',"n")
new_user_word(xseg,'京阿尼',"n")
new_user_word(xseg,'東京',"n")
new_user_word(xseg,'日本',"n")
new_user_word(xseg,'當選',"n")
new_user_word(xseg,'大選',"n")
new_user_word(xseg,'總統大選',"n")
new_user_word(xseg,'2020',"n")
new_user_word(xseg,'送中',"n")
new_user_word(xseg,'立法會',"n")
new_user_word(xseg,'太陽花',"n")
new_user_word(xseg,'港警',"n")
new_user_word(xseg,'攻佔',"n")
new_user_word(xseg,'2020',"n")
new_user_word(xseg,'示威',"n")
new_user_word(xseg,'民調',"n")
new_user_word(xseg,'元朗',"n")
new_user_word(xseg,'打人',"n")
new_user_word(xseg,'黑道',"n")
new_user_word(xseg,'私菸',"n")
new_user_word(xseg,'白衣',"n")

tokenizer = function(d)
{
  unlist(segment(d[[1]], xseg) )
}
seg = lapply(d.corpus, tokenizer)
  
count_token = function(d)
{
   as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM.titles = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)

for(id in 2 : 47)
{
  print(id)
  TDM.titles = merge(TDM.titles, tokens[[id]], by="d", all = TRUE)
  names(TDM.titles) = c('d', colNames[1 : id])
}
TDM.titles[is.na(TDM.titles)] <- 0


folder <- 'texts/'
d.corpus <- Corpus(DirSource(folder))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
xseg = worker()
new_user_word(xseg,'柯文哲',"n")
new_user_word(xseg,'柯P',"n")
new_user_word(xseg,'柯p',"n")
new_user_word(xseg,'蔡英文',"n")
new_user_word(xseg,'民進黨',"n")
new_user_word(xseg,'國民黨',"n")
new_user_word(xseg,'韓國瑜',"n")
new_user_word(xseg,'郭台銘',"n")
new_user_word(xseg,'中天',"n")
new_user_word(xseg,'柯市長',"n")
new_user_word(xseg,'蔡總統',"n")
new_user_word(xseg,'韓導',"n")
new_user_word(xseg,'韓草包',"n")
new_user_word(xseg,'韓總機',"n")
new_user_word(xseg,'高雄',"n")
new_user_word(xseg,'發大財',"n")
new_user_word(xseg,'蘇貞昌',"n")
new_user_word(xseg,'柯粉',"n")
new_user_word(xseg,'反送中',"n")
new_user_word(xseg,'逃犯條例',"n")
new_user_word(xseg,'問卦',"n")
new_user_word(xseg,'一個中國',"n")
new_user_word(xseg,'一國兩制',"n")
new_user_word(xseg,'出來',"n")
new_user_word(xseg,'罷工',"n")
new_user_word(xseg,'長榮',"n")
new_user_word(xseg,'台獨',"n")
new_user_word(xseg,'台獨分子',"n")
new_user_word(xseg,'台獨份子',"n")
new_user_word(xseg,'京阿尼',"n")
new_user_word(xseg,'東京',"n")
new_user_word(xseg,'日本',"n")
new_user_word(xseg,'當選',"n")
new_user_word(xseg,'大選',"n")
new_user_word(xseg,'總統大選',"n")
new_user_word(xseg,'2020',"n")
new_user_word(xseg,'送中',"n")
new_user_word(xseg,'立法會',"n")
new_user_word(xseg,'太陽花',"n")
new_user_word(xseg,'港警',"n")
new_user_word(xseg,'攻佔',"n")
new_user_word(xseg,'2020',"n")
new_user_word(xseg,'示威',"n")
new_user_word(xseg,'民調',"n")
new_user_word(xseg,'元朗',"n")
new_user_word(xseg,'打人',"n")
new_user_word(xseg,'黑道',"n")
new_user_word(xseg,'私菸',"n")
new_user_word(xseg,'白衣',"n")

tokenizer = function(d)
{
  unlist(segment(d[[1]], xseg) )
}
seg = lapply(d.corpus, tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)
n = length(seg)
TDM.texts = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)

for(id in 2 : 47)
{
  print(id)
  TDM.texts = merge(TDM.texts, tokens[[id]], by="d", all = TRUE)
  names(TDM.texts) = c('d', colNames[1 : id])
}
TDM.texts[is.na(TDM.texts)] <- 0

setwd("D:/GitHub Files/Extradition-Bill/PttData/output")
write.csv(TDM.titles, 'TDM_titles.csv', row.names = TRUE)
write.csv(TDM.texts, 'TDM_texts.csv', row.names = TRUE)
