library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(tibble)
library(dplyr)

library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(tibble)
library(dplyr)

setwd("D:/GitHub Files/Extradition-Bill/PttData")

ptt.url <- "https://www.ptt.cc"
gossiping.url <- str_c(ptt.url, "/bbs/Gossiping")
gossiping.pre.session <- html_session(url = gossiping.url)
gossiping.pre.session
full_page <- read_html(gossiping.url)
full_page
form.18 <- gossiping.pre.session %>%
  html_node("form")%>%
  html_form()
form.18
gossiping.session <- submit_form(
  session = gossiping.pre.session,
  form = form.18,
  submit = "yes"
)

links.article <- read.table("links.csv", header = TRUE, sep = ",") %>%
  as.vector() %>%
  unlist() %>%
  as.character()

temp.time.parts <- NULL
temp.data <- data.frame(
  titles = NA,
  texts = NA,
  time.stamp = NA,
  month = NA,
  date = NA)
#tab <- data.frame(
#  titles = NA,
#  texts = NA,
#  time.stamp = NA)
progress <- 0
abs.progress <- 0
from <- 107925
to <- 110341
total <- to - from + 1
for (i in from : to){
  progress <- progress + 1
  abs.progress <- abs.progress + 1
  article.link <- paste0(ptt.url, links.article[i])
  cat('processing article', abs.progress, '/', total, '...')
  print(article.link)
  temp.session <- gossiping.session %>%
    jump_to(article.link)
  #  print(temp.session)
  temp.data[progress,"titles"] <- paste(temp.session %>%
                                          html_nodes(".article-metaline-right+ .article-metaline .article-meta-value") %>%
                                          html_text() %>%
                                          str_c(collapse = ""), "")
  temp.data[progress,'texts'] <- paste(temp.session %>%
                                         html_nodes(xpath = '//*[@id="main-content"]/text()') %>%
                                         html_text() %>%
                                         str_c(collapse = ""), "")
  temp.data[progress,"time.stamp"] <- paste(temp.session %>%
                                              html_nodes(".article-metaline+ .article-metaline .article-meta-value") %>%
                                              html_text() %>%
                                              str_c(collapse = ""), "")
  #  temp.data[progress,"pushes"] <- paste(temp.session %>%
  #                                          html_nodes(".push-tag") %>%
  #                                          html_text() %>%
  #                                          str_c(collapse = ""), "")
  temp.time.parts <- as.character(temp.data[progress, "time.stamp"]) %>%
    strsplit(split = " ") %>%
    unlist()
  temp.data[progress, "date"] <- temp.time.parts[4]
  temp.data[progress, "month"] <- temp.time.parts[2]
}
error.row <- NULL
need.erase <- FALSE
for (i in 1:nrow(temp.data)) {
  if(temp.data[i, 1] == " "){
    error.row <- c(error.row, i)
    need.erase = TRUE
  }
}
if(need.erase){
  temp.data <- temp.data[-as.vector(error.row), ]
}
filename <- paste0('data_Jul_22.csv')
write.csv(temp.data, filename, row.names = FALSE)