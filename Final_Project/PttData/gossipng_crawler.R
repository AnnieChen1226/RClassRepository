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
gossiping.session
page.latest <- gossiping.session %>%
  html_node(".wide:nth-child(2)") %>%
  html_attr("href") %>%
  str_extract("[0-9]+")%>%
  as.numeric()
page.latest
links.article <- NULL
#page.number <- 130 33559
fetch.to <- 33448
page.index <- fetch.to
page.required <- page.latest - fetch.to
page.processed <- 0
for (page.index in fetch.to: page.latest){
  page.processed <- page.processed + 1
  link <- str_c(gossiping.url, "/index", page.index, ".html")
  cat("fetching link", page.processed, "/", page.required + 1, "...", link, "\n")
  links.article <- c(links.article,
                     gossiping.session %>%
                       jump_to(link) %>%
                       html_nodes(".title a") %>%
                       html_attr("href"))}
links.article <- unique(links.article)
write.csv(links.article, links, row.names = FALSE)

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
for (i in 1 : length(links.article)){
  progress <- progress + 1
  abs.progress <- abs.progress + 1
  article.link <- paste0(ptt.url, links.article[i])
  cat('processing article', abs.progress, '/', length(links.article), '...')
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
  if(progress == 1) next
  if(is.na(temp.data[as.character(progress), "date"])) next
  if(is.na(temp.data[as.character(progress - 1), "date"])) next
  if((temp.data[as.character(progress - 1), "date"] != temp.data[as.character(progress - 1), "date"]) & (progress > 10)){
    error.row = NULL
    need.erase = FALSE
    saving.data <- temp.data[- as.character(progress), ]
    for (i in 1 : nrow(saving.data)) {
      if(saving.data[i, 1] == " "){
        error.row <- c(error.row, i)
        need.erase = TRUE
      }
    }
    filename <- paste0('data_', saving.data[2, 'month'], '_', saving.data[2, 'date'], '.csv')
    if(need.erase){
      saving.data <- saving.data[-as.vector(error.row), ]
    }
    write.csv(saving.data, filename, row.names = FALSE)
#    save(saving.data, file = filename)
    temp.data <- temp.data[as.character(progress), ]
    rownames(temp.data) <- c(1)
    progress <- 1
    print("data saved!")
  }
}
for (i in 1:nrow(temp.data)) {
  if(temp.data[i, 1] == " "){
    error.row <- c(error.row, i)
    need.erase = TRUE
  }
}
if(need.erase){
  temp.data <- temp.data[-as.vector(error.row), ]
}
filename <- paste0('data_', temp.data[1, 'month'], '_', temp.data[1, 'date'], '.RData')
save(temp.data, file = filename)