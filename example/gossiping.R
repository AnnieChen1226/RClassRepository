library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(tibble)
library(dplyr)
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
page.number <- 100
page.index <- page.latest
page.processed <- 0
for (page.index in page.latest : (page.latest - page.number)){
  page.processed <- page.processed + 1
  link <- str_c(gossiping.url, "/index", page.index, ".html")
  cat("fetching link", page.processed, "/", page.number + 1, "...", link, "\n")
  links.article <- c(links.article,
                     gossiping.session %>%
                       jump_to(link) %>%
                       html_nodes(".title a") %>%
                       html_attr("href"))}
links.article <- unique(links.article)
temp.data <- data.frame(
  titles = NA,
  texts = NA,
  time.stamp = NA,
  pushes = NA)
tab <- data.frame(
  titles = NA,
  texts = NA,
  time.stamp = NA,
  pushes = NA)
progress <- 0
for (temp.link in links.article) {
  progress <- progress + 1
  article.link <- paste0(ptt.url, temp.link)
  cat('processing article', progress, '/', length(links.article), '...')
  print(article.link)
  temp.session <- gossiping.session %>%
    jump_to(article.link)
  print(temp.session)
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
  temp.data[progress,"pushes"] <- paste(temp.session %>%
    html_nodes(".push-tag") %>%
    html_text() %>%
    str_c(collapse = ""), "")
}
view(temp.data)

