library(httr)
library(jsonlite)
library(dplyr)

getwd()
setwd("D:/Annie/Documents/GitHub/GitHubForRClass/RClassRepository/example/test")
options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")
dcardurl <- 'https://www.dcard.tw/_api/forums/'
board <- 'trending'
mainurl <- paste0(dcardurl,board,'/posts?popular=false')
resdata <- fromJSON(content(GET(mainurl), "text"))
resdataD<-as.data.frame(resdata)%>%
  select(title,createdAt)

head(resdata[,1:2])
tail(resdata[,1:2])

end <- resdata$id[length(resdata$id)]
end

n <- 36000
page <- (36000/30)-1
pageread<-2
resdata1<-NULL
for(i in 2:page){
  Sys.sleep(1)
  url <- paste0(mainurl,"&before=",end,"&limit=30")
  tmpres <- fromJSON(content(GET(url), "text"))
  tmpresD<-as.data.frame(tmpres)%>%
    select(title,createdAt)
  end <- tmpres$id[length(tmpres$id)]
  #resdata1<-do.call(rbind,resdata,tmpres)
  resdata1 <-rbind(tmpresD,resdata1)
  
  pageread=pageread+1
  print(pageread)
  if(pageread==20)
  {
    pageread=0
    fileName=paste0("./","DcardData",as.character(i),".csv")
    file.create(fileName)
    write.csv(resdata1, file = fileName) 
    resdata1=NULL
  }
  
}
