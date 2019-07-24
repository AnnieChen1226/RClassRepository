#讀檔
library(readxl)

titleData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_titles.csv")
txtData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_texts.csv")

library(data.table)
titleData1<-setDT(titleData, key='d')[.("香港")]
titleData2<-setDT(titleData, key='d')[.("反送中")]
titleData3<-setDT(titleData, key='d')[.("逃犯條例")]
titleData4<-setDT(titleData, key='d')[.("共產黨")]
titleData5<-setDT(titleData, key='d')[.("遊行")]
titleData6<-setDT(titleData, key='d')[.("中共")]
titleData7<-setDT(titleData, key='d')[.("元朗")]
titleData8<-setDT(titleData, key='d')[.("立法會")]
titleDataFinal<-rbind(titleData1,titleData2,titleData3,titleData4,titleData5,titleData6,titleData7,titleData8)
write.csv(titleDataFinal, file = "titleDataFinal.csv")

txtData1<-setDT(txtData, key='d')[.("香港")]
txtData2<-setDT(txtData, key='d')[.("反送中")]
txtData3<-setDT(txtData, key='d')[.("逃犯條例")]
txtData4<-setDT(txtData, key='d')[.("共產黨")]
txtData5<-setDT(txtData, key='d')[.("遊行")]
txtData6<-setDT(txtData, key='d')[.("中共")]
txtData7<-setDT(txtData, key='d')[.("元朗")]
txtData8<-setDT(txtData, key='d')[.("立法會")]
txtDataFinal<-rbind(txtData1,txtData2,txtData3,txtData4,txtData5,txtData6,txtData7,txtData8)
write.csv(txtDataFinal, file = "txtDataFinal.csv")

library(NLP)
library(ggplot2)
library(varhandle)

#讀檔
#讀檔
library(readxl)

titleData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_titles.csv")
txtData<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//TDM_texts.csv")

library(data.table)
titleData1<-setDT(titleData, key='d')[.("香港")]
titleData2<-setDT(titleData, key='d')[.("反送中")]
titleData3<-setDT(titleData, key='d')[.("逃犯條例")]
titleData4<-setDT(titleData, key='d')[.("共產黨")]
titleData5<-setDT(titleData, key='d')[.("遊行")]
titleData6<-setDT(titleData, key='d')[.("中共")]
titleData7<-setDT(titleData, key='d')[.("元朗")]
titleData8<-setDT(titleData, key='d')[.("立法會")]
titleDataFinal<-rbind(titleData1,titleData2,titleData3,titleData4,titleData5,titleData6,titleData7,titleData8)
write.csv(titleDataFinal, file = "titleDataFinal.csv")

txtData1<-setDT(txtData, key='d')[.("香港")]
txtData2<-setDT(txtData, key='d')[.("反送中")]
txtData3<-setDT(txtData, key='d')[.("逃犯條例")]
txtData4<-setDT(txtData, key='d')[.("共產黨")]
txtData5<-setDT(txtData, key='d')[.("遊行")]
txtData6<-setDT(txtData, key='d')[.("中共")]
txtData7<-setDT(txtData, key='d')[.("元朗")]
txtData8<-setDT(txtData, key='d')[.("立法會")]
txtDataFinal<-rbind(txtData1,txtData2,txtData3,txtData4,txtData5,txtData6,txtData7,txtData8)
write.csv(txtDataFinal, file = "txtDataFinal.csv")

library(NLP)
library(ggplot2)
library(varhandle)
library(NLP)
library(ggplot2)
library(varhandle)

library(NLP)
library(ggplot2)
library(varhandle)

#讀檔
titleDate<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//titleDate.csv",sep = ",", as.is = TRUE)
txtDate<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//txtDate.csv",sep = ",", as.is = TRUE)

#title 

titleDate$t <- factor(titleDate$t, levels = c("香港","","反送中","逃犯條例","共產黨","遊行","中共","元朗","立法會"), ordered = TRUE )
titleDate$date <- as.Date(titleDate$date,format="%Y-%m-%d")

Graph_title<-ggplot(data=titleDate,aes(x=date,y=qua,group=t))+
  geom_line(aes(color=t))+
  geom_point(aes(color=t))

Graph_plus_title<-Graph_title+scale_color_manual(values=c("#28004d","#ae0000", "#006030","#9f5000","#613030","#ffa042","#5b5b5b","#eac100"))+ scale_x_date(date_minor_breaks = "3 day")+scale_y_continuous(trans = 'sqrt') 
print(Graph_plus_title)

#txt


txtDate$t <- factor(txtDate$t, levels = c("香港","","反送中","逃犯條例","共產黨","遊行","中共","元朗","立法會"), ordered = TRUE )
txtDate$date <- as.Date(txtDate$date,format="%Y-%m-%d")


Graph_txt<-ggplot(data=txtDate,aes(x=date,y=qua,group=t))+
  geom_line(aes(color=t))+
  geom_point(aes(color=t))

Graph_plus_txt<-Graph_txt+scale_color_manual(values=c("#28004d","#ae0000", "#006030","#9f5000","#613030","#ffa042","#5b5b5b","#eac100"))+ scale_x_date(date_minor_breaks="3 day ")+scale_y_continuous(trans = 'sqrt') 
print(Graph_plus_txt)


#讀檔
simpletitleDate<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//titleDateSimple.csv",sep = ",", as.is = TRUE)
simpletxtDate<-read.csv("D://Annie//Documents//GitHub//GitHubForRClass//Extradition-Bill//PttData//output//txtDateSimple.csv",sep = ",", as.is = TRUE)

#title 

simpletitleDate$t <- factor(simpletitleDate$t, levels = c("香港","反送中","逃犯條例","共產黨","元朗"), ordered = TRUE )
simpletitleDate$date <- as.Date(simpletitleDate$date,format="%Y-%m-%d")

Graph_title_S<-ggplot(data=simpletitleDate,aes(x=date,y=qua,group=t))+
  geom_line(aes(color=t))+
  geom_point(aes(color=t))

Graph_plus_title_S<-Graph_title_S+scale_color_manual(values=c("#28004d","#ae0000", "#006030","#9f5000","#613030"))+ scale_x_date(date_minor_breaks = "3 day")+scale_y_continuous(trans = 'sqrt') 
print(Graph_plus_title_S)

#txt


simpletxtDate$t <- factor(simpletxtDate$t, levels = c("香港","","反送中","逃犯條例","共產黨","元朗"), ordered = TRUE )
simpletxtDate$date <- as.Date(simpletxtDate$date,format="%Y-%m-%d")


Graph_txt_S<-ggplot(data=simpletxtDate,aes(x=date,y=qua,group=t))+
  geom_line(aes(color=t))+
  geom_point(aes(color=t))

Graph_plus_txt_S<-Graph_txt_S+scale_color_manual(values=c("#28004d","#ae0000", "#006030","#9f5000","#613030"))+ scale_x_date(date_minor_breaks="3 day ")+scale_y_continuous(trans = 'sqrt') 
print(Graph_plus_txt_S)
