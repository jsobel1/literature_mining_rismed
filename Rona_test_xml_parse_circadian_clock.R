library(XML)
library(dplyr)
library(reshape)
library(textreuse)
library("xlsx")
library("lubridate")

wk=xmlParse(file = "Wikipedia-20180117070855.xml")

roorwk=xmlRoot(wk)

tab_wkidat_test=xmlToDataFrame(roorwk[[2]])

tab_wkidat_test$tl=as.numeric(as.character(sapply(tab_wkidat_test$text,nchar)))

write.xlsx(tab_wkidat_test,"test_table_file_CC.xlsx", sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

tab_wkidat_test=tab_wkidat_test[-which(tab_wkidat_test$tl<100),]

test_al=align_local(tab_wkidat_test$text[1], tab_wkidat_test$text[2],edit_mark = "°")

##### test with the token approach (tm package or RNewsflow , stringr, gutenbergr, mallet)
# see  latent Dirichlet allocation algorithm

#Robinson, David. 2017. broom: Convert Statistical Analysis Objects into Tidy Data Frames. https://CRAN.R-project.org/package=broom.
#Mimno, David. 2013. mallet: A Wrapper Around the Java Machine Learning Tool MALLET. https://cran.r-project.org/package=mallet.

####################################
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots

ref_data_wiki_dump=read.csv("doi_and_pubmed_citations.enwiki20150205.tsv",sep="\t",header=T)
clock_data=ref_data_wiki_dump[grep("Molecular clock",ref_data_wiki_dump$page_title),]

clock_data$ts=matrix(unlist(strsplit(as.character(clock_data$timestamp),"T")),byrow=T,ncol=2)[,1]
clock_data$ts=as.Date(clock_data$ts)
ggplot(dplyr::arrange(clock_data, ts), aes(x = ts)) + geom_bar()+scale_x_date()+ggtitle("Molecular clock papers introuced in wikipedia")

library(Rismed)
library(RISmed)

get_article_info=function(id){
res <- EUtilsSummary(id, type="esearch", db="pubmed", datetype='pdat')
GR=EUtilsGet(res)
if(length(DayPubmed(GR))==0){return(c(NA,NA))}
if(length(DayPubmed(GR))>1){return(c(NA,NA))}
AT=ArticleTitle(GR)
DP=DayPubmed(GR)
MP=MonthPubmed(GR)
YP=YearPubmed(GR)
if(nchar(DP)==1){
  DP=paste("0",DP,sep="")
}
if(nchar(MP)==1){
  MP=paste("0",MP,sep="")
}
return(c(paste(DP,MP,YP,sep=""),AT))
}

get_article_info(as.character(clock_data$id[29]))

test=sapply(as.character(clock_data$id),get_article_info)

df=matrix(unlist(test),ncol=2,byrow=T)
colnames(df)=c("date_pubmed","title")

clock_data=cbind(clock_data,df)

dp=matrix(unlist(test),ncol=2,byrow=T)[,1]

clock_data$duration=(as.duration(dmy(dp) %--% clock_data$ts))/ dyears(1)
clock_data$date_pubmed=as.Date(dmy(dp))

write.xlsx(clock_data,"publication_latency_MC.xlsx", sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


mean((as.duration(dmy(dp) %--% clock_data$ts))/ dyears(1),na.rm=T)

