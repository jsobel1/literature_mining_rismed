#install.packages("RISmed")
library("doBy")
library("RISmed")
library("doBy")
library("reshape")
library("plyr")
#require("plotrix") 
#library("ggplot2")

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

library("xlsx")

imapct_factor<-read.csv("JCR_impact_factor.csv",header=T)

search_topic <- 'Cyanobacteria [AND] circadian' 

year_start=1970
year_stop=2018

pubmed_data <- data.frame('ArticleID'=c(),
'Title'=c(),
'Journal'=c(),
'Cited'=c(),
'Year'=c(),
'country'=c(),
'PMID'=c(),
'Issue'=c(),
'Abstract'=c()
)

autors_table_clean<-data.frame(
  Author=c(),
  PMID=c(),
  Journal=c(),
  ImpactFactor=c(),
  Citation=c(),
  year=c())

get_pubmed_data=function(search_topic,year_start=2000,year_stop=2015){
  
for(y in year_start:year_stop){
	print(y)
  try(
search_query <- EUtilsSummary(search_topic, mindate=y, maxdate=y)
)
summary(search_query)

# see the ids of our returned query
if(length(QueryId(search_query))==0) next

# get actual data from PubMed
records<- EUtilsGet(search_query)
class(records)

mesh_vec=c()
pub_type=c()
for(i in 1:length(Author(records))){

mesh_vec=c(mesh_vec,paste(unlist(Mesh(records)[[i]][1]),sep=";",collapse=";"))
pub_type=c(pub_type,paste(unlist(PublicationType(records)[[i]]),sep=";",collapse=";"))
}

#PublicationType(records)

# store it
pubmed_data_load <- data.frame('ArticleID'=ArticleId(records),
'Title'=ArticleTitle(records),
'Journal'=MedlineTA(records),
'Cited'=Cited(records),
'Year'=YearPubmed(records),
'country'=Country(records),
'PMID'=PMID(records),
'Issue'=Issue(records),
'Abstract'=AbstractText(records)
)

pubmed_data_load=cbind(pubmed_data_load,ImpactFactor=imapct_factor$X5.Year.Impact.Factor[match(toupper(pubmed_data_load$Journal),imapct_factor$JCR.Abbreviated.Title)])

pubmed_data_load=cbind(pubmed_data_load,mesh_vec)
pubmed_data_load=cbind(pubmed_data_load,pub_type)

AuthorList=Author(records)
LastFirst <- sapply(AuthorList, function(x) paste(toupper(x$LastName), substr(toupper(x$ForeName),1, 1)))
AuthorList_all=paste(as.character(LastFirst), sep = ';')

pubmed_data_load=cbind(pubmed_data_load,AuthorList_all)

pubmed_data=rbind(pubmed_data,pubmed_data_load)

autors_table_clean_load=data.frame(Author=c(),PMID=c(),Journal=c(),ImpactFactor=c(),Citation=c(),year=c())
for(pub in 1:dim(pubmed_data_load)[1]){

  for(Aut in 1:length(unlist(LastFirst[pub]))){
    line_out=data.frame(Author=as.character(LastFirst[[pub]][Aut]),
                        PMID=as.character(pubmed_data_load$PMID[pub]),
                        Journal=as.character(pubmed_data_load$Journal[pub]),
                        ImpactFactor=as.numeric(as.character(pubmed_data_load$ImpactFactor[pub])),
                        Citation=as.numeric(as.character(pubmed_data_load$Cited[pub])),
                        year=as.numeric(as.character(pubmed_data_load$Year[pub])))
    
    autors_table_clean_load=rbind(autors_table_clean_load,line_out)
  }
}

autors_table_clean=rbind(autors_table_clean,unique(autors_table_clean_load))

}
return(list(autors_table_clean,pubmed_data))
}

pub_dat=get_pubmed_data(search_topic,year_start=1970,year_stop=2018)

autors_table_clean=pub_dat[[1]]
pubmed_data=pub_dat[[2]]

save(pubmed_data,file=paste("pubmed_data",search_topic,year_start,year_stop,"pubmed_data.Rdata",sep="_"))
save(autors_table_clean,file=paste("pubmed_authors",search_topic,year_start,year_stop,"pubmed_data.Rdata",sep="_"))

############################################################################
############################################################################

pubmed_data_sel=pubmed_data[which(pubmed_data$Cited>2),]

pubmed_data_sel=orderBy(~-Cited, pubmed_data_sel)

write.csv(pubmed_data_sel[1:100,], file=paste(search_topic,year_start,year_stop,"pubmed_data_sel_top100.csv",sep="_") ,row.names=FALSE,sep="\t")

write.xlsx(pubmed_data_sel[1:100,],paste(search_topic,year_start,year_stop,"pubmed_data_sel_top100.xlsx",sep="_"), sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

pubmed_data_s=orderBy(~-Cited, pubmed_data)
write.csv(pubmed_data, file=paste(search_topic,year_start,year_stop,"pubmed_data_sel_ALL.csv",sep="_") ,row.names=FALSE,sep="\t")

write.xlsx(pubmed_data,paste(search_topic,year_start,year_stop,"pubmed_data_sel_ALL.xlsx",sep="_"), sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

III=union(grep("Retra",pubmed_data$pub_type),grep("Erra",pubmed_data$pub_type))

write.xlsx(pubmed_data[III,],paste(search_topic,year_start,year_stop,"pubmed_data_Retracted_or_Erratum.xlsx",sep="_"), sheetName="Sheet1",  col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

############################################################################
############################################################################
############################################################################
