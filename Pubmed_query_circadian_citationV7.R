#install.packages("RISmed")
library("doBy")
library("RISmed")
library("doBy")
library("reshape")
library("plyr")
require("plotrix") 
library("ggplot2")

imapct_factor<-read.csv("JCR_impact_factor.csv",header=T)

search_topic <- 'Circadian clock'  #'circadian [and] modelling' #'molecular [AND] Clock'

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
#search_query <- EUtilsSummary(search_topic, mindate=2000, maxdate=2015)

summary(search_query)

# see the ids of our returned query
if(length(QueryId(search_query))==0) next

# get actual data from PubMed
records<- EUtilsGet(search_query)
class(records)

mesh_vec=c()
pub_type=c()
for(i in 1:length(Author(records))){
#print(Author(records)[[i]]$LastName[1])	
#print(paste(unlist(Mesh(records)[[i]][1]),sep=";",collapse=";"))
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
  #print(as.character(pubmed_data_load$PMID[pub]))
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
#write.table(autors_table_clean, file = "autors_table_clean_Pubmed_molecularClock2012_2015.csv", append = F, quote = FALSE, sep = "\t", na = "NA", row.names = F)
#write.table(pubmed_data, file = "Pubmed_molecularClock_2000_2015.csv", append = T, quote = FALSE, sep = "\t", na = "-", row.names = T)
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

load("pubmed_data_Circadian clock_1970_2018_pubmed_data.Rdata")
load("pubmed_Authors_Circadian clock_1970_2018_pubmed_data.Rdata")

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

library("xlsx")


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

#search_topic=gsub("\\[and\\]","",search_topic)

### Nb occurence of Mesh words ####
pdf(paste("pubmed_data",search_topic,year_start,year_stop,"All_viz.pdf",sep="_"))
par(mar=c(15,5 , 4, 2))
barplot(rev(sort(table(unlist(strsplit(as.character(pubmed_data$mesh_vec),";")))))[1:40],
        horiz=F,las=2,col="blue",
        cex.lab=0.4,cex.names=0.7,main=paste("Mesh Words",search_topic,year_start,year_stop),ylab="Sum mesh words")

barplot(rev(sort(table(as.character(pubmed_data$Journal))))[1:40],
        horiz=F,las=2,col="blue",
        cex.lab=0.4,cex.names=0.7,main=paste("Journal",search_topic,year_start,year_stop),ylab="Sum Publilcations")

plot(table(as.character(pubmed_data$Year)),
        las=2,col="blue",type="l",
        cex.lab=1,cex.names=0.7,
     main=paste("pub nb per year",search_topic,year_start,year_stop),ylab="Sum Publilcations")


plot(table(pubmed_data$Cited),col="darkgreen",
     main=paste("Citations distribution",search_topic,year_start,year_stop),
     ylab="Publilcations")
library(tm)

mesh_corpus=VCorpus(VectorSource(unlist(strsplit(as.character(pubmed_data$mesh_vec),";"))))

mesh_corpus=tm_map(mesh_corpus, removePunctuation)

library("wordcloud")
#wordcloud(mesh_corpus)

 tdm <- TermDocumentMatrix(mesh_corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    pal=brewer.pal(8, "Dark2")
    wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,,.1,pal)
#dev.off()

# #################################################### 

####### Statistics and Viz ####

ddata <- ddply(autors_table_clean, c("Author"), summarise,
               N    = sum(!is.na(Citation)),
               sum_C  = sum(Citation),
               sum_IF =sum(ImpactFactor,na.rm =T),
               min_year=min(year),
               mean = mean(Citation, na.rm=TRUE),
               sd   = sd(Citation, na.rm=TRUE),
               se   = sd / sqrt(N)
)
    
#write.csv(ddata, file = "Network_author_attribute.csv",row.names=FALSE)

ddata_sub= subset(ddata, N>2 & sum_C>10,select=c("Author","sum_C"))
ddata_sub=orderBy(~-sum_C, ddata_sub)

par(mar=c(5, 10, 4, 2))
barplot(rev(as.numeric(as.character(ddata_sub$sum_C[1:40]))),names.arg=rev(ddata_sub$Author[1:40]),
        las=2,horiz=TRUE,col="darkblue",xlab="Citation sum",cex.names=.8,main="Top 40 Authors \n in molecular clock papers")

ddata_sub= subset(ddata, N>2 & sum_IF>0,select=c("Author","sum_IF"))
ddata_sub=orderBy(~-sum_IF, ddata_sub)

par(mar=c(5, 10, 4, 2))
barplot(rev(as.numeric(as.character(ddata_sub$sum_IF[1:40]))),names.arg=rev(ddata_sub$Author[1:40]),
        las=2,horiz=TRUE,col="darkblue",xlab="IF sum",cex.names=.8,main="Top 40 Authors \n in molecular clock papers")

ddata_sub= subset(ddata, N>2,select=c("Author","N"))
ddata_sub=orderBy(~-N, ddata_sub)

par(mar=c(5, 10, 4, 2))
barplot(rev(as.numeric(as.character(ddata_sub$N[1:40]))),names.arg=rev(ddata_sub$Author[1:40]),
        las=2,horiz=TRUE,col="darkblue",xlab="Publication sum",cex.names=.8,main="Top 40 Authors \n in molecular clock papers")


#ggplot(ddata_sub[1:40,],aes(x=factor(Author),y=as.numeric(as.character(sum))))+geom_bar(stat="identity")+
#theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+theme_bw()

#ddata_sub <- within(ddata_sub, 
 #                  sum<- factor(sum, 
  #                                    levels=names(sort(table(sum), 
   #                                                     decreasing=TRUE))))
## plot
#ggplot(data=ddata_sub[1:40,], aes(x=Author, y=as.numeric(as.character(sum)))) + 
 #              geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+theme_bw()


#gap.barplot(rev(sort(table(pubmed_data$Cited))),gap=c(100,400),xlab="Index",ytics=c(0,20,40,60,80,300,350,400,450,500,550),
#ylab="Group values",main="Barplot with gap")

p <- ggplot(pubmed_data_sel, aes(factor(Journal), Cited))
p + geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()



h <- qplot(Cited, data=pubmed_data_sel, geom="histogram")
print(h)

ggplot(pubmed_data_sel, aes(x=ImpactFactor, y=Cited)) +
    geom_point(aes(colour=factor(Journal))) +geom_text(aes(label=Journal), size=3,vjust=-1.5)+theme_bw()
    

cdata <- ddply(pubmed_data_sel, c("ImpactFactor", "Journal"), summarise,
               N    = sum(!is.na(Cited)),
               mean = mean(Cited, na.rm=TRUE),
               sd   = sd(Cited, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata

# ggplot(cdata[which(cdata$N>1),],aes(x=factor(Journal),y=as.numeric(as.character(N))))+geom_bar(stat="identity")+
# theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+theme_bw()

cdata$N=as.numeric(as.character(cdata$N))
cdata=orderBy(~-N, cdata)
par(mar=c(5, 10, 4, 2))
barplot(rev(as.numeric(as.character(cdata$N))[1:40]),
        names.arg=rev(cdata$Journal[1:40]),
        las=2,horiz=TRUE,col="darkblue",
        xlab="Nb of articles",
        cex.names=.6,
        main="Top 40 journals publication score \n for molecular clock papers")

# cdata <- within(cdata, 
#                    N <- factor(N, 
#                                       levels=names(sort(table(N), 
#                                                         decreasing=TRUE))))
# ggplot(data=cdata,aes(x=as.character(Journal),y=as.numeric(as.character(N)))) + geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+theme_bw()
# 
#                
cdata$Journal <- reorder(as.factor(cdata$Journal), cdata$N)
levels(cdata$Journal)
attributes(cdata$Journal)
md <- melt(cdata, id=(c("Journal","N")))
ggplot(data=md, aes(x=Journal, y=N)) + 
               geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+theme_bw()
               
dev.off()  

             
#####Network Analysis #### 

get_network_PMID=function(PMID_1,threshold_citation=2,name="output_network.txt"){
	autors_table_clean_sub=autors_table_clean[which(as.character(autors_table_clean$PMID)==PMID_1),c("Author","PMID","Citation","ImpactFactor")]
	autors_table_clean_sub$Author=as.character(autors_table_clean_sub$Author)
	autors_table_clean_sub$PMID=as.character(autors_table_clean_sub$PMID)
	autors_table_clean_sub$Citation=as.numeric(as.character(autors_table_clean_sub$Citation))
	autors_table_clean_sub$ImpactFactor=as.numeric(as.character(autors_table_clean_sub$ImpactFactor))
	
	for(ind1 in 1:length(autors_table_clean_sub$Author)){
		for(ind2 in ind1:length(autors_table_clean_sub$Author)){
			if(ind2==ind1) next
			if(autors_table_clean_sub$Citation[ind1]<threshold_citation) next
			line=paste(c(autors_table_clean_sub$Author[ind1],autors_table_clean_sub$Author[ind2],autors_table_clean_sub$Citation[ind1],autors_table_clean_sub$ImpactFactor[ind1],    PMID_1),sep="\t",collapse="\t")
			#print(line)
			cat(line, file=name, append=TRUE, sep = "\n")
					}
	}	
}

data_Naef_paper=pubmed_data[grep("NAEF F", as.character(pubmed_data$AuthorList_all)),]

data_Naef_paper=as.character(pubmed_data[grep("NAEF F", as.character(pubmed_data$AuthorList_all)),]$PMID)

get_network_PMID(data_Naef_paper[1])

for(Id in 1:length(unique(data_Naef_paper))){
  get_network_PMID(as.character(unique(data_Naef_paper)[Id]))
}

ddata_sub= subset(ddata, N>2 & sum_C>10,select=c("Author","N"))

ddata_sub=orderBy(~-N, ddata_sub)
#ddata_sub$Author[1:40]

get_athors_bibl=function(Author_name){
  #pubmed_data[grep(Author_name, as.character(pubmed_data$AuthorList_all)),]
  data_paper=as.character(pubmed_data[grep(Author_name, as.character(pubmed_data$AuthorList_all)),]$PMID)
  return(data_paper)
}

for(aut in 1:40){
data_paper=get_athors_bibl(ddata_sub$Author[aut])
for(Id in 1:length(unique(data_paper))){
  get_network_PMID(as.character(unique(data_paper)[Id]),name="outputnetworkTop40.txt")
}
}

network=read.csv("outputnetwork.txt",sep="\t",header=F)
# 
# install.packages("igraph")
# install.packages("network")
# install.packages("sna") 
# install.packages("ndtv")
# library (RCytoscape)
#   cy = CytoscapeConnection ()
#   pluginVersion (cy)


############################
############################TRASH
# melt(pubmed_data_sel,  id=c("Journal", "Cited"))
# pie(sort(table(pubmed_data_sel$country)))
# pie(sort(table(pubmed_data_sel$Journal)))
# plot(pubmed_data_sel$ImpactFactor,pubmed_data_sel$Cited,pch=19,cex=0.5)
# boxplot(pubmed_data_sel$Cited~pubmed_data_sel$Journal,las=2,horiz=F)
#head(pubmed_data,5)
#}
#pubmed_all=read.table("Pubmed_molecularClock.csv",sep="\t",header=T)
#"ArticleID","Title","Cited","Year","country","PMID"
#head(orderBy(~ -Cited , pubmed_all),60)
#AbstractText(records)
#AbstractText(records) 
#Author(records)
#CollectiveName(records) 
#Mesh(records)
#PMID(records)
#pie(sort(table(Country(records))))
#head(pubmed_data,1)
#pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
#pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
# see what we have
#str(pubmed_data)
# library(plyr)
# df_autors=ldply(AuthorList, data.frame)
# colnames(df_autors)[1]="ID"
# df_autors$ID=as.numeric(as.character(df_autors[,1]))
# library(doBy)
# df_autors=orderBy(~ID, df_autors)
# #sort(as.numeric(as.character(df_autors$.id)))
# df_autors$Journal=pubmed_data$Journal[df_autors$ID]
# df_autors$Cited=pubmed_data$Cited[df_autors$ID]
# df_autors$ImpactFactor=pubmed_data$ImpactFactor[df_autors$ID]
# df_autors$PMID=pubmed_data$PMID[df_autors$ID]
# df_autors$AUT=paste(toupper(df_autors$LastName),substr(toupper(df_autors$ForeName), 1, 1),sep=" ")
# write.table(df_autors[which(df_autors$Cited>5),], file = "df_autors.csv", append = F, quote = FALSE, sep = "\t", na = "-", row.names = F)
# df_autors_sub= subset(df_autors, as.numeric(as.character(Cited))>5,select=c("AUT","PMID"))
# df_autors_sub= subset(df_autors, as.numeric(as.character(Cited))>5 & as.numeric(as.character(ImpactFactor))>5,select=c("AUT","PMID"))
# library(reshape2)
# #m <- dcast(df_autors_sub,PMID~AUT)
# m <-dcast(network,PMID~AUT)
# m=t(data.matrix(m))
# #m[is.na(m)]=0
# d = dist(m, method = "binary")
# hc = hclust(d, method="ward")
# plot(hc)
# cluster.means = aggregate(m,by=list(cutree(hc, k = 6)), mean)
# heatmap(cor(t(m)))
# cor(t(table(df_autors_sub)))
# sort(table(df_autors_sub$AUT))
#cast(pubmed_data_sel,  Cited~Journal , sum)