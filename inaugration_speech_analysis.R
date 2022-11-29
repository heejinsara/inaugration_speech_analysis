#1.데이터 전처리 과정
#트럼프 취임연설문 텍스트: 2017_Trump.txt
Trump<-scan(file='2017_Trump.txt',what='',quote=NULL)
Trump<-gsub('^[[:punct:]]+|[[:punct:]]+$','',tolower(Trump))
Trump<-Trump[nchar(Trump)>0]
head(Trump)
#바이든 취임연설문 텍스트: 2021_Biden.txt
Biden<-scan(file='2021_Biden.txt',what='',quote=NULL)
Biden<-gsub('^[[:punct:]]+|[[:punct:]]+$','',tolower(Biden))
Biden<-Biden[nchar(Biden)>0]
head(Biden)

#2.빈도분석
Trump.freq<-data.frame(sort(table(Trump),decreasing=T))
Biden.freq<-data.frame(sort(table(Biden),decreasing=T))
head(Trump.freq,15)
head(Biden.freq,15)
#stop words list에 포함된 어휘 제거해주기
#수업시간에 다룬 13_EnglishStopwords.txt와 다른 텍스트임!
#더 많은 어휘들이 stop words list에 포함되어 있음.
#stop words list 텍스트: stop.txt
stop<-scan(file='stop.txt',what='',quote=NULL)
filter.Trump<-Trump[!(Trump%in%stop)]
filter.Trump.freq<-data.frame(sort(table(filter.Trump),decreasing=T))
filter.Biden<-Biden[!(Biden%in%stop)]
filter.Biden.freq<-data.frame(sort(table(filter.Biden),decreasing=T))
head(filter.Trump.freq,15)
head(filter.Biden.freq,15)

head_t<-head(filter.Trump.freq$filter.Trump,15)
head_b<-head(filter.Biden.freq$filter.Biden,15)
intersect(head_t,head_b)

#3.bigrams
Trump_bi.grams<-paste(Trump[1:length(Trump)-1],Trump[2:length(Trump)])
Trump_bi.Freq<-data.frame(sort(table(Trump_bi.grams),decreasing=T))
Biden_bi.grams<-paste(Biden[1:length(Biden)-1],Biden[2:length(Biden)])
Biden_bi.Freq<-data.frame(sort(table(Biden_bi.grams),decreasing=T))

library(wordcloud)
wordcloud(Trump_bi.Freq$Trump_bi.grams,Trump.freq$Freq,
scale=c(4,1),min.freq=4,max.word=120,random.order=F,rot.per=0.4,
colors=brewer.pal(8,"Dark2"))

wordcloud(Biden_bi.Freq$Biden_bi.grams,Biden.freq$Freq,
scale=c(4,1),min.freq=4,max.word=120,random.order=F,rot.per=0.4,
colors=brewer.pal(8,"Dark2"))

#3.연어추출
#Trump에서 연어추출
#1단계:공기어 추출
node<-'america.*'
index<-grep(node,Trump)
span<-vector()
for(i in index)
{
span<-c(span,c((i-2):(i-1),(i+1):(i+2)))
}
span<-span[span>0&span<length(Trump)]
crc<-Trump[span]
#2단계:공기어 데이터프레임
Freq.span<-sort(table(crc),decreasing=T)
Freq.all<-table(Trump)
Freq.co<-data.frame(W1=vector(),W2=vector(),W1W2=vector(),N=vector())
Freq.co<-data.frame(t(sapply(names(Freq.span),
function(x){c(length(index),Freq.all[x],Freq.span[x],length(Trump))})))
colnames(Freq.co)<-c('W1','W2','W1W2','N')
#3단계:연어계산
collocates<-data.frame(Freq.co,
MI=log2((Freq.co$W1W2*Freq.co$N)/(Freq.co$W1*Freq.co$W2)))
MI.sort_t<-collocates[order(collocates$MI,decreasing=T),]
#Biden에서 연어추출
#1단계:공기어 추출
node<-'america.*'
index<-grep(node,Biden)
span<-vector()
for(i in index)
{
span<-c(span,c((i-2):(i-1),(i+1):(i+2)))
}
span<-span[span>0&span<length(Biden)]
crc<-Biden[span]
#2단계:공기어 데이터프레임
Freq.span<-sort(table(crc),decreasing=T)
Freq.all<-table(Biden)
Freq.co<-data.frame(W1=vector(),W2=vector(),W1W2=vector(),N=vector())
Freq.co<-data.frame(t(sapply(names(Freq.span),
function(x){c(length(index),Freq.all[x],Freq.span[x],length(Biden))})))
colnames(Freq.co)<-c('W1','W2','W1W2','N')
#3단계:연어계산
collocates<-data.frame(Freq.co,
MI=log2((Freq.co$W1W2*Freq.co$N)/(Freq.co$W1*Freq.co$W2)))
MI.sort_b<-collocates[order(collocates$MI,decreasing=T),]
head(MI.sort_t[MI.sort_t$W1W2>2,],10)
head(MI.sort_b[MI.sort_b$W1W2>2,],10)

#4.키워드 분석
TDM<-data.frame(words=vector())
for (i in list.files())
{
if (length(grep('[[:digit:]]',i))>0)
{
file<-scan(file=paste('./',i,sep='/'),what='',quote=NULL)
file<-gsub('^[[:punct:]]+|[[:punct:]]+$','',tolower(file))
file<-file[nchar(file)>0]
TDM<-merge(TDM,data.frame(table(file)),by.x='words',by.y='file',all=T)
colnames(TDM)[length(TDM)]<-substring(gsub('[.].+?$','',i),6)
}
}
TDM<-data.frame(row.names=TDM$words,TDM[2:length(TDM)])
TDM[is.na(TDM)]<-0
colnames(TDM)<-gsub('X','',colnames(TDM))
comparison.cloud(TDM,random.order=FALSE,
scale=c(4,1),rot.per=.4,max.words=150,
colors=brewer.pal(8,'Dark2'),title.size=1.1)

