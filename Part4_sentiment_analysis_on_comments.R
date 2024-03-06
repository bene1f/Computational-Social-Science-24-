#sentiment analyisis with comments not with words 
#transgender
### 2.8 **qdap polarity**
library(qdap)
library(ggplot2)
library(ggthemes)

#### polarity

pol<-polarity(transgender$textNoEmoji)
transplot2= ggplot(pol$all, aes(x=polarity ,
                                y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Transgender") 

data5 <- transgender
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(transplot2)

#lgbt
library(qdap)
library(ggplot2)
library(ggthemes)
#### polarity
pol<-polarity(lgbt$textNoEmoji)
lgbtplot2=ggplot(pol$all, aes(x=polarity ,
                              y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "LGBT") 

data5 <- lgbt
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(lgbtplot2)

#racism 
#### polarity
pol<-polarity(blackpeople$textNoEmoji)
racplot2=ggplot(pol$all, aes(x=polarity ,
                             y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Black People") 

data5 <- blackpeople
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(racplot2)

#gender

pol<-polarity(gender$textNoEmoji)
genplot2=ggplot(pol$all, aes(x=polarity ,
                             y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Women") 

data5 <- gender
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(genplot2)

#disability 
pol<-polarity(disability$textNoEmoji)
displot2=ggplot(pol$all, aes(x=polarity ,
                             y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Disability") 
data5 <- disability
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(displot2)


#black women 
pol<-polarity(bwomen$textNoEmoji)
bwplot2=ggplot(pol$all, aes(x=polarity ,
                            y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Black Women") 
data5 <- bwomen
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(bwplot2)

#control 
pol<-polarity(control$textNoEmoji)
controlplo2=ggplot(pol$all, aes(x=polarity ,
                                y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75) +
  labs(title = "Control") 

data5 <- control
data5$polarity <- scale(pol$all$polarity)
pos.comments<-subset(data5$content,
                     data5$polarity>0)
neg.comments<-subset(data5$content,
                     data5$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)
all.corpus<-VCorpus(VectorSource(all.terms))


all.tdm<-TermDocumentMatrix(all.corpus, control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))

all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')
print(controlplo2)
