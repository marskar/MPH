getwd()
setwd()
times<-read.csv("Lab0hist.csv", header = FALSE)
install.packages("plyr")
library(plyr)
times[1]
count(times[2])
plot(count(times[2]))
plot(table(times[2]))
median(table(times[2]))
median(frequency(times[2]))
namesList<-unlist(times[2])
namesList
freqNames <- as.vector(names(table(unlist(namesList))))
freqCount <- as.numeric(table(unlist(namesList)))

df <- data.frame(Word = freqNames,
                    Count = freqCount)
attach(df)
dfSort<-df[order(-Count),]
detach(df)

barplot(df[1:5,2],col="blue",
        names.arg = freqNames,srt = 45,
        space=0.1, xlim=c(0,20),
        main = "hist",
        cex.names = 0.1, xpd = FALSE)

dat<-read.csv("Lab0dat.csv", header = FALSE)
freqNames <- as.vector(names(table(unlist(dat$V2[which(dat$V5=="X")]))))
freqCount <- as.numeric(table(unlist(dat$V2[which(dat$V5=="X")])))
dfX <- data.frame(Word = freqNames, Count = freqCount)

freqNames <- as.vector(names(table(unlist(dat$V2[which(dat$V5=="O")]))))
freqCount <- as.numeric(table(unlist(dat$V2[which(dat$V5=="O")])))
dfO <- data.frame(Word = freqNames, Count = freqCount)


dat<-read.csv("Lab1-dat-2015.csv", header = TRUE)
dat2<-dat[1:163,]
#of people who were sick how many had sore throat
table(sick$Throat)
#of people who were not sick how many had sore throat
table(dat2[ns,]$Throat)
table(dat$Fever)
table(dat$Headache)
table(dat$Vomit)
table(dat$Diarrhea)
table(dat2$Date)
table(dat2[which(dat2$Date=="[:digit:]")])
117/(35+117)
sickRow<-grep("[0-9]",dat2$Date)
throatRow<-grep("1",dat2$Throat)
feverRow<-grep("1",dat2$Fever)
foods<-colnames(dat2[,10:15])
membersAteSick<-membersAte[which(membersAte$Throat==1&membersAte$Fever==1),]
guestsAteSick<-guestsAte[which(guestsAte$Throat==1&guestsAte$Fever==1),]
ate<-dat2[which(dat2$Eggsalad==1|dat2$Macaroni==1|dat2$Cottage==1|dat2$Tunasal==1|dat2$Icecream==1|dat2$Other==1),]
membersAte<-ate[which(ate$Type==1),]
guestsAte<-ate[which(ate$Type==2),]
ateSick<-ate[which(ate$Throat==1&ate$Fever==1),]

I<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==1),]
II<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm=="noon"),]
III<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==2),]
IV<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==1),]
V<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm=="noon"),]
VI<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==2),]
VII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==1),]
VIII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm=="noon"),]
IX<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==2),]
X<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==1),]
XI<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm=="noon"),]
XII<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==2),]
XIII<-ateSick[which(ateSick$Date=="NK"),]
rowCounts<-c(nrow(I),nrow(II),nrow(III),nrow(IV),nrow(V), nrow(VI), nrow(VII),nrow(VIII),nrow(IX), nrow(X), nrow(XI),nrow(XII),nrow(XIII)) 
rowNames<-c("6am","6noon","6pm","7am","7noon","7pm","8am","8noon","8pm","9am","9noon","9pm","NK")
reg
I<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==1),]
II<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm=="noon"|ateSick$Am_Pm==0),]
III<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==2),]
IV<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==1),]
V<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm=="noon"|ateSick$Am_Pm==0),]
VI<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==2),]
VII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==1),]
VIII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm=="noon"|ateSick$Am_Pm==0),]
IX<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==2),]
X<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==1),]
XI<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm=="noon"|ateSick$Am_Pm==0),]
XII<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==2),]
XIII<-ateSick[which(ateSick$Date=="NK"),]
rowCounts<-c(nrow(I),nrow(II),nrow(III),nrow(IV),nrow(V), nrow(VI), nrow(VII),nrow(VIII),nrow(IX), nrow(X), nrow(XI),nrow(XII),nrow(XIII)) 
rowNames<-c("6am","6noon","6pm","7am","7noon","7pm","8am","8noon","8pm","9am","9noon","9pm","NK")


I0<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==0),]
I<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==1),]
II<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm=="noon"),]
III<-ateSick[which(ateSick$Date==6&ateSick$Am_Pm==2),]
IV0<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==0),]
IV<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==1),]
V<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm=="noon"),]
VI<-ateSick[which(ateSick$Date==7&ateSick$Am_Pm==2),]
VII0<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==0),]
VII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==1),]
VIII<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm=="noon"),]
IX<-ateSick[which(ateSick$Date==8&ateSick$Am_Pm==2),]
X0<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==0),]
X<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==1),]
XI<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm=="noon"),]
XII<-ateSick[which(ateSick$Date==9&ateSick$Am_Pm==2),]
XIII<-ateSick[which(ateSick$Date=="NK"),]
nrow()
rowCounts<-c(nrow(I0),nrow(I),nrow(II),nrow(III),nrow(IV0),nrow(IV),nrow(V), nrow(VI), nrow(VII0),nrow(VII),nrow(VIII),nrow(IX), nrow(X0), nrow(X), nrow(XI),nrow(XII),nrow(XIII)) 
rowNames<-c("6","6am","6noon","6pm","7","7am","7noon","7pm","8","8am","8noon","8pm","9","9am","9noon","9pm","NK")





sum(rowCounts)

plot(rowCounts)
?plot

names(rowCounts)<-rowNames
barplot(rowCounts, cex.names = 1, las=2)
plot(count(cases$Date[which(cases$Am_Pm==1)]))


length(ateSickCount$Freq[which(ateSickCount$Freq!=0)])
sum(ateSickCount$Freq[which(ateSickCount$Freq!=0)])
barplot(ateSickCount$Freq[which(ateSickCount$Freq!=0)], 
        names.arg = ateSickCount$Var1[which(ateSickCount$Freq!=0)])

text(x = xx, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")
ateSickCount<-as.data.frame(table(ateSick$Date))


colnames(ate)[1]<-"Type"
ate<-dat2[which(dat2$Eggsalad==1),]
dat2$foods[1]
NKrow<-grep("NK",dat2$Date)
dotRow<-grep("\\.",dat2$Date)
#members 
90/(62+90)
dat2$Date!="NK"
#how many people who ate 1 were sick?
table(dat2$Throat[which(dat2$Eggsalad==1)])
#how many people who ate 2 were sick?
table(dat2$Throat[which(dat2$Macaroni==1)])
#how many people who ate 3 were sick?
table(dat2$Throat[which(dat2$Cottage==1)])
#how many people who ate 4 were sick?
table(dat2$Throat[which(dat2$Tunasal==1)])
#how many people who ate 5 were sick?
table(dat2$Throat[which(dat2$Other==1)])

# attack rate

48/67
41/85
