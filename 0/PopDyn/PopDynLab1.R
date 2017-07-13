#PopDyn Lab
#One way of looking at population growth is to calculate the percent change in population size over a given time interval.  If Po is the population at a starting date, and Pn the population n years later, then the percent increase is:
getwd()
#((Pn - Po)/ Po ) x 100    or   ( (Pn/Po) - 1) ) x 100

popGro1<-function(n,o) {((n - o)/ o ) * 100}    #or
popGro2<-function(n,o) {( (n/o)-1) * 100}
tbl1<-read.csv("PopDynTable1.csv")
E1950<-tbl1$Population[which(tbl1$ï..Region=="Europe"&tbl1$Year==1950)]
E2000<-tbl1$Population[which(tbl1$ï..Region=="Europe"&tbl1$Year==2000)]
E2050<-tbl1$Population[which(tbl1$ï..Region=="Europe"&tbl1$Year==2050)]
popGro1(E2000,E1950)
popGro1(E2050,E2000)

NA1950<-tbl1$Population[which(tbl1$ï..Region=="North America"&tbl1$Year==1950)]
NA2000<-tbl1$Population[which(tbl1$ï..Region=="North America"&tbl1$Year==2000)]
NA2050<-tbl1$Population[which(tbl1$ï..Region=="North America"&tbl1$Year==2050)]
popGro1(NA2000,NA1950)
popGro1(NA2050,NA2000)

LA1950<-tbl1$Population[which(tbl1$ï..Region=="Latin America"&tbl1$Year==1950)]
LA2000<-tbl1$Population[which(tbl1$ï..Region=="Latin America"&tbl1$Year==2000)]
LA2050<-tbl1$Population[which(tbl1$ï..Region=="Latin America"&tbl1$Year==2050)]
popGro1(LA2000,LA1950)
popGro1(LA2050,LA2000)

Af1950<-tbl1$Population[which(tbl1$ï..Region=="Africa"&tbl1$Year==1950)]
Af2000<-tbl1$Population[which(tbl1$ï..Region=="Africa"&tbl1$Year==2000)]
Af2050<-tbl1$Population[which(tbl1$ï..Region=="Africa"&tbl1$Year==2050)]
popGro1(Af2000,Af1950)
popGro1(Af2050,Af2000)

As1950<-tbl1$Population[which(tbl1$ï..Region=="Asia"&tbl1$Year==1950)]
As2000<-tbl1$Population[which(tbl1$ï..Region=="Asia"&tbl1$Year==2000)]
As2050<-tbl1$Population[which(tbl1$ï..Region=="Asia"&tbl1$Year==2050)]
popGro1(As2000,As1950)
popGro1(As2050,As2000)

rate<- function(n,Pn,Po) {(1/n)*(log(Pn/Po))}
rate(50,E2050,E2000)*100
rate(50,E2000,E1950)*100
rate(50,As2050,As2000)*100
rate(50,As2000,As1950)*100
rate(50,Af2050,Af2000)*100
rate(50,Af2000,Af1950)*100
rate(50,LA2050,LA2000)*100
rate(50,LA2000,LA1950)*100
rate(50,NA2050,NA2000)*100
rate(50,NA2000,NA1950)*100

#Assuming the average annual rate of growth of Africa in the period 2000-2050 continued indefinitely:
#a. How many years would it take for the population to double?

dbl<-function(r) {(0.693/r)*100}
dbl(2)

#About how many times would Africa's 2000 population have to double to exceed the world's present population of 7 billion? 
log2(7000/Af2000)

#About how many years would this take?
#ln3/r=n
(log(3)/2)*100
log(3)/0.02

#The world's population reached 7 billion in the year 2011 and was growing at 1% per year.  If this growth rate continues, how many years will it take to add 1 billion to the population?
log(8/7)/1*100

