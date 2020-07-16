library(foreign)
filelocation="/Users/kevinkuehn/Downloads/yrbs2015.sav"
dataset=read.spss(filelocation, to.data.frame=TRUE)

### NOTE - AGES 12 AND 18 ARE <=12 AND >=18
age=as.numeric(substr(dataset$Q1,1,2))

female=(dataset$Q2=='Female')*1

### NOTE - UNGRADED/OTHER DENOTED WITH 0
class=as.vector(dataset$Q3)
class[class=="9th grade"]=9
class[class=="10th grade"]=10
class[class=="11th grade"]=11
class[class=="12th grade"]=12
class[class=="Ungraded or other grade"]=0
class=as.numeric(class)

### NOTE - ONLY SELF REPORTED
glbq=(dataset$Q68==levels(dataset$Q68)[2] |
	dataset$Q68==levels(dataset$Q68)[3] |
	dataset$Q68==levels(dataset$Q68)[4])*1

### NOTE - SUICIDEATTEMPTS 2=2OR3, 4=4OR5, 6=6+
suicideattempts=as.numeric(substr(dataset$Q29,1,1))
suicide=(suicideattempts>0)*1

### NOTE - FIGHTS 2=2/3, ... ,10=10/11, 12=12+
numfights=as.numeric(substr(dataset$Q18,1,2))
fight=1*(numfights>0)

rape=1*(dataset$Q21=='Yes')

### NOTE - ONLY WITHIN RELATIONSHIP, -1 DID NOT DATE
sexualassault=as.numeric(substr(dataset$Q23,1,1))
sexualassault[dataset$Q23=="Did not date"]=-1
sexualassault=(sexualassault>0)*1

bully=1*(dataset$Q24=='Yes')
ebully=1*(dataset$Q25=='Yes')

### NOTE - NONE AND NOT SURE CODED AS ""
grade=substring(dataset$Q89,8,8)
grade[grade==" "]=''
grade[grade=="e"]=''

### NOTE - 4=4-, 10=10+
sleep=as.numeric(substr(dataset$Q88,1,2))

### NOTE - WELL OR VERY WELL CONSIDERED PROFICIENT
engproficiency=1*(dataset$Q99=="Very well" | dataset$Q99=="Well")

obese=1*(dataset$qnobese=='1')

### NOTE - CAN BE HISPANIC AND MULTIRACIAL
race.white=1*(dataset$raceeth=="White")
race.black=1*(dataset$raceeth=="Black or African American")
hispanic=1*(dataset$raceeth=="Hispanic / Latino" | dataset$raceeth=="Multiple - Hispanic")
race.asian=race.white=1*(dataset$raceeth=="Asian")
race.multiple=1*(dataset$raceeth=="Multiple - Non-Hispanic" | dataset$raceeth=="Multiple - Hispanic")
race.other=1*(dataset$raceeth=="Native Hawaiian/other PI" | dataset$raceeth=="Am Indian / Alaska Native")

nonwhite=1*(dataset$raceeth=="Black of African American"|dataset$raceeth=="Multiple - Hispanic"|dataset$raceeth=="Multiple - Non-Hispanic"|dataset$raceeth=="Hispanic / Latino"|dataset$raceeth=="Am Indian / Alaska Native"|dataset$raceth=="Asian"|dataset$raceeth=="Native Hawaiian/other PI")

dataset$Q5=as.numeric(dataset$Q5)
nonwhite=1*(!dataset$raceeth=="White")

hopeless=1*(dataset$Q26=="Yes")

### NOTE - 0 IS 0 OR <1, 5 IS 5+
compuse=substring(dataset$Q82,1,1)
compuse[compuse=="L" | compuse=="N"]="0"
compuse=as.numeric(compuse)

white=race.white
nonwhiteasian=!race.white & !race.asian
academictrouble=(grade=='F' |  grade=='D')

YRBS=data.frame(age=age, female=female, class=class, glbq=glbq, 
		suicideattempts=suicideattempts, hopeless=hopeless,
		suicide=suicide, numfights=numfights, fight=fight, rape=rape, 
		sexualassault=sexualassault, compuse=compuse, bully=bully, 
		ebully=ebully, grade=grade, sleep=sleep, obese=obese, 
		engproficiency=engproficiency, race.white=race.white, 
		race.black=race.black, hispanic=hispanic, 
		race.asian=race.asian, race.multiple=race.multiple, 
		race.other=race.other, nonwhiteasian=nonwhiteasian,
		academictrouble=academictrouble)

haveall=!is.na(glbq) & !is.na(suicideattempts) & !is.na(hopeless) &
	 !is.na(fight) & !is.na(rape) & !is.na(bully) &
	 !is.na(ebully)& !is.na(academictrouble) & !is.na(hopeless) &!is.na(female)

YRBShaveall=YRBS[haveall,c(2,4,5,6, 7,9,10,13,14,26)]
YRBShaveall$rape[YRBShaveall$rape==-1]=0
names(YRBShaveall)



logit.fit=glm(suicide~glbq+hopeless+fight+rape+academictrouble+bully+ebully,data=YRBShaveall,family=binomial)
summary(logit.fit)

logit.fit=glm(suicide~glbq+hopeless+fight+rape+academictrouble+bully+ebully,data=YRBShaveall,family=binomial)
summary(logit.fit)
summary(logit.fit)$coefficients[,1]
summary(logit.fit)$coefficients[,1]+qnorm(.975)*summary(logit.fit)$coefficients[,2]
summary(logit.fit)$coefficients[,1]-qnorm(.975)*summary(logit.fit)$coefficients[,2]

library(AER)
poisbinom.fit=glm(suicide~glbq+hopeless+fight+rape+academictrouble+bully+ebully,data=YRBShaveall,family=poisson(link=log))
summary(poisbinom.fit)
dispersiontest(poisbinom.fit)
exp(summary(poisbinom.fit)$coefficients[,1])
exp(summary(poisbinom.fit)$coefficients[,1]+qnorm(.975)*summary(poisbinom.fit)$coefficients[,2])
exp(summary(poisbinom.fit)$coefficients[,1]-qnorm(.975)*summary(poisbinom.fit)$coefficients[,2])

poiscount.fit=glm(suicideattempts~glbq+hopeless+fight+rape+academictrouble+bully+ebully,data=YRBShaveall,family=poisson(link=log))
summary(poiscount.fit)
dispersiontest(poiscount.fit)
exp(summary(poiscount.fit)$coefficients[,1])
exp(summary(poiscount.fit)$coefficients[,1]+qnorm(.975)*summary(poiscount.fit)$coefficients[,2])
exp(summary(poiscount.fit)$coefficients[,1]-qnorm(.975)*summary(poiscount.fit)$coefficients[,2])

a1=rep(c(rep(0,2^7),rep(1,2^7)),2^0)
a2=rep(c(rep(0,2^6),rep(1,2^6)),2^1)
a3=rep(c(rep(0,2^5),rep(1,2^5)),2^2)
a4=rep(c(rep(0,2^4),rep(1,2^4)),2^3)
a5=rep(c(rep(0,2^3),rep(1,2^3)),2^4)
a6=rep(c(rep(0,2^2),rep(1,2^2)),2^5)
a7=rep(c(rep(0,2^1),rep(1,2^1)),2^6)
a8=rep(c(rep(0,2^0),rep(1,2^0)),2^7)
sets=cbind(a1,a2,a3,a4,a5,a6,a7,a8)

PofA<-function(a){
	#P(sui=a1,glb=a2,hop=a3,
	#  fig=a4,rap=a5,aca=a6,
	#  bul=a7,ebu=a8


sets=cbind(sets,apply(sets,1,PofA))
tmp=logit.fit$coefficients[1]+sets[,2:8]%*%logit.fit$coefficients[2:8]
tmp=1/(exp(-tmp)+1)
sets=cbind(sets,tmp)

bu=sets[1:128,7]
eb=sets[1:128,8]
PZX=sets[1:128,9]+sets[129:256,9]
PYgZX=sets[1:128,10]

sum((PYgZX[bu==1]-PYgZX[bu==0])*(PZX[bu==1]+PZX[bu==0]))
sum((PYgZX[eb==1]-PYgZX[eb==0])*(PZX[eb==1]+PZX[eb==0]))


tmp=sets[1:128,3]
sum((PYgZX[tmp==1]-PYgZX[tmp==0])*(PZX[tmp==1]+PZX[tmp==0]))

sets[1:64,10]/(sets[1:64,10]+sets[65:128,10])

YRBS=data.frame(glbq=glbq, suicide=suicide, hopeless=hopeless,
	  fight=fight, rape=rape, bully=bully, 
	  ebully=ebully, academictrouble=academictrouble, female=female,
	  age2=age2, sexualassault=sexualassault, engproficiency=engproficiency, compuse2=compuse2,
	  nonwhite=nonwhite, hispanic=hispanic, sleep2=sleep2,  obese=obese)

age2=(age>mean(age[!is.na(age)]))*1
compuse2=(compuse>mean(compuse[!is.na(compuse)]))*1
sleep2=(sleep>mean(sleep[!is.na(sleep)]))*1


haveall=!is.na(glbq) & !is.na(suicide) & !is.na(hopeless) &
	 !is.na(fight) & !is.na(rape) & !is.na(bully) &
	 !is.na(ebully)& !is.na(academictrouble)& !is.na(female)& 
	 !is.na(age2)& !is.na(sexualassault)& !is.na(engproficiency)&
	 !is.na(compuse2)& !is.na(nonwhite) &  
	 !is.na(hispanic)& !is.na(sleep2) & !is.na(obese)

YRBShaveall=YRBS[haveall,c(1:17)]

YRBShaveall$rape[YRBShaveall$rape==-1]=3627
names(YRBShaveall)

haveall2=!is.na(glbq) & !is.na(suicide) & !is.na(hopeless) &
  !is.na(fight) & !is.na(rape) & !is.na(bully) &
  !is.na(ebully)& !is.na(academictrouble) 

YRBShaveall2=YRBS[haveall2,c(1:8)]

###FOR PC ALGORITHM###
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
# install.packages("pcalg", repos="http://cran.fhcrc.org/")
library("pcalg")
library("graph")
#
biocLite("Rgraphviz")  ## seems not to run on many systems
library("Rgraphviz")
#
 library(abind)
 library(corpcor)
 library(sfsmisc)
 library(robustbase)
######################

attr(YRBShaveall2,"names")
indepTest <- gaussCItest
corr.mat <- YRBShaveall2
names <- attr(YRBShaveall2, "names")
n <- nrow(YRBShaveall2)
suffStat <- list(C=cor(YRBShaveall2, n = n))
p <- ncol(YRBShaveall2) 

alpha <- 0.0005
pc.fit <- pc(suffStat, indepTest, p = p, alpha = alpha, verbose = TRUE)
showAmat(pc.fit)
showEdgeList(pc.fit,names)
if (plotcpdag) {
  plot(pc.fit, main = "Estimated CPDAG",labels=names)
}

summary(glm(suicide~female+age+glbq+rape+hopeless+fight+bully+ebully,data=YRBShaveall2,family=binomial(link='logit')))

