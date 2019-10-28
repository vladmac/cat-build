library(readxl)

FullData1<- read_excel("C:/Users/Vlad-PC/Desktop/Math work complete/programming/working in r/2010/FullData.xlsx", 
                      sheet = "Sheet2")


FullData<-(na.omit(as.matrix((FullData1[,-c(26,27,50,51)])))) # we omit all the N/As since can cause problems, these N/a are mostly due to no data from the scottish regions

attach(FullData)


D=matrix(ncol = ncol(FullData), nrow = nrow(FullData)) #we change the data from an array to a matrix and then to numeric values since the data was listed as strings and that caused me some headache, so I would rather work with stuff I know


for (i in (1:ncol(FullData))){
  D[,i]=as.numeric(FullData[,i])
}





#Here we look at the percentage of people between certain age groups and the signatures on the page
par(mfrow=c(1,1))

l=1         #lower age group
k=30        #upper age group
U30=rowSums(D[,c((48+l):(48+k))])
P30=U30/D[,48]
#percentage of people under 30

O60=rowSums(D[,c((48+60):(48+89))])
P60=O60/D[,48]
#percentage of people over 60

O45=rowSums(D[,c((48+31):(48+59))])
P45=O45/D[,48]
#percentage of people between 30 and 60

plot(SIGN,P30, main ="percentage of people under 30")
plot(SIGN,P60, main = "percentage of people over 60")
plot(SIGN,P45, main = "percentage of people between 30 and 60")


#after playing around , we can see that there is a weak correlation between age group and signature, i.e. in areas where we have higher percentage of young people we have more signatures and areas with more older people, less signatures
#this is what we would expect




#signature count vs main parties
par(mfrow=c(3,2))
for (i in 11:16){
  plot(SIGN,D[,i]/D[,48], main=i)
}

#perentage P30,P60 and main parties 
par(mfrow=c(3,4))
for (i in 11:16){
  plot(P30,D[,i]/D[,48], main=i)
  plot(P60,D[,i]/D[,48], main=i)
  }
#talk about the graphs





install.packages("MASS")
library(MASS)
#anyway let's try and create a linear model to that will explain the type of people who are in favour of a second referendum / abolish brexit
#from the above we can see that people's age group is a good(ish) indicator of their political party 
#let's check some variables against normalised signatures


par(mfrow=c(5,4))
par(mar=c(1,1,1,1))
n1=(c(26:32, 34:35, 37:47))
for (i in n1){
  plot(SIGN,D[,i], main = i)
}

Par=matrix(data=c(D[,c(n1)],P30,P45,P60), nrow= 573)

fit1<-lm(SIGN~.,data.frame(Par))
fit2<-lm(SIGN ~ 1, data.frame(Par))
summary(full.model)
anova(full.model)

aic.forward.model <- stepAIC(fit2, scope=list(upper=fit1,lower=fit2),direction="forward")



s.model<-lm (SIGN ~ Par[,4] + Par[,9] + Par[,8] + Par[,6] + Par[,11] + Par[,19] + Par[,14] + Par[,17] + Par[,22] + Par[,7]+ 
               Par[,10] + Par[,12])
summary(s.model)

anova(s.model)

res=rstandard(s.model)

fits=fitted(s.model)

plot(res,fits)
qqnorm(res)


qqline(res)
shapiro.test(res)


####



s1=log(SIGN/(1-SIGN))
fits1<-lm(s1~.,data.frame(Par))
fits2<-lm(s1 ~ 1, data.frame(Par))

aic.forward.model <- stepAIC(fit2, scope=list(upper=fits1,lower=fits2),direction="forward")




s1.model<-lm(s1 ~ Par[,4] + Par[,6] + Par[,9] + Par[,8] + Par[,19] + Par[,17] + Par[,10] + Par[,22] + Par[,7] + Par[,21] + Par[,23] + 
               Par[,1] + Par[,12]+ Par[,20] + Par[,14] + Par[,2] + Par[,5] )




summary(s1.model)
anova(s1.model)

res1=rstandard(s1.model)

fits1=fitted(s1.model)

plot(res1,fits1)

qqnorm(res1)

qqline(res1)

shapiro.test(res1)


####

s2=log(SIGN/(1-SIGN))

fits12<-lm(s2~.,data.frame(Par))
fits22<-lm(s2 ~ 1, data.frame(Par))

aic.forward.model <- stepAIC(fits22, scope=list(upper=fits12,lower=fits22),direction="forward")
