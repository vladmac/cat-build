library(readxl)

FullData<- read_excel("C:/Users/Vlad-PC/Desktop/Math work complete/programming/working in r/2010/FullData.xlsx", 
                                  sheet = "Sheet2")
View(FullData)

attach(FullData)


#SET OUT WHAT YOU WANT TO ANALYSE OR LOOK AT ?



plot(`Average house price`,
     `Leave Vote`)

#Data<-FullData[,c(4,8,9,11,15,25,28,29,30,31,32,33,34)]
#D<-as.matrix(Data)

model1<-lm(`Leave Vote`~ukip+`signature count`+`Percent with degree`+ majority)

x=seq(from=-5,to= 5 ,by=0.1)
y1=0.2-0.05*x
y2=0.75-0.0625*x

summary(model1)
anova(model1)

std<-rstandard(model1)
fits<-fitted(model1)
plot(std,fits)

lines(x,y1)
lines(x,y2)





shapiro.test(std)

model2<-lm(`Leave Vote`~ukip+`signature count`+`Percent with degree`)
summary(model2)

std2<-rstandard(model2)
shapiro.test(std2)

model.best<-lm(`Leave Vote`~ `signature count`+electorate+`valid votes`+`invalid votes`+majority+lab+ld+ukip+pc+`Average house price`+`public sector employment`+nonukborn)
summary(model.best)


FullData2 <- read_excel("C:/Users/Vlad-PC/Desktop/Math work/programming/working in r/2010/FullData.xlsx", 
                        sheet = "Sheet3")

d<-as.matrix(FullData2)


max(FullData[1,c(11:20)])

Votes<-vector()

for (i in 1:650) {
Votes[i]<- max(FullData[i,c(11:20)]) 
}
maj<-Votes-`signature count`
plot((1:650),maj)


FullData[(FullData[,38] < 0),c(1,4,12:15,38)]

full.model<-lm(d[,19]~d[,-c(19)])
aic.forward.model <- step(full.model, scope=~d[,1]+d[,2]+d[,3]+d[,4]+d[,5]+d[,6]+d[,7]+d[,8]+d[,9]+d[,10]+d[,11]+d[,12]+d[,13]+d[,14]+d[,15],
                          +d[,16]+d[,17]+d[,18],direction="backward")
