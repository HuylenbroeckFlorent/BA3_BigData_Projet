
#p-value=0.052
age.lm=lm(y~age,data = Dtrain)
summary(age.lm)

#p-value=1.956e-08
data1=Dtrain[Dtrain$marital=="married",]
data1$marital <- 1
data2=Dtrain[Dtrain$marital=="single",]
data2$marital <- 0
currentset=rbind(data1,data2)
married.lm=lm(y~marital,data = currentset)
summary(married.lm)

#p-value=0.1624
data1=Dtrain[Dtrain$housing=="yes",]
data1$housing <- 1
data2=Dtrain[Dtrain$housing=="no",]
data2$housing <- 0
currentset=rbind(data1,data2)
housing.lm=lm(y~housing,data=currentset)
summary(housing.lm)

#p-value=0.6493
data1=Dtrain[Dtrain$loan=="yes",]
data1$loan <- 1
data2=Dtrain[Dtrain$loan=="no",]
data2$loan <- 0
currentset=rbind(data1,data2)
loan.lm=lm(y~loan,data=currentset)
summary(loan.lm)


#marital+age p-value=1.018e-07
data1=Dtrain[Dtrain$marital=="married",]
data1$marital <- 1
data2=Dtrain[Dtrain$marital=="single",]
data2$marital <- 0
currentset=rbind(data1,data2)
marriedage.lm=lm(y~marital+age,data=currentset)
summary(marriedage.lm)




res=data.frame(id=1:nrow(Xtest),prob=0)



data1=Xtest[Xtest$marital=="married",]
data1$marital <- 1
data2=Xtest[Xtest$marital=="single",]
data2$marital <- 0
data3=Xtest[Xtest$marital!="married" & Xtest$marital!="single",]
currentset1=rbind(data1,data2)

proba=c()

for(i in 1:nrow(Xtest)){
  if(Xtest[i,]$marital=="single" | Xtest[i,]$marital=="married"){
    prob=predict(married.lm,currentset1[currentset1$id==i,],type="response")
    proba=c(proba,prob)
  }
  else{
    prob=predict(age.lm,Xtest[i,],type="response")
    proba=c(proba,prob)
  }
  
}


res=data.frame(id=1:length(proba),prob=proba)



write.csv(res,"C:/Users/Charly/Desktop/ProjetBD/Res.csv", row.names = FALSE)





