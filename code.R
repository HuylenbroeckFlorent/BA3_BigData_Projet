

#age p-value=0.053
#age+I(age^2) p-value <2.2e-16
summary(lm(y~age+I(age^2),data=Dtrain))

crossvalid(Dtrain,c("age","I(age^2)"))

#job (bcp de job >0.05 p value)
summary(lm(y~job,data=Dtrain))
crossvalid(Dtrain,"job")
#marital (married et unknown >0.05)
summary(lm(y~marital,data=Dtrain))
crossvalid(Dtrain,"marital")
#contact p-value<2.2e-16
summary(lm(y~contact,data=Dtrain))

#default (defaultyes=0.59)
summary(lm(y~default,data=Dtrain))

#housing p-value 0.2822
summary(lm(y~housing,data=Dtrain))

#loan pvalue 0.6765
summary(lm(y~loan,data=Dtrain))

#month (monthdec=0.286)
summary(lm(y~month,data=Dtrain))

#day_of_week (bcp >0.05)
summary(lm(y~day_of_week,data=Dtrain))

#edu (bcp >0.05)
summary(lm(y~edu,data=Dtrain))

#poutcome (success ok, nonexistent pas ok)
summary(lm(y~poutcome,data=Dtrain))

#backyard
summary(lm(y~age+job+marital+contact+default+housing+loan+month+day_of_week+edu+poutcome,data=Dtrain))
summary(lm(y~age+job+marital+contact+default+housing+month+day_of_week+edu+poutcome,data=Dtrain))
summary(lm(y~age+job+marital+contact+default+housing+month+day_of_week+poutcome,data=Dtrain))
summary(lm(y~age+job+contact+default+housing+month+day_of_week+poutcome,data=Dtrain))
summary(lm(y~age+job+contact+default+month+day_of_week+poutcome,data=Dtrain))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain))
summary(lm(y~age+contact+month+day_of_week+poutcome,data=Dtrain))
summary(lm(y~contact+month+poutcome,data=Dtrain))


summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue",]))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec",]))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown",]))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management",]))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed",]))
summary(lm(y~age+job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed",]))
summary(lm(y~job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed",]))
summary(lm(y~job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed" & Dtrain$job!="housemaid",]))
summary(lm(y~job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed" & Dtrain$job!="housemaid" & Dtrain$job!="entrepreneur",]))
summary(lm(y~job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed" & Dtrain$job!="housemaid" & Dtrain$job!="entrepreneur" & Dtrain$day_of_week!="thu",]))
summary(lm(y~job+contact+month+day_of_week+poutcome,data=Dtrain[Dtrain$day_of_week!="tue" & Dtrain$month!="dec" & Dtrain$job!="unknown" & Dtrain$job!="management" & Dtrain$job!="self-employed" & Dtrain$job!="unemployed" & Dtrain$job!="housemaid" & Dtrain$job!="entrepreneur" & Dtrain$day_of_week!="thu" & Dtrain$day_of_week!="wed",]))


#contact dummies
contactv=Dtrain[,"contact"]
contact <- as.numeric(contactv=="telephone")

y=Dtrain[,"y"]
finalset=data.frame(cbind(contact,y))
summary(lm(y~contact,data=finalset))

crossvalid(finalset,"contact")

#married dummies
marriedv=Dtrain[,"marital"]
married <- as.numeric(marriedv=="married")

y=Dtrain[,"y"]

finalset=data.frame(cbind(married,y))
summary(lm(y~married,data=finalset))

crossvalid(finalset,"married")

#married+contact

finalset=data.frame(cbind(contact,married,y))
summary(lm(y~contact+married,data=finalset))

crossvalid(finalset,c("contact","married"))

#student dummies
studentv=Dtrain[,"job"]
student=as.numeric(studentv=="student")
y=Dtrain[,"y"]
finalset=data.frame(cbind(student,y))
summary(lm(y~student,data=finalset))
crossvalid(finalset,c("student"))

#retired dummies
retiredv=Dtrain[,"job"]
retired=as.numeric(retiredv=="retired")
y=Dtrain[,"y"]
finalset=data.frame(cbind(retired,y))
summary(lm(y~retired,data=finalset))
crossvalid(finalset,c("retired"))

#divorce dummies p-value=0.539
divorcedv=Dtrain[,"marital"]
divorced <- as.numeric(divorcedv=="divorced")

y=Dtrain[,"y"]

finalset=data.frame(cbind(divorced,y))
summary(lm(y~divorced,data=finalset))

#single dummies
singlev=Dtrain[,"marital"]
single <- as.numeric(singlev=="single")

y=Dtrain[,"y"]

finalset=data.frame(cbind(single,y))
summary(lm(y~single,data=finalset))
crossvalid(finalset,c("single"))

#eduuniversity.degree dummies
univ=Dtrain[,"edu"]
uni <- as.numeric(univ=="university.degree")
finalset=data.frame(cbind(uni,y))
summary(lm(y~uni,data=finalset))
crossvalid(finalset,c("uni"))

#successpoutcome dummies
successv=Dtrain[,"poutcome"]
success <- as.numeric(successv=="success")
finalset=data.frame(cbind(success,y))
summary(lm(y~success,data=finalset))
crossvalid(finalset,c("success"))

#failurepoutcome dummies p-value=0.3735
failurev=Dtrain[,"poutcome"]
failure <- as.numeric(failurev=="failure")
finalset=data.frame(cbind(failure,y))
summary(lm(y~failure,data=finalset))
crossvalid(finalset,c("failure"))



#student+married
finalset=data.frame(cbind(student,married,y))
summary(lm(y~student+married,data=finalset))
crossvalid(finalset,c("student","married"))

#married+student+contact
finalset=data.frame(cbind(student,married,contact,y))
summary(lm(y~student+married+contact,data=finalset))
crossvalid(finalset,c("student","married","contact"))

#student+contact+age+(age^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(student,contact,age,y))

summary(lm(y~student+contact+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","contact","age","I(age^2)"))


#student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(student,retired,contact,age,y))

summary(lm(y~student+contact+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","retired","contact","age","I(age^2)"))


#single+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,student,retired,contact,age,y))

summary(lm(y~student+single+contact+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","single","retired","contact","age","I(age^2)"))


#uni+single+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,uni,student,retired,contact,age,y))

summary(lm(y~student+contact+uni+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","uni","retired","contact","age","I(age^2)"))

#uni+success+student+retired+contact+age+age(^2)
age=Dtrain[,"age"]
finalset=data.frame(cbind(single,success,uni,student,retired,contact,age,y))

summary(lm(y~student+contact+uni+success+retired+age+I(age^2),data=finalset))
crossvalid(finalset,c("student","uni","success","retired","contact","age","I(age^2)"))


crossvalid <- function (set,predictor){
  set=set[sample(nrow(set)),]
  
  folds <- cut(seq(1,nrow(Dtrain)),breaks=10,labels=FALSE)
  
  errors=c()
  
  for(i in 1:10){
    
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- set[testIndexes, ]
    trainData <- set[-testIndexes, ]
    
    a <- "y~"
    for(i in 1:length(predictor)){
      if(i!=length(predictor)){
        a <- paste(a,predictor[i],"+") 
      }
      else{
        a <- paste(a,predictor[i])
      }
    }
    
    
    lm.fit=lm(a,data=trainData)
    
    currenterr=mean((testData$y-predict(lm.fit ,testData))^2)
    errors=c(errors,currenterr)
  }  
  print(mean(errors))
}




#test married+student+contact
marriedv=Xtest[,"marital"]
marriedTest <- as.numeric(marriedvTest=="married")
studentv=Xtest[,"job"]
studentTest <- as.numeric(studentvTest=="student")
contactv=Xtest[,"contact"]
contactTest <- as.numeric(contactvtest=="telephone")



test.lm=lm(y~student+married+contact,data=finalset)


finalTestSet=data.frame(cbind(married,student,contact))

proba=predict(test.lm,finalTestSet, type="response")


#test uni+success+student+retired+contact+age+age(^2)
studentvT=Xtest[,"job"]
studentT=as.numeric(studentvT=="student")
retiredvT=Xtest[,"job"]
retiredT=as.numeric(retiredvT=="retired")
univT=Xtest[,"edu"]
uniT <- as.numeric(univT=="university.degree")
contactvT=Xtest[,"contact"]
contactT <- as.numeric(contactvT=="telephone")
ageT=Xtest[,"age"]
successvT=Xtest[,"poutcome"]
successT <- as.numeric(successvT=="success")


test.lm=lm(y~student+contact+uni+success+retired+age+I(age^2),data=finalset)

finalTestSet=data.frame(cbind(student=studentT,retired=retiredT,uni=uniT,contact=contactT,success=successT,age=ageT))

proba=predict(test.lm,finalTestSet,type="response")



res=data.frame(id=1:nrow(Xtest),prob=proba)




write.csv(res,"C:/Users/Charly/Desktop/ProjetBD/Res.csv", row.names = FALSE)









