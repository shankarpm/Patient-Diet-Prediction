
patientData <- read.csv('PatientDiet\\dataset1.csv',header=T) 
head(patientData)
VisitData <- read.csv('PatientDiet\\dataset2.csv',header=T) 
names(patientData)[1] <- "id"

head(patientData)
finaldata <- join(patientData, VisitData, by='id', type='left', match='all')

write.csv(finaldata,'PatientDiet\\output.csv')
finaldata$treatment <- factor(finaldata$treatment)
finaldata$visitnumber <- factor(finaldata$visitnumber)


head(finaldata)

library(e1071)

summary(finaldata)

model <- svm(outcome ~ treatment + age + visitnumber , data = finaldata, kernel = "linear", cost = .1, scale = FALSE)
model_treatment <- lm(outcome ~ treatment + age + visitnumber , data = finaldata)

model_no_treatment <- lm(outcome ~  age + visitnumber , data = finaldata)

cor(finaldata,method="pearson")
par(model_treatment)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model_treatment)
qqplot(finaldata$outcome[finaldata$treatment == 1]),
       +    finaldata$outcome[finaldata$treatment == 0])

qqplot(finaldata$outcome[finaldata$treatment == 0],
+    finaldata$outcome[finaldata$treatment == 1])

cor(finaldata$outcome,finaldata$treatment)
 
warnings()
summary(model_treatment) #0.6841   pvalue < 2.2e-16
summary(model_no_treatment) #0.0003   pvalue : .2584

finaldataT1 <- finaldata[finaldata$treatment == 1,]

finaldataT0 <- finaldata[finaldata$treatment == 0,]


head(finaldataT1)

library(dplyr)

# data_T1_MeanWeight  <- finaldataT1 %>% 
#   group_by(id) %>%
#   summarise(ageMean = mean(age),
#             meanOutcome = mean(outcome))
# 
# head(data_T1_MeanWeight)

#finaldataT1_t <- finaldataT1[finaldataT1$id==1,]

#ggplot(data=finaldataT1,aes(x=visitnumber,y=outcome),size =1)        + geom_line()   
#ggplot(data=finaldataT1_t,aes(x=visitnumber,y=outcome,group = id))        + geom_line()   
ggplot(data=finaldataT1,aes(x=visitnumber,y=outcome,group = id,colour=id))+
geom_line() + ggtitle("Patient Treatment with Diet")  +
  labs(x="By Patient Vists",y="Weight outcome") 

ggplot(data=finaldataT0,aes(x=visitnumber,y=outcome,group = id,colour=id))+
  geom_line() + ggtitle("Patient Treatment without Diet")  +
  labs(x="By Patient Vists",y="Weight outcome") 

plot(data = finaldataT0, outcome ~ visitnumber ,pch=id, xlab = "By Patient Vists",
     ylab = "Weight outcome", main = "Patient Treatment without Diet",
     col=as.integer(id))


plot(data = finaldataT1, outcome ~ visitnumber ,pch=id, xlab = "By Patient Vists",
     ylab = "Weight outcome", main = "Patient Treatment with Diet",
     col=as.integer(id)+1)

 