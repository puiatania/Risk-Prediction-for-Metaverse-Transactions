library(readxl)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score","high_risk")]
date_n
#View(dateKNN)
attach(date)
date <- na.omit(date)
date

###
#Statistici descriptive
###

summary(date_n)
#Standard Deviation
apply(date_n,2,sd)
#cv - am facut o functie in R pentru a calculca coef de variatie
cv <- function(x) {
  coef <- sd(x)/mean(x) 
  return(coef)
}
apply(date_n, 2, cv)
#Histograme
hist(date_n$amount, main="Amount of the transfer", col = "plum2")
hist(date_n$login_frequency, main="Login frequency of the account", col = "purple")
hist(date_n$session_duration, main="Duration of session", col = "pink")
hist(date_n$risk_score, main="Risk score", col = 
       "indianred2" )
#Boxploturi
boxplot(date_n$amount, main="Amount of the transfer", col = "plum2")
boxplot(date_n$login_frequency, main="Login frequency of the account", col = "purple")
boxplot(date_n$session_duration, main="Duration of session", col = "pink")
boxplot(date_n$risk_score, main="Risk score", col = 
       "indianred2" )
#
R <- cor(date_n)
#View(R)
library(corrplot)
#windows()
corrplot(R,method="number", type="upper")


###
#Clusterizare fuzzy
###

library(e1071)
set.seed(123)
result<-cmeans(date_n,3,100,m=3, method="cmeans") 
result
result$centers
library(cluster)
rownames(date_n)=date$Name
res.fanny <- fanny(date_n, 3)
data.frame(res.fanny$clustering, date$Name)
library(factoextra)
?fviz_cluster()
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE, palette = "jco", ggtheme = theme_minimal(), legend = "right")
fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())
res.fanny$silinfo


###
#Problema de regresie logistica binomiala
###

library(ggplot2)
#windows()
plot<-ggplot(data=date, aes(x=amount, y=login_frequency, col=high_risk))
plot<-plot+geom_point(aes(size=5))
plot
date$high_risk.f<-factor(date$high_risk)
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(date$high_risk.f, SplitRatio=0.75)
split
setantrenare<-subset(date, split==TRUE)
setantrenare
settestare<-subset(date, split==FALSE)
settestare
plot<-ggplot(data=date, aes(x=amount, y=login_frequency, col=high_risk.f))
plot<-plot+geom_point(aes(size=5))
plot
model<-glm(high_risk.f~login_frequency+amount, data=setantrenare, 
           family=binomial)
summary(model)#AIC: 203.19
exp(coef(model))
contrasts(date$high_risk.f)
prob1<-predict(model, setantrenare, type="response")
prob1
pred_ant<-rep("0", dim(setantrenare)[1])
pred_ant
pred_ant[prob1>.5]="1"
pred_ant
table(pred_ant, setantrenare$high_risk.f)
prob2<-predict(model, settestare, type="response")
prob2
pred_test<-rep("0", dim(settestare)[1])
pred_test
pred_test[prob2>.5]="1"
pred_test
table(pred_test, settestare$high_risk.f)
mean(pred_test==settestare$high_risk.f)
#View(settestare)
#install.packages("ROCR")
library(ROCR)
p<-predict(model, newdata=settestare, type="response")
pr<-prediction(p, settestare$high_risk.f)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc<-performance(pr, measure="auc")
auc<-auc@y.values[[1]]
auc#acuratete de 84,88%

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score","high_risk")]
date_n
#View(dateKNN)
attach(date)
date <- na.omit(date)
date
library(ggplot2)
#windows()
plot<-ggplot(data=date, aes(x=amount, y=session_duration, col=high_risk))
plot<-plot+geom_point(aes(size=5))
plot
date$high_risk.f<-factor(date$high_risk)
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(date$high_risk.f, SplitRatio=0.75)
split
setantrenare<-subset(date, split==TRUE)
setantrenare
settestare<-subset(date, split==FALSE)
settestare
plot<-ggplot(data=date, aes(x=amount, y=session_duration, col=high_risk.f))
plot<-plot+geom_point(aes(size=5))
plot
model<-glm(high_risk.f~session_duration+amount, data=setantrenare, 
           family=binomial)
summary(model)#AIC: 198.26
exp(coef(model))
contrasts(date$high_risk.f)
prob1<-predict(model, setantrenare, type="response")
prob1
pred_ant<-rep("0", dim(setantrenare)[1])
pred_ant
pred_ant[prob1>.5]="1"
pred_ant
table(pred_ant, setantrenare$high_risk.f)
prob2<-predict(model, settestare, type="response")
prob2
pred_test<-rep("0", dim(settestare)[1])
pred_test
pred_test[prob2>.5]="1"
pred_test
table(pred_test, settestare$high_risk.f)
mean(pred_test==settestare$high_risk.f)
#View(settestare)
#install.packages("ROCR")
library(ROCR)
p<-predict(model, newdata=settestare, type="response")
pr<-prediction(p, settestare$high_risk.f)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc<-performance(pr, measure="auc")
auc<-auc@y.values[[1]]
auc#acuratete de 85.27%

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score","high_risk")]
date_n
#View(dateKNN)
attach(date)
date <- na.omit(date)
date
library(ggplot2)
#windows()
plot<-ggplot(data=date, aes(x=amount, y=risk_score, col=high_risk))
plot<-plot+geom_point(aes(size=5))
plot
date$high_risk.f<-factor(date$high_risk)
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(date$high_risk.f, SplitRatio=0.75)
split
setantrenare<-subset(date, split==TRUE)
setantrenare
settestare<-subset(date, split==FALSE)
settestare
plot<-ggplot(data=date, aes(x=amount, y=risk_score, col=high_risk.f))
plot<-plot+geom_point(aes(size=5))
plot
model<-glm(high_risk.f~risk_score+amount, data=setantrenare, 
           family=binomial)
summary(model)#AIC: 6
exp(coef(model))
contrasts(date$high_risk.f)
prob1<-predict(model, setantrenare, type="response")
prob1
pred_ant<-rep("0", dim(setantrenare)[1])
pred_ant
pred_ant[prob1>.5]="1"
pred_ant
table(pred_ant, setantrenare$high_risk.f)
prob2<-predict(model, settestare, type="response")
prob2
pred_test<-rep("0", dim(settestare)[1])
pred_test
pred_test[prob2>.5]="1"
pred_test
table(pred_test, settestare$high_risk.f)
mean(pred_test==settestare$high_risk.f)
#View(settestare)
#install.packages("ROCR")
library(ROCR)
p<-predict(model, newdata=settestare, type="response")
pr<-prediction(p, settestare$high_risk.f)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc<-performance(pr, measure="auc")
auc<-auc@y.values[[1]]
auc#acuratete de 100%

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score","high_risk")]
date_n
#View(dateKNN)
attach(date)
date <- na.omit(date)
date
library(ggplot2)
#windows()
plot<-ggplot(data=date, aes(x=login_frequency, y=risk_score, col=high_risk))
plot<-plot+geom_point(aes(size=5))
plot
date$high_risk.f<-factor(date$high_risk)
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(date$high_risk.f, SplitRatio=0.75)
split
setantrenare<-subset(date, split==TRUE)
setantrenare
settestare<-subset(date, split==FALSE)
settestare
plot<-ggplot(data=date, aes(x=login_frequency, y=risk_score, col=high_risk.f))
plot<-plot+geom_point(aes(size=5))
plot
model<-glm(high_risk.f~risk_score+login_frequency, data=setantrenare, 
           family=binomial)
summary(model)#AIC: 6
exp(coef(model))
contrasts(date$high_risk.f)
prob1<-predict(model, setantrenare, type="response")
prob1
pred_ant<-rep("0", dim(setantrenare)[1])
pred_ant
pred_ant[prob1>.5]="1"
pred_ant
table(pred_ant, setantrenare$high_risk.f)
prob2<-predict(model, settestare, type="response")
prob2
pred_test<-rep("0", dim(settestare)[1])
pred_test
pred_test[prob2>.5]="1"
pred_test
table(pred_test, settestare$high_risk.f)
mean(pred_test==settestare$high_risk.f)
#View(settestare)
#install.packages("ROCR")
library(ROCR)
p<-predict(model, newdata=settestare, type="response")
pr<-prediction(p, settestare$high_risk.f)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc<-performance(pr, measure="auc")
auc<-auc@y.values[[1]]
auc#acuratete de 100%

library(ggplot2)
#windows()
plot<-ggplot(data=date, aes(x=login_frequency, y=session_duration, col=high_risk))
plot<-plot+geom_point(aes(size=5))
plot
date$high_risk.f<-factor(date$high_risk)
#install.packages("caTools")
library(caTools)
set.seed(123)
split=sample.split(date$high_risk.f, SplitRatio=0.75)
split
setantrenare<-subset(date, split==TRUE)
setantrenare
settestare<-subset(date, split==FALSE)
settestare
plot<-ggplot(data=date, aes(x=login_frequency, y=session_duration, col=high_risk.f))
plot<-plot+geom_point(aes(size=5))
plot
model<-glm(high_risk.f~session_duration+login_frequency, data=setantrenare, 
           family=binomial)
summary(model)#AIC: 194.23
exp(coef(model))
contrasts(date$high_risk.f)
prob1<-predict(model, setantrenare, type="response")
prob1
pred_ant<-rep("0", dim(setantrenare)[1])
pred_ant
pred_ant[prob1>.5]="1"
pred_ant
table(pred_ant, setantrenare$high_risk.f)
prob2<-predict(model, settestare, type="response")
prob2
pred_test<-rep("0", dim(settestare)[1])
pred_test
pred_test[prob2>.5]="1"
pred_test
table(pred_test, settestare$high_risk.f)
mean(pred_test==settestare$high_risk.f)
#View(settestare)
#install.packages("ROCR")
library(ROCR)
p<-predict(model, newdata=settestare, type="response")
pr<-prediction(p, settestare$high_risk.f)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc<-performance(pr, measure="auc")
auc<-auc@y.values[[1]]
auc
#acuratete de 87.65%

###
#Regresie logistica multinomiala
###

date$risk_score <-
  factor(date$risk_score)
date$risk_score
date$out <- relevel(date$risk_score, ref="100")
#install.packages("nnet")
library(nnet)
mymodel<-multinom(out~amount+login_frequency+session_duration, data=date, 
                  trace=FALSE)
summary(mymodel)#AIC=2132.019
exp(coef(mymodel))
p1 <- predict(mymodel,date)
p1
predict(mymodel,date,type="prob")
predict(mymodel, date[c(10,400,600),])#, type="prob")
matriceconfuzie <- table(date$risk_score, predict(mymodel))
matriceconfuzie
mean(date$risk_score==predict(mymodel))
#46.42% acuratete

date$transaction_type <-
  factor(date$transaction_type)
date$transaction_type
date$out <- relevel(date$transaction_type, ref="purchase")
#install.packages("nnet")
library(nnet)
mymodel<-multinom(out~amount+login_frequency+session_duration, data=date, 
                  trace=FALSE)
summary(mymodel)#AIC: 1498.257
exp(coef(mymodel))
p1 <- predict(mymodel,date)
p1
predict(mymodel,date,type="prob")
predict(mymodel, date[c(10,400,600),])#, type="prob")
matriceconfuzie <- table(date$transaction_type, predict(mymodel))
matriceconfuzie
mean(date$transaction_type==predict(mymodel))
#50.31% acuratete

date$transaction_type <-
  factor(date$transaction_type)
date$transaction_type
date$out <- relevel(date$transaction_type, ref="scam")
#install.packages("nnet")
library(nnet)
mymodel<-multinom(out~amount+login_frequency+session_duration+risk_score, data=date, 
                  trace=FALSE)
summary(mymodel) 
exp(coef(mymodel))
p1 <- predict(mymodel,date)
p1
predict(mymodel,date,type="prob")
predict(mymodel, date[c(10,400,600),])#, type="prob")
matriceconfuzie <- table(date$transaction_type, predict(mymodel))
matriceconfuzie
mean(date$transaction_type==predict(mymodel))
#92.23% acuratete

date$purchase_pattern <-
  factor(date$purchase_pattern)
date$purchase_pattern
date$out <- relevel(date$purchase_pattern, ref="focused")
#install.packages("nnet")
library(nnet)
mymodel<-multinom(out~amount+login_frequency+session_duration+risk_score, data=date, 
                  trace=FALSE)
summary(mymodel)#AIC: 80.00008 
exp(coef(mymodel))
p1 <- predict(mymodel,date)
p1
predict(mymodel,date,type="prob")
predict(mymodel, date[c(10,400,600),])#, type="prob")
matriceconfuzie <- table(date$purchase_pattern, predict(mymodel))
matriceconfuzie
mean(date$purchase_pattern==predict(mymodel))
#100% acuratete

date$age_group <-
  factor(date$age_group)
date$purchase_pattern
date$out <- relevel(date$age_group, ref="established")
#install.packages("nnet")
library(nnet)
mymodel<-multinom(out~amount+login_frequency+session_duration+risk_score, data=date, 
                  trace=FALSE)
summary(mymodel)#AIC: 80.00008 
exp(coef(mymodel))
p1 <- predict(mymodel,date)
p1
predict(mymodel,date,type="prob")
predict(mymodel, date[c(10,400,600),])#, type="prob")
matriceconfuzie <- table(date$age_group, predict(mymodel))
matriceconfuzie
mean(date$age_group==predict(mymodel))
#100% acuratete

#Arbori de regresie si de clasificare. Curatarea arborelui. 
#Arborele de regresie l-am facut intr-un alt script

#Arbori de clasificare
library(rpart.plot)
library(tree)
library(ISLR)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score")]
date_n
date_n$risk_score<-ifelse(risk_score<=median(risk_score),'Low','High')
date_n$risk_score<-factor(date_n$risk_score)

set.seed(111)
library(caret)
#vom imparti in antrenare 60% si testare 40% folosind createDataPartition

split <-createDataPartition(y=date_n$risk_score, p=0.6, list=FALSE)
split

#extragem setul de antrenare
train <-date_n[split,]
train

#extragem setul de testare
test <-date_n[-split,]
test


arbore<-tree(risk_score~., data=train)
plot(arbore)
text(arbore, pretty=0)

#Predictie pentru setul de testare:

pred<-predict(arbore, test, type='class')
pred

mean(pred==test$risk_score)
#avem o acuratete de 100% (100% din datele de testare au fost etichetate in mod corect)
confusionMatrix <-table(pred, test$risk_score)
confusionMatrix

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
date<-Metaverse_transactions

date$transaction_type<-factor(date$transaction_type)

set.seed(111)
library(caret)
#vom imparti in antrenare 60% si testare 40% folosind createDataPartition

split <-createDataPartition(y=date$transaction_type, p=0.6, list=FALSE)
split

#extragem setul de antrenare
train <-date[split,]
train

#extragem setul de testare
test <-date[-split,]
test


arbore<-tree(transaction_type~., data=train)
plot(arbore)
text(arbore, pretty=0)

#Predictie pentru setul de testare:

pred<-predict(arbore, test, type='class')
pred

mean(pred==test$transaction_type)
#avem o acuratete de 99.21% (99.21% din datele de testare au fost etichetate in mod corect)
confusionMatrix <-table(pred, test$transaction_type)
confusionMatrix

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
date<-Metaverse_transactions

date$age_group<-factor(date$age_group)

set.seed(111)
library(caret)
#vom imparti in antrenare 60% si testare 40% folosind createDataPartition

split <-createDataPartition(y=date$age_group, p=0.6, list=FALSE)
split

#extragem setul de antrenare
train <-date[split,]
train

#extragem setul de testare
test <-date[-split,]
test


arbore<-tree(age_group~., data=train)
plot(arbore)
text(arbore, pretty=0)

#Predictie pentru setul de testare:

pred<-predict(arbore, test, type='class')
pred

mean(pred==test$age_group)
#avem o acuratete de 100% (100% din datele de testare au fost etichetate in mod corect)
confusionMatrix <-table(pred, test$age_group)
confusionMatrix

Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
date<-Metaverse_transactions

date$login_frequency<-factor(date$login_frequency)

set.seed(111)
library(caret)
#vom imparti in antrenare 60% si testare 40% folosind createDataPartition

split <-createDataPartition(y=date$login_frequency, p=0.6, list=FALSE)
split

#extragem setul de antrenare
train <-date[split,]
train

#extragem setul de testare
test <-date[-split,]
test


arbore<-tree(login_frequency~., data=train)
plot(arbore)
text(arbore, pretty=0)

#Predictie pentru setul de testare:

pred<-predict(arbore, test, type='class')
pred

mean(pred==test$login_frequency)
#avem o acuratete de 43.13% (100% din datele de testare au fost etichetate in mod corect)
confusionMatrix <-table(pred, test$login_frequency)
confusionMatrix

#Imbunatatirea acuratetei predictiei utilizand validarea incrucisata

set.seed(12)
cv.tree1 <-cv.tree(arbore, FUN=prune.misclass)

#la fel ca mai sus, putem avea size sau dev
cv.tree1$size

cv.tree1$dev

plot(cv.tree1$size, cv.tree1$dev, type='b')

arbore1<-prune.misclass(arbore, best=4)
plot(arbore1)
text(arbore1, pretty=0)

prune.predictie <-predict(arbore1, test, type='class')
prune.predictie

mean(prune.predictie==test$risk_score)

#acuratetea modelului este de 43.13%
confusionMatrixCV<-table(prune.predictie, test$login_frequency)
confusionMatrixCV

