library(neuralnet)
set.seed(1234567890)
library(readxl)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date1<-Metaverse_transactions[,-c(1,3,4,7,8,10)]
attach(date1)
View(date1)
index<-sample(2, nrow(date1), replace=TRUE, prob=c(0.5, 0.5))
index
setantrenare<-date1[index==1,]
setantrenare
settestare<-date1[index==2,]
settestare
retea <- neuralnet(high_risk~amount+login_frequency+session_duration+risk_score, setantrenare, 
                   hidden = 4, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)
plot(retea, rep = "best")
temp_test <- subset(settestare, select = c("amount", "login_frequency", "session_duration","risk_score"))
#View(temp_test)
#View(temp_test)
retea.results <- compute(retea, temp_test)
results <- data.frame(actual = settestare$high_risk, prediction = retea.results$net.result)
results
results$prediction <- round(results$prediction)
results
tab=table(settestare$high_risk, results$prediction)
tab
(200+9)/(200+3+1+9)
#install.packages("e1071")
library(e1071)
classAgreement(tab)
