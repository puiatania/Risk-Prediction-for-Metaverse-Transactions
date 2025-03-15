library(readxl)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions
dateKNN <- date
dateANN <- date
den <- names(date)
den
date_n <- date[,den %in% c("amount","login_frequency","session_duration","risk_score")]
date_n
library(tree)
library(ISLR)
hist(date_n$session_duration)
#Vom aplica logaritm asupra salariului pentru a il face distribuit normal
date_n$session_duration <-log(date_n$session_duration)
hist(date_n$session_duration)
library(caret)
split<-createDataPartition(y=date_n$session_duration, p=0.5, list=FALSE)
split
train <-date_n[split,]
train
test<-date_n[-split,]
test
arbore <-tree(session_duration~., train)
plot(arbore)
text(arbore, pretty=0)
cv.trees <-cv.tree(arbore)
cv.trees$size
cv.trees$dev
plot(cv.trees$size, cv.trees$dev, type='b')
#conform graficului, minimul erorii se atinge pentru o marime a arborelui de 6 noduri 
#terminale. Vom construi arborele curatat:
arbore1 <-prune.tree(arbore, best=3)
#best=parametru care se egaleaza cu numarul de noduri terminale ale arborelui curatat 
#(6 in cazul nostru)
plot(arbore1)
text(arbore1, pretty=0)

#Vom folosi arborele curatat pentru a face predictii pe setul de testare
pred <-predict(arbore1, test)
pred

#vrem sa afisam predictia scorului critic logaritmat al fiecarui joc 

plot(pred, test$session_duration)

#print(length(test$Critic_Score))

#Eroarea de predictie o vom calcula ca medie a patratelor diferentelor dintre
#salariul previzionat si salariul real
mean((pred-test$session_duration)^2)
#24.70%
