library(FNN)
library(tidyverse)
library(caret)
set.seed(123)
library(readxl)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions

#vom elimina coloanele 1, 4 si 11:
date1<-date[,-c(1,4,11)]
View(date1)
#pentru transaction_type
training.sample <-date1$transaction_type %>% createDataPartition(p=0.8, list=FALSE)
training.sample

#setul de antrenare:
train.data <-date1[training.sample,]
train.data

#setul de testare:

test.data <-date1[-training.sample,]
test.data

model <-train(transaction_type~.,data=train.data, method='knn', trControl=trainControl("cv",number=10),
              preProcess=c('center','scale'),tuneLength=20)
model

#acuratetea este cea care ne determina modelul optim. In cazul nostru, acuratetea maxima
#este de 0.9132, indicand k=5 valoarea optima pentru numarul celor mai apropiat k vecini ai
#oricarei observatii

plot(model)

#observam ca pe Ox avem nmarul de vecini k
#iar pe Oy avem acuratetea modelului

#parametrul optim k care maximizeaza acuratetea modelului este:
model$bestTune #se obtine valoarea k=5 la iteratia 1 din 20

#predictii asupra setului de testare:

predicted.classes <-model %>% predict(test.data)
predicted.classes

#determinam rata de exactitate a modelului:
mean(predicted.classes == test.data$transaction_type)

#observam ca 92.91% din datele de testare au fost corect previzionate



library(FNN)
library(tidyverse)
library(caret)
set.seed(123)
library(readxl)
Metaverse_transactions <- read_excel("Metaverse_transactions.xlsx")
View(Metaverse_transactions)
date<-Metaverse_transactions

#vom elimina coloanele 1, 4 si 11:
date1<-date[,-c(1,4,11)]
View(date1)
#pentru anomaly
training.sample <-date1$anomaly %>% createDataPartition(p=0.8, list=FALSE)
training.sample

#setul de antrenare:
train.data <-date1[training.sample,]
train.data

#setul de testare:

test.data <-date1[-training.sample,]
test.data

model <-train(anomaly~.,data=train.data, method='knn', trControl=trainControl("cv",number=10),
              preProcess=c('center','scale'),tuneLength=20)
model

#acuratetea este cea care ne determina modelul optim. In cazul nostru, acuratetea maxima
#este de 0.99803, indicand k=11 valoarea optima pentru numarul celor mai apropiat k vecini ai
#oricarei observatii

plot(model)

#observam ca pe Ox avem nmarul de vecini k
#iar pe Oy avem acuratetea modelului

#parametrul optim k care maximizeaza acuratetea modelului este:
model$bestTune #se obtine valoarea k=11 la iteratia 4 din 20

#predictii asupra setului de testare:

predicted.classes <-model %>% predict(test.data)
predicted.classes

#determinam rata de exactitate a modelului:
mean(predicted.classes == test.data$anomaly)

#observam ca 100% din datele de testare au fost corect previzionate




