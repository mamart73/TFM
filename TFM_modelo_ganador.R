#-----MODELOS GANADORES Y PREDICCION EN TEST-----
#-----1. Librerias-----
rm(list = ls())
library(skimr)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(showtextdb)
library(extrafont)
library(ggplot2)
library(ggthemes)
library(showtext)
library(patchwork)
library(forcats)
library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)
library(caret)
library(MASS)
library(reshape)
library(explore)
library(imbalance)

#-----2. Importamos datasets arbol y rf----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nhs <- readRDS("nhs.rds") #conjunto train
test_nhs <- readRDS("test_nhs.rds") #conjunto test
#eliminamos id
nhs$id <- NULL
test_nhs$id <- NULL

#-----3. Modelos ganadores----
#-----3.1. Mejor arbol de decision-----

#Mejor arbol con minbucket=1150
set.seed(1234)
control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 
arbolgrid <- expand.grid(cp=c(0))
best_arbol <- train(diabetes ~ ., data=nhs,
                    method="rpart",
                    trControl=control,
                    tuneGrid=arbolgrid,
                    control=rpart.control(minbucket = 1150))

#Prediccion arbol
pred_arbol <- predict(best_arbol, newdata = test_nhs, type = "prob")
pred_arbol <- as.data.frame(pred_arbol) 
pred_arbol_yes <- pred_arbol %>% dplyr::select(Yes)

fresa <- cbind(test_nhs[, c(1:109)], pred_arbol_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.7766
plot(roc_curve)

#A partir de probabilidad 0.5 se clasifica
pred_arbol$diabetespredclass <- predict(best_arbol, newdata = test_nhs, type = "raw")
conf_mat <- confusionMatrix(pred_arbol$diabetespredclass,test_nhs$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)

accuracy
sensitivity
specificity

conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat


#-----3.2. Mejor random forest----

#El mejor modelo de random forest es samplesize=2140, nodesize=107 y mtry=108
nhs <- readRDS("nhs.rds")
nhs$id <- NULL
set.seed(1234)
control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 
rfgrid<-expand.grid(mtry=108)
best_rf <- train(data=nhs,diabetes ~.,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,sample_size=2140,ntree=600,
           nodesize=107,replace=TRUE)

#Prediccion random forest
pred_rf <- predict(best_rf, newdata = test_nhs, type = "prob")
pred_rf <- as.data.frame(pred_rf) 
pred_rf_yes <- pred_rf %>% dplyr::select(Yes)

fresa <- cbind(test_nhs[, c(1:109)], pred_rf_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.8179
plot(roc_curve)

pred_rf$diabetespredclass <- predict(best_rf, newdata = test_nhs, type = "raw")
conf_mat <- confusionMatrix(pred_rf$diabetespredclass,test_nhs$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)


conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat


#-----Importamos nuevos datasets-----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
test_nhs <- readRDS("test_nhs.rds") #conjunto test
nhs <- readRDS("nhs.rds") #conjunto train
#eliminamos id
test_nhs$id <- NULL
nhs$id <- NULL

#para evaluar el conjunto test para cada uno de nuestros modelos ganadores de 
#regresión logística, redes neuronales y máquina de vector soporte se debe
#estandarizar, dado que los modelos han sido entrenados con el datset estandarizado

#estandarizamos variables numericas no dummies
objetivo <- c("diabetes")
numericas <- c("age","poverty","homerooms","weight","height","pulse",
               "directchol","totchol","urinevol1","urineflow1",
               "sleephrsnight","physactivedays","alcoholyear","bpdia","bpsys")
categoricas <- c(
  'surveyyr.2009_10','gender.male','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','education.8th_Grade','education.9_11th_Grade',
  'education.College_Grad','education.High_School','education.Some_College',
  'maritalstatus.Married','maritalstatus.NeverMarried','maritalstatus.Separated',
  'homeown.Own','work.Working','healthgen.Fair',
  'healthgen.Good','healthgen.Vgood','daysphyshlthbad.Few','daysphyshlthbad.Many',
  'daysphyshlthbad.NA','daysphyshlthbad.None','daysmenthlthbad.Few',
  'daysmenthlthbad.Many','daysmenthlthbad.NA','daysmenthlthbad.None',
  'littleinterest.NA','littleinterest.None','littleinterest.Several',
  'depressed.NA','depressed.None','depressed.Several','npregnancies.1or2',
  'npregnancies.MoreThan2','npregnancies.NA','nbabies.0to2','nbabies.MoreThan2',
  'nbabies.NA','age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA',
  'sleeptrouble.Yes','tvhrsday.0to1hr','tvhrsday.2to3hr','tvhrsday.from4hr',
  'comphrsday.0_hrs','comphrsday.0_to_1_hr','comphrsday.from1hr',
  'alcohol12plusyr.No','alcohol12plusyr.Yes','alcoholday.1',
  'alcoholday.2or3','alcoholday.NA','smokenow.No','smokenow.Yes',
  'smoke100.No','smoke100.Yes','smokeage.18orMore','smokeage.Before18',
  'smokeage.NA','agefirstmarij.18orMore','agefirstmarij.Before18',
  'agefirstmarij.NA','ageregmarij.18orMore','ageregmarij.Before18',
  'ageregmarij.NotApply','harddrugs.NA','harddrugs.No','harddrugs.NotAsked',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','marij.HasTried','marij.MarijUser',
  'marij.Never','marij.NotAsked')

#estandarizamos el test en base a estadisticos train
means <- apply(nhs[,numericas],2,mean,na.rm=TRUE)
sds <- sapply(nhs[,numericas],sd,na.rm=TRUE)
numericas_test_estan <- scale(test_nhs[,numericas],center=means,scale=sds)

test_nhs_estan <- data.frame(cbind(numericas_test_estan,test_nhs[,c(categoricas,objetivo)]))
#-----3.3. Mejor regresion logistica----

#El mejor modelo de regresion logistica es con seleccion de variables AIC
nhs <- readRDS("nhs_estan.rds")
set.seed(1234)
control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 

best_glm<- train(data=nhs, diabetes ~ age + weight + healthgen.Fair + race1.White + totchol + 
                  healthgen.Vgood + harddrugs.NotAsked + directchol + sleeptrouble.Yes + 
                  pulse + height + sexnumpartnlife.6to10 + surveyyr.2009_10 + 
                  sexnumpartyear.None + smokenow.No + alcohol12plusyr.No + 
                  bpsys + bpdia + maritalstatus.Separated + agefirstmarij.18orMore + 
                  smokeage.NA + education.High_School + daysmenthlthbad.Few + 
                  sexage.15orLess + sexnumpartyear.MoreThan1 + marij.NotAsked + 
                  comphrsday.0_hrs + age1stbaby.22orMore + smokenow.Yes + smoke100.No + 
                  tvhrsday.from4hr + race1.Hispanic + race1.Mexican + homerooms + 
                  samesex.Yes + sexnumpartnlife.MoreThan25 + sexnumpartnlife.11to25 + 
                  poverty + urinevol1 + ageregmarij.Before18 + marij.Never + 
                  daysmenthlthbad.NA + littleinterest.NA + depressed.NA + alcoholyear + 
                  nbabies.NA + maritalstatus.Married + education.Some_College + 
                  race1.Black + nbabies.MoreThan2 + littleinterest.Several + 
                  littleinterest.None,
                method="glm",trControl=control)

#Prediccion regresion logistica
pred_glm <- predict(best_glm, newdata = test_nhs_estan, type = "prob")
pred_glm <- as.data.frame(pred_glm) 
pred_glm_yes <- pred_glm %>% dplyr::select(Yes)

fresa <- cbind(test_nhs_estan[, c(1:109)], pred_glm_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.8243
plot(roc_curve)

pred_glm$diabetespredclass <- predict(best_glm, newdata = test_nhs_estan, type = "raw")
conf_mat <- confusionMatrix(pred_glm$diabetespredclass,test_nhs_estan$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)

conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat


#-----3.4. Mejor red neuronal-----
#El mejor modelo de red neuronal es con size=30, decay=0.001, iter=300
#y seleccion de variables BIC
nhs <- readRDS("nhs_estan.rds")

set.seed(1234)
control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")
nnetgrid<-expand.grid(size=30,decay=0.001,bag=F)

best_rednnet<- train(diabetes ~ age+weight+healthgen.Fair+race1.White+ 
                       totchol+healthgen.Vgood+harddrugs.NotAsked+directchol+
                       sleeptrouble.Yes+pulse+height+sexnumpartnlife.6to10+
                       surveyyr.2009_10+sexnumpartyear.None+smokenow.No+alcohol12plusyr.No+
                       bpsys+bpdia+maritalstatus.Separated+agefirstmarij.18orMore+
                       smokeage.NA+education.High_School,
                     data=nhs,method="avNNet",linout = TRUE,maxit=300,
                     trControl=control,tuneGrid=nnetgrid,trace=TRUE)

#Prediccion red neuronal
pred_rednnet <- predict(best_rednnet, newdata = test_nhs_estan, type = "prob")
pred_rednnet <- as.data.frame(pred_rednnet) 
pred_rednnet_yes <- pred_rednnet %>% dplyr::select(Yes)

fresa <- cbind(test_nhs_estan[, c(1:109)], pred_rednnet_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.8102
plot(roc_curve)

pred_rednnet$diabetespredclass <- predict(best_rednnet, newdata = test_nhs_estan, type = "raw")
conf_mat <- confusionMatrix(pred_rednnet$diabetespredclass,test_nhs_estan$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)

conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat



conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat

conf_mat <- confusionMatrix(pred_rednnet$diabetespredclass,test_nhs_estan$diabetes, positive = "Yes")
conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat

#-----3.5 Mejor máquina de vectoe soporte-----
#El mejor modelo de red neuronal es con C=0.01, degree=3, scale=1
#y seleccion de variables AIC
nhs <- readRDS("nhs_estan.rds")
set.seed(1234)

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")
SVMgridpol<-expand.grid(C=0.01,degree=3,scale=1)

best_svm <- train(data=nhs,diabetes ~ age + weight + healthgen.Fair + race1.White + 
                      totchol + healthgen.Vgood + harddrugs.NotAsked + directchol + 
                      sleeptrouble.Yes + pulse + height + sexnumpartnlife.6to10 + 
                      surveyyr.2009_10 + sexnumpartyear.None + smokenow.No + alcohol12plusyr.No + 
                      bpsys + bpdia + maritalstatus.Separated + agefirstmarij.18orMore + 
                      smokeage.NA + education.High_School + daysmenthlthbad.Few + 
                      sexage.15orLess + sexnumpartyear.MoreThan1 + marij.NotAsked + 
                      comphrsday.0_hrs + age1stbaby.22orMore + smokenow.Yes + smoke100.No + 
                      tvhrsday.from4hr + race1.Hispanic + race1.Mexican + homerooms + 
                      samesex.Yes + sexnumpartnlife.MoreThan25 + sexnumpartnlife.11to25 + 
                      poverty + urinevol1 + ageregmarij.Before18 + marij.Never + 
                      daysmenthlthbad.NA + littleinterest.NA + depressed.NA + alcoholyear + 
                      nbabies.NA + maritalstatus.Married + education.Some_College + 
                      race1.Black + nbabies.MoreThan2 + littleinterest.Several + 
                      littleinterest.None,
                    method="svmPoly",trControl=control,
                    tuneGrid=SVMgridpol,verbose=FALSE)

#Prediccion svm
pred_svm <- predict(best_svm, newdata = test_nhs_estan, type = "prob")
pred_svm <- as.data.frame(pred_svm) 
pred_svm_yes <- pred_svm %>% dplyr::select(Yes)

fresa <- cbind(test_nhs_estan[, c(1:109)], pred_svm_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.6695
plot(roc_curve)

pred_svm$diabetespredclass <- predict(best_svm, newdata = test_nhs_estan, type = "raw")
 
conf_mat <- confusionMatrix(pred_svm$diabetespredclass,test_nhs_estan$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)

conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat

#otro modelo de svm
#El mejor modelo de red neuronal es con C=0.01, degree=3, scale=2
#y seleccion de variables BIC
nhs <- readRDS("nhs_estan.rds")
set.seed(1234)

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")
SVMgridpol<-expand.grid(C=0.01,degree=3,scale=2)

best_svm <- train(data=nhs,diabetes ~ age+weight+healthgen.Fair+race1.White+ 
                    totchol+healthgen.Vgood+harddrugs.NotAsked+directchol+
                    sleeptrouble.Yes+pulse+height+sexnumpartnlife.6to10+
                    surveyyr.2009_10+sexnumpartyear.None+smokenow.No+alcohol12plusyr.No+
                    bpsys+bpdia+maritalstatus.Separated+agefirstmarij.18orMore+
                    smokeage.NA+education.High_School,
                  method="svmPoly",trControl=control,
                  tuneGrid=SVMgridpol,verbose=FALSE)

#Prediccion svm
pred_svm <- predict(best_svm, newdata = test_nhs_estan, type = "prob")
pred_svm <- as.data.frame(pred_svm) 
pred_svm_yes <- pred_svm %>% dplyr::select(Yes)

fresa <- cbind(test_nhs_estan[, c(1:109)], pred_svm_yes)
fresa <- fresa %>% dplyr::select(diabetes, Yes)

roc_curve <- roc(response=fresa$diabetes,predictor=fresa$Yes)
roc_curve[["auc"]]
#Area under the curve: 0.6695
plot(roc_curve)

pred_svm$diabetespredclass <- predict(best_svm, newdata = test_nhs_estan, type = "raw")

conf_mat <- confusionMatrix(pred_svm$diabetespredclass,test_nhs_estan$diabetes, positive = "Yes")
conf_mat

accuracy <- round(conf_mat[["overall"]][["Accuracy"]],4)
sensitivity  <- round(conf_mat[["byClass"]][["Sensitivity"]],4)
specificity  <- round(conf_mat[["byClass"]][["Specificity"]],4)

conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat
