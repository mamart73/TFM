#-----1.1. Librerias-----
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
library(dummies)
library(MASS)
library(reshape)
library(explore)
library(imbalance)
#-----1.2. Importamos dataset nhs----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nhs <- readRDS("nhs_estan.rds")

#eliminamos id
nhs$id <- NULL
#-----2. SVM lineal- B?squeda mejores parametros-----
#-----2.1. Mejores par?metros para selecci?n AIC-----
set.seed(1234)
svm_statistics <- data.frame(cost = integer(),
                            accuracy    = double(),
                            auc         = double(),
                            sensitivity = double(),
                            specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

    for (cost in c(0.01,0.015,0.02,0.03,0.04,0.05))
    {
      set.seed(1234)
      print(cost)
      cat("/n")
      #Definimos el grid de par?metros
      SVMgrid<-expand.grid(C=cost)
      
      SVM_AIC<- train(data=nhs,diabetes ~ age + weight + healthgen.Fair + race1.White + 
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
                      method="svmLinear",trControl=control,
                      tuneGrid=SVMgrid,verbose=FALSE) 
      
      #prediccion
      sal <- SVM_AIC$pred
      #matriz de confusion (prediccion, valor real, resultado positivo)
      salconfu <- confusionMatrix(sal$pred,sal$obs,positive = "Yes")
      #curva roc
      curvaroc <- roc(response=sal$obs,predictor=sal$Yes)
      #estadisticos
      accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
      auc <- round(curvaroc$auc,4)
      sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
      specificity  <- round(salconfu[["byClass"]][["Specificity"]],4)
      #insertamos en dataframe
      svm_statistics[nrow(svm_statistics) + 1,] <- c(cost,accuracy,auc,sensitivity,specificity)
    }

ggplot(data=svm_statistics, aes(x=cost, y=sensitivity))+
  geom_point()

#Elijo los dos mejores parametros con seleccion AIC
#C=0.015 y C=0.03

#-----2.2. Mejores par?metros para selecci?n BIC-----
set.seed(1234)
svm_statistics <- data.frame(cost = integer(),
                             accuracy    = double(),
                             auc         = double(),
                             sensitivity = double(),
                             specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (cost in c(0.01,0.015,0.02,0.03,0.04,0.05))
{
  set.seed(1234)
  print(cost)
  cat("/n")
  #Definimos el grid de par?metros
  SVMgrid<-expand.grid(C=cost)
  
  SVM_BIC<- train(data=nhs,diabetes ~ age+weight+healthgen.Fair+race1.White+ 
                    totchol+healthgen.Vgood+harddrugs.NotAsked+directchol+
                    sleeptrouble.Yes+pulse+height+sexnumpartnlife.6to10+
                    surveyyr.2009_10+sexnumpartyear.None+smokenow.No+alcohol12plusyr.No+
                    bpsys+bpdia+maritalstatus.Separated+agefirstmarij.18orMore+
                    smokeage.NA+education.High_School,
                  method="svmLinear",trControl=control,
                  tuneGrid=SVMgrid,verbose=FALSE) 
  
  #prediccion
  sal <- SVM_BIC$pred
  #matriz de confusion (prediccion, valor real, resultado positivo)
  salconfu <- confusionMatrix(sal$pred,sal$obs,positive = "Yes")
  #curva roc
  curvaroc <- roc(response=sal$obs,predictor=sal$Yes)
  #estadisticos
  accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
  auc <- round(curvaroc$auc,4)
  sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
  specificity  <- round(salconfu[["byClass"]][["Specificity"]],4)
  #insertamos en dataframe
  svm_statistics[nrow(svm_statistics) + 1,] <- c(cost,accuracy,auc,sensitivity,specificity)
}

ggplot(data=svm_statistics, aes(x=cost, y=sensitivity))+
  geom_point()

#Elijo los dos mejores parametros con seleccion BIC
#C=0.01 y C=0.04
#(a igualdad de bondad de ajuste entre costes, tomamos el menor)

#-----3. Funcion vcrep SVM lineal-----

cruzadasvmbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,C=0.01)
  { 
    # Preparacion del archivo
    # pasar las categoricas a dummies
    
    if  (listclass!=c(""))
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # estandarizar las variables continuas
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C)
    
    SVM<- train(formu,data=databis,
                method="svmLinear",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    sensibilidad<-function(x,y) {
      confu<-confusionMatrix(x,y)
      sensib<-confu[[4]][1]
      return(sensib)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    #-------------------------------------
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repeticion de cv 
    # Definimnos funcion
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc#curvaroc[9]
      auc<-as.numeric(auc)
      # auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
    
    #----------------------------------------
    # medias<-preditest %>%
    #   group_by(Rep) %>%
    #   summarize(tasa=1-tasafallos(pred,obs))
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      auc<-as.numeric(auc)
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }

#-----4. SVM polinomico- B?squeda mejores parametros----
#-----4.1. Mejores par?metros para selecci?n AIC----

set.seed(1234)
svm_poly_statistics <- data.frame(cost = integer(),
                                  grad = integer(),
                                  scl = integer(),
                             accuracy    = double(),
                             auc         = double(),
                             sensitivity = double(),
                             specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (cost in c(0.01,0.015,0.02,0.03,0.04,0.05))
{
  for (grad in c(2,3))
  {
    for (scl in c(0.05,0.1,0.2,0.5,1,2))
    {
  set.seed(1234)
  print(cost)
  print(grad)
  print(scl)
  cat("/n")
  
  #Definimos el grid de par?metros
  SVMgridpol<-expand.grid(C=cost,degree=grad,scale=scl)
  
  SVMpol_AIC <- train(data=nhs,diabetes ~ age + weight + healthgen.Fair + race1.White + 
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
    
  #prediccion
  sal <- SVMpol_AIC$pred
  #matriz de confusion (prediccion, valor real, resultado positivo)
  salconfu <- confusionMatrix(sal$pred,sal$obs,positive = "Yes")
  #curva roc
  curvaroc <- roc(response=sal$obs,predictor=sal$Yes)
  #estadisticos
  accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
  auc <- round(curvaroc$auc,4)
  sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
  specificity  <- round(salconfu[["byClass"]][["Specificity"]],4)
  #insertamos en dataframe
  svm_poly_statistics[nrow(svm_poly_statistics) + 1,] <- c(cost,grad,scl,
                                                           accuracy,auc,sensitivity,specificity)
    }
  }
}

ggplot(svm_poly_statistics, aes(x=factor(cost), y=sensitivity, 
                                color=factor(scl),pch=factor(grad))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de parámetros con selección de variables AIC") +
  theme(text = element_text(family = "lato", size=12),
      legend.title=element_text(family='lato')) +
  labs(x='cost',color='scale', pch='degree')

setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
#saveRDS(svm_poly_statistics, file = "svm_pol_statistics_aic.rds")
svm_poly_statistics <- readRDS("svm_pol_statistics_aic.rds")

#Elijo los dos mejores parametros con seleccion AIC
#C=0.01=, degree=2 y scale=3
#C=0.01, degree=1 y scale=3


#-----4.2. Mejores par?metros para selecci?n BIC-----
set.seed(1234)
svm_poly_statistics <- data.frame(cost = integer(),
                                  grad = integer(),
                                  scl = integer(),
                                  accuracy    = double(),
                                  auc         = double(),
                                  sensitivity = double(),
                                  specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (cost in c(0.01,0.015,0.02,0.03,0.04,0.05))
{
  for (grad in c(2,3))
  {
    for (scl in c(0.05,0.1,0.2,0.5,1,2))
    {
      set.seed(1234)
      print(cost)
      print(grad)
      print(scl)
      cat("/n")
      
      #Definimos el grid de par?metros
      SVMgridpol<-expand.grid(C=cost,degree=grad,scale=scl)
      
      SVMpol_BIC <- train(data=nhs,diabetes ~ age+weight+healthgen.Fair+race1.White+ 
                            totchol+healthgen.Vgood+harddrugs.NotAsked+directchol+
                            sleeptrouble.Yes+pulse+height+sexnumpartnlife.6to10+
                            surveyyr.2009_10+sexnumpartyear.None+smokenow.No+alcohol12plusyr.No+
                            bpsys+bpdia+maritalstatus.Separated+agefirstmarij.18orMore+
                            smokeage.NA+education.High_School,
                          method="svmPoly",trControl=control,
                          tuneGrid=SVMgridpol,verbose=FALSE)
      
      #prediccion
      sal <- SVMpol_BIC$pred
      #matriz de confusion (prediccion, valor real, resultado positivo)
      salconfu <- confusionMatrix(sal$pred,sal$obs,positive = "Yes")
      #curva roc
      curvaroc <- roc(response=sal$obs,predictor=sal$Yes)
      #estadisticos
      accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
      auc <- round(curvaroc$auc,4)
      sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
      specificity  <- round(salconfu[["byClass"]][["Specificity"]],4)
      #insertamos en dataframe
      svm_poly_statistics[nrow(svm_poly_statistics) + 1,] <- c(cost,grad,scl,
                                                               accuracy,auc,sensitivity,specificity)
    }
  }
}

ggplot(svm_poly_statistics, aes(x=factor(cost), y=sensitivity, 
                                color=factor(scl),pch=factor(grad))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de parámetros con selección de variables BIC") +
  theme(text = element_text(family = "lato", size=12),
        legend.title=element_text(family='lato')) +
  labs(x='cost',color='scale', pch='degree')

#setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
#saveRDS(svm_poly_statistics, file = "svm_pol_statistics_bic.rds")
svm_poly_statistics <- readRDS("svm_pol_statistics_bic.rds")

#Elijo los dos mejores parametros con seleccion BIC
#C=0.01, degree=2 y scale=3
#C=0.015, degree=2 y scale=3

#-----6. Funcion vcrep SVM polinomico----

cruzadasvmpolybin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5, C=1,degree=2,scale=1)
  { 
    # Preparacion del archivo
    # pasar las categoricas a dummies
    
    if  (listclass!=c(""))
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # estandarizar las variables continuas
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste("factor(",vardep,")~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
    
    SVM<- train(formu,data=databis,
                method="svmPoly",trControl=control,
                tuneGrid=SVMgrid,verbose=FALSE)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    sensibilidad<-function(x,y) {
      confu<-confusionMatrix(x,y)
      sensib<-confu[[4]][1]
      return(sensib)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    #-------------------------------------
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repeticion de cv 
    # Definimnos funcion
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc#curvaroc[9]
      auc<-as.numeric(auc)
      # auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
    
    #----------------------------------------
    # medias<-preditest %>%
    #   group_by(Rep) %>%
    #   summarize(tasa=1-tasafallos(pred,obs))
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      auc<-as.numeric(auc)
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }
#-----4. Mejor SVM-----
#Lineal AIC con C=0.015
set.seed(1234)
cruzadasvm_aic015<-cruzadasvmbin(data=nhs,vardep="diabetes",
                                 listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                             "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                             "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                             "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                             "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                             "smokeage.NA", "education.High_School", "daysmenthlthbad.Few",
                                             "sexage.15orLess", "sexnumpartyear.MoreThan1", "marij.NotAsked",
                                             "comphrsday.0_hrs", "age1stbaby.22orMore", "smokenow.Yes", "smoke100.No",
                                             "tvhrsday.from4hr", "race1.Hispanic", "race1.Mexican", "homerooms",
                                             "samesex.Yes", "sexnumpartnlife.MoreThan25", "sexnumpartnlife.11to25",
                                             "poverty", "urinevol1", "ageregmarij.Before18", "marij.Never",
                                             "daysmenthlthbad.NA", "littleinterest.NA", "depressed.NA", "alcoholyear",
                                             "nbabies.NA", "maritalstatus.Married", "education.Some_College",
                                             "race1.Black", "nbabies.MoreThan2", "littleinterest.Several",
                                             "littleinterest.None"),listclass="",
                                 grupos=4,sinicio=1234,repe=10,C=0.015)

cruzadasvm_aic015$modelo="svmlin_aic015"

#Lineal AIC con C=0.03
set.seed(1234)
cruzadasvm_aic03<-cruzadasvmbin(data=nhs,vardep="diabetes",
                                listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                            "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                            "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                            "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                            "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                            "smokeage.NA", "education.High_School", "daysmenthlthbad.Few",
                                            "sexage.15orLess", "sexnumpartyear.MoreThan1", "marij.NotAsked",
                                            "comphrsday.0_hrs", "age1stbaby.22orMore", "smokenow.Yes", "smoke100.No",
                                            "tvhrsday.from4hr", "race1.Hispanic", "race1.Mexican", "homerooms",
                                            "samesex.Yes", "sexnumpartnlife.MoreThan25", "sexnumpartnlife.11to25",
                                            "poverty", "urinevol1", "ageregmarij.Before18", "marij.Never",
                                            "daysmenthlthbad.NA", "littleinterest.NA", "depressed.NA", "alcoholyear",
                                            "nbabies.NA", "maritalstatus.Married", "education.Some_College",
                                            "race1.Black", "nbabies.MoreThan2", "littleinterest.Several",
                                            "littleinterest.None"),listclass="",
                                grupos=4,sinicio=1234,repe=10,C=0.03) 

cruzadasvm_aic03$modelo="svmlin_aic03"

#Lineal BIC con C=0.01
set.seed(1234)
cruzadasvm_bic01<-cruzadasvmbin(data=nhs,vardep="diabetes",
                                listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                            "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                            "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                            "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                            "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                            "smokeage.NA", "education.High_School"), listclass="",
                                grupos=4,sinicio=1234,repe=10,C=0.01) 

cruzadasvm_bic01$modelo="svmlin_bic01"

#Lineal BIC con C=0.04
set.seed(1234)
cruzadasvm_bic04<-cruzadasvmbin(data=nhs,vardep="diabetes",
                                listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                            "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                            "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                            "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                            "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                            "smokeage.NA", "education.High_School"),listclass="",
                                grupos=4,sinicio=1234,repe=10,C=0.04) 

cruzadasvm_bic04$modelo="svmlin_bic04"

#Polinomial AIC con C=0.01, degree=3 y scale=2
set.seed(1234)
cruzadasvmpol_aic_sca2<-cruzadasvmpolybin(data=nhs,vardep="diabetes",
                                 listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                             "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                             "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                             "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                             "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                             "smokeage.NA", "education.High_School", "daysmenthlthbad.Few",
                                             "sexage.15orLess", "sexnumpartyear.MoreThan1", "marij.NotAsked",
                                             "comphrsday.0_hrs", "age1stbaby.22orMore", "smokenow.Yes", "smoke100.No",
                                             "tvhrsday.from4hr", "race1.Hispanic", "race1.Mexican", "homerooms",
                                             "samesex.Yes", "sexnumpartnlife.MoreThan25", "sexnumpartnlife.11to25",
                                             "poverty", "urinevol1", "ageregmarij.Before18", "marij.Never",
                                             "daysmenthlthbad.NA", "littleinterest.NA", "depressed.NA", "alcoholyear",
                                             "nbabies.NA", "maritalstatus.Married", "education.Some_College",
                                             "race1.Black", "nbabies.MoreThan2", "littleinterest.Several",
                                             "littleinterest.None"),listclass="",
                                 grupos=4,sinicio=1234,repe=10,C=0.01,degree=3,scale=2)

cruzadasvmpol_aic_sca2$modelo="svmpol_aic2"

#Polinomial AIC con C=0.01, degree=3 y scale=1
set.seed(1234)
cruzadasvmpol_aic_sca1<-cruzadasvmpolybin(data=nhs,vardep="diabetes",
                                          listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                                      "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                                      "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                                      "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                                      "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                                      "smokeage.NA", "education.High_School", "daysmenthlthbad.Few",
                                                      "sexage.15orLess", "sexnumpartyear.MoreThan1", "marij.NotAsked",
                                                      "comphrsday.0_hrs", "age1stbaby.22orMore", "smokenow.Yes", "smoke100.No",
                                                      "tvhrsday.from4hr", "race1.Hispanic", "race1.Mexican", "homerooms",
                                                      "samesex.Yes", "sexnumpartnlife.MoreThan25", "sexnumpartnlife.11to25",
                                                      "poverty", "urinevol1", "ageregmarij.Before18", "marij.Never",
                                                      "daysmenthlthbad.NA", "littleinterest.NA", "depressed.NA", "alcoholyear",
                                                      "nbabies.NA", "maritalstatus.Married", "education.Some_College",
                                                      "race1.Black", "nbabies.MoreThan2", "littleinterest.Several",
                                                      "littleinterest.None"),listclass="",
                                          grupos=4,sinicio=1234,repe=10,C=0.01,degree=3,scale=1)

cruzadasvmpol_aic_sca1$modelo="svmpol_aic1"

#Polinomial BIC con C=0.01, degree=3 y scale=2
set.seed(1234)
cruzadasvmpol_bic_c01<-cruzadasvmpolybin(data=nhs,vardep="diabetes",
                                          listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                                      "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                                      "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                                      "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                                      "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                                      "smokeage.NA", "education.High_School"),listclass="",
                                          grupos=4,sinicio=1234,repe=10,C=0.01,degree=3,scale=2)

cruzadasvmpol_bic_c01$modelo="svmpol_bic01"

#Polinomial BIC con C=0.015, degree=3 y scale=2
set.seed(1234)
cruzadasvmpol_bic_c015<-cruzadasvmpolybin(data=nhs,vardep="diabetes",
                                          listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                                      "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                                      "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                                      "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                                      "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                                      "smokeage.NA", "education.High_School"),listclass="",
                                          grupos=4,sinicio=1234,repe=10,C=0.015,degree=3,scale=2)

cruzadasvmpol_bic_c015$modelo="svmpol_bic015"

#Comparacion mejor SVM
unionsvm <- rbind(cruzadasvm_aic015, cruzadasvm_aic03, cruzadasvm_bic01, cruzadasvm_bic04, 
                     cruzadasvmpol_aic_sca2, cruzadasvmpol_aic_sca1, cruzadasvmpol_bic_c01, cruzadasvmpol_bic_c015)

par(cex.axis=1, family='lato')
boxplot(data=unionsvm, sen~modelo, col='#9698ed', main="SENSIBILIDAD")
boxplot(data=unionsvm, auc~modelo, col='#9698ed', main="AUC")


#Guardo los modelos 
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
#saveRDS(unionsvm, file = "unionsvm.rds")
