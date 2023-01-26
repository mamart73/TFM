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
#-----1.2. Importamos dataset con dummies-----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nhs <- readRDS("nhs_estan.rds")

nhs$id <- NULL
#-----2. Funcion cvrep regresion logistica-----

cruzadalogistica <- function(data=data,vardep=NULL,
                             listconti=NULL,listclass=NULL,grupos=4,sinicio=1234,repe=5)
{
  
  if  (listclass !=c(""))
  {
    for (i in 1:dim(array(listclass))) {
      numindi<-which(names(data)==listclass[[i]])
      data[,numindi]<-as.character(data[,numindi])
      data[,numindi]<-as.factor(data[,numindi])
    }
  }   
  
  data[,vardep]<-as.factor(data[,vardep])
  
  # Creo la formula para la logistica
  
  if  (listclass!=c(""))
  {
    koko<-c(listconti,listclass)
  }  else   {
    koko<-c(listconti)
  }
  
  modelo<-paste(koko,sep="",collapse="+")
  formu<-formula(paste(vardep,"~",modelo,sep=""))
  
  formu 
  # Preparo caret
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                        savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  regresion <- train(formu,data=data,
                     trControl=control,method="glm",family = binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  # tasafallos<-function(x,y) {
  #   confu<-confusionMatrix(x,y)
  #   tasa<-confu[[3]][1]
  #   return(tasa)
  # }
  
  sensibilidad<-function(x,y) {
    confu<-confusionMatrix(x,y)
    sensib<-confu[[4]][1]
    return(sensib)
  }
  # Aplicamos función sobre cada Repetición
  
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
#-----3. vcrep con las distintas selecciones de variables------
#me devuelve la sensibilidad en boxplots y elegir? las mejores
#una vez hemos hecho vcrep para todos los conjuntos de variables, el que de menor eror
#es el ganador de regresion logistica
#de este saco interpretacion de los odds etc.
#con los 3 mejores hago modelos para redes

reglog_aic<-cruzadalogistica(data=nhs,
 vardep="diabetes",listconti=c("age", "weight", "healthgen.Fair", "race1.White",
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
                               "littleinterest.None"),
 listclass=c(""), grupos=4,sinicio=1234,repe=10)

 reglog_aic$modelo="AIC"

 #en listconti meto seleccion de variables bic
 reglog_bic<-cruzadalogistica(data=nhs,
  vardep="diabetes",listconti=c("age", "weight", "healthgen.Fair", "race1.White",
                                "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                "smokeage.NA", "education.High_School"),
  listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_bic$modelo="BIC"
 
 #en listconti meto seleccion de variables sbf
 reglog_sbf<-cruzadalogistica(data=nhs,
  vardep="diabetes",listconti=c("surveyyr.2009_10", "age", "race1.Black", "race1.White", "education.8th_Grade",
                                "education.9_11th_Grade", "education.College_Grad", "maritalstatus.Married",
                                "maritalstatus.NeverMarried", "maritalstatus.Separated", "poverty",
                                "homeown.Own", "work.Working", "weight", "height", "pulse", "directchol",
                                "totchol", "urinevol1", "urineflow1", "healthgen.Fair", "healthgen.Vgood",
                                "daysphyshlthbad.Few", "daysphyshlthbad.Many", "daysphyshlthbad.NA",
                                "daysphyshlthbad.None", "daysmenthlthbad.Many", "daysmenthlthbad.NA",
                                "daysmenthlthbad.None", "littleinterest.NA", "littleinterest.None",
                                "depressed.NA", "depressed.None", "npregnancies.1or2", "npregnancies.MoreThan2",
                                "npregnancies.NA", "nbabies.0to2", "nbabies.MoreThan2", "nbabies.NA",
                                "age1stbaby.21orLess", "age1stbaby.NA", "sleephrsnight", "sleeptrouble.Yes",
                                "physactivedays", "tvhrsday.0to1hr", "tvhrsday.from4hr", "comphrsday.0_hrs",
                                "comphrsday.from1hr", "alcohol12plusyr.No", "alcohol12plusyr.Yes",
                                "alcoholday.1", "alcoholday.2or3", "alcoholday.NA", "alcoholyear",
                                "smokenow.No", "smokenow.Yes", "smoke100.No", "smoke100.Yes",
                                "smokeage.18orMore", "smokeage.Before18", "smokeage.NA", "agefirstmarij.18orMore",
                                "agefirstmarij.Before18", "agefirstmarij.NA", "ageregmarij.18orMore",
                                "ageregmarij.Before18", "ageregmarij.NotApply", "harddrugs.NA",
                                "harddrugs.No", "harddrugs.NotAsked", "sexever.NA", "sexever.No",
                                "sexever.Yes", "sexage.15orLess", "sexage.16to18", "sexage.NA",
                                "sexnumpartnlife.11to25", "sexnumpartnlife.1to5", "sexnumpartnlife.6to10",
                                "sexnumpartnlife.MoreThan25", "sexnumpartnlife.NA", "sexnumpartnlife.None",
                                "sexnumpartyear.1", "sexnumpartyear.MoreThan1", "sexnumpartyear.NA",
                                "sexnumpartyear.None", "samesex.NA", "samesex.No", "samesex.Yes",
                                "bpsys", "bpdia", "marij.HasTried", "marij.MarijUser", "marij.Never",
                                "marij.NotAsked"),
  listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_sbf$modelo="SBF"
 
 #en listconti meto seleccion de variables rfe
 reglog_rfe<-cruzadalogistica(data=nhs,
  vardep="diabetes",listconti= c("age", "totchol", "height", "weight", "urineflow1", "urinevol1",
                                 "pulse", "directchol", "homerooms", "bpdia", "poverty", "healthgen.Fair",
                                 "bpsys", "healthgen.Vgood", "sleephrsnight", "marij.NotAsked",
                                 "race1.White", "alcoholyear", "education.9_11th_Grade", "education.Some_College",
                                 "race1.Hispanic", "maritalstatus.Married", "comphrsday.0_to_1_hr",
                                 "race1.Mexican", "smokeage.18orMore", "gender.male", "homeown.Own",
                                 "smoke100.No", "tvhrsday.2to3hr", "alcohol12plusyr.Yes", "sleeptrouble.Yes",
                                 "smokeage.Before18", "education.High_School", "maritalstatus.Separated",
                                 "daysmenthlthbad.Few", "comphrsday.0_hrs", "daysmenthlthbad.None",
                                 "comphrsday.from1hr", "smokenow.Yes", "daysphyshlthbad.Few",
                                 "maritalstatus.NeverMarried", "surveyyr.2009_10", "alcoholday.1",
                                 "daysmenthlthbad.Many", "smoke100.Yes", "sexnumpartnlife.11to25",
                                 "sexage.15orLess", "sexage.16to18", "sexnumpartnlife.1to5", "age1stbaby.22orMore",
                                 "daysphyshlthbad.None", "healthgen.Good", "education.College_Grad",
                                 "nbabies.0to2", "physactivedays"),
  listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_rfe$modelo="RFE"
 
 #en listconti meto seleccion de variables boruta
 reglog_boruta<-cruzadalogistica(data=nhs,
  vardep="diabetes",listconti=c("age", "poverty", "homerooms", "weight", "height", "pulse", 
    "directchol", "totchol", "urinevol1", "urineflow1", "sleephrsnight",
    "physactivedays", "alcoholyear", "bpdia", "bpsys", "surveyyr.2009_10",
    "gender.male", "race1.Black", "race1.Hispanic", "race1.Mexican",
    "race1.White", "education.8th_Grade", "education.9_11th_Grade",
    "education.College_Grad", "education.High_School", "education.Some_College",
    "maritalstatus.Married", "maritalstatus.NeverMarried", "maritalstatus.Separated",
    "homeown.Own", "work.Working", "healthgen.Fair", "healthgen.Good",
    "healthgen.Vgood", "daysphyshlthbad.Few", "daysphyshlthbad.Many",
    "daysphyshlthbad.NA", "daysphyshlthbad.None", "daysmenthlthbad.Few",
    "daysmenthlthbad.Many", "daysmenthlthbad.NA", "daysmenthlthbad.None",
    "littleinterest.NA", "littleinterest.None", "littleinterest.Several",
    "depressed.NA", "depressed.None", "depressed.Several", "npregnancies.1or2",
    "npregnancies.MoreThan2", "npregnancies.NA", "nbabies.0to2",
    "nbabies.MoreThan2", "nbabies.NA", "age1stbaby.21orLess", "age1stbaby.22orMore",
    "age1stbaby.NA", "sleeptrouble.Yes", "tvhrsday.0to1hr", "tvhrsday.2to3hr",
    "tvhrsday.from4hr", "comphrsday.0_hrs", "comphrsday.0_to_1_hr",
    "comphrsday.from1hr", "alcohol12plusyr.No", "alcohol12plusyr.Yes",
    "alcoholday.1", "alcoholday.2or3", "alcoholday.NA", "smokenow.No",
    "smokenow.Yes", "smoke100.No", "smoke100.Yes", "smokeage.18orMore",
    "smokeage.Before18", "smokeage.NA", "agefirstmarij.18orMore",
    "agefirstmarij.Before18", "agefirstmarij.NA", "ageregmarij.18orMore",
    "ageregmarij.Before18", "ageregmarij.NotApply", "harddrugs.NA",
    "harddrugs.No", "harddrugs.NotAsked", "sexever.NA", "sexever.No",
    "sexever.Yes", "sexage.15orLess", "sexage.16to18", "sexage.NA",
    "sexnumpartnlife.11to25", "sexnumpartnlife.1to5", "sexnumpartnlife.6to10",
    "sexnumpartnlife.MoreThan25", "sexnumpartnlife.NA", "sexnumpartnlife.None",
    "sexnumpartyear.1", "sexnumpartyear.MoreThan1", "sexnumpartyear.NA",
    "sexnumpartyear.None", "samesex.NA", "samesex.No", "samesex.Yes",
    "marij.HasTried", "marij.MarijUser", "marij.Never", "marij.NotAsked"),
  listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_boruta$modelo="Boruta"
 
 #en listconti meto seleccion de variables mmpc y ses (es la misma)
 reglog_ses<-cruzadalogistica(data=nhs,
  vardep="diabetes",listconti=c("age", "race1.Black", "maritalstatus.Separated", "work.Working",
                                "weight", "directchol", "totchol", "healthgen.Fair", "healthgen.Vgood",
                                "daysphyshlthbad.Many", "sleeptrouble.Yes", "physactivedays",
                                "tvhrsday.from4hr", "alcoholyear", "harddrugs.NotAsked", "bpsys"),
  listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_ses$modelo="SES"

 #en listconti meto mejores variables arboles
 reglog_arbol<-cruzadalogistica(data=nhs,
                              vardep="diabetes",listconti=c("age","totchol","healthgen.Fair"),
                              listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_arbol$modelo="arbol"
 
 #en listconti meto mejores variables random forest
 reglog_rf<-cruzadalogistica(data=nhs,
                              vardep="diabetes",listconti=c("age", "totchol", "weight", "healthgen.Fair", 
                                                            "healthgen.Vgood","directchol","height","bpdia",
                                                            "poverty","bpsys"),
                              listclass=c(""), grupos=4,sinicio=1234,repe=10)
 
 reglog_rf$modelo="rf"
 
#-----4. Boxplots mejor selecci?n de variables----

 union<-rbind(reglog_aic,reglog_bic,reglog_sbf,reglog_rfe,
              reglog_boruta,reglog_ses, reglog_arbol,reglog_rf)

 par(cex.axis=1.5)
 boxplot(data=union,sen~modelo,col='#9698ed',main="Sensibilidad")
 boxplot(data=union,auc~modelo,col='#9698ed',main="AUC")
 
#-----5. Ganador regresi?n log?stica----
 #La mejor regresi?n log?stica es la contruida con la selecci?n de variables AIC
 #Vamos a sacar coeficientes para poder interpretarlos
 nhs <- readRDS("nhs.rds")
 nhs$id <- NULL
 set.seed(1234)
 control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 
 
 glm_aic<- train(data=nhs, diabetes ~ age + weight + healthgen.Fair + race1.White + totchol + 
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

 summary(glm_aic)

#del que tenga mayor sensibilidad saco conclusiones de los odds
#prediccion
pred_glm <- glm_aic$pred

#calculamos odds y log-odds para interpretacion
pred_glm <- pred_glm %>% mutate(odds = Yes / No, log.odds = log(odds))
confusionMatrix(pred_glm$pred, pred_glm$obs)

coeficientes <- glm_aic[["finalModel"]][["coefficients"]]
coeficientes
coeficientes <- as.data.frame(coeficientes)

#write.csv(coeficientes, file = "glm_coeficientes.csv", row.names = TRUE)
#los betas de la regresion son los siguientes
exp(coeficientes)

#-----6. Selecci?n de variables para redes neuronales-----
#AIC
#Boruta da buen resultado pero sobreajusta (comprobar)
#SBF 106 variables
#RFE 55 variables (esta la descartaria, baja el AUC)
#BIC 28 variables