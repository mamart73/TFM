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
setwd("C:/Users/MediaService/Desktop/MASTER/TFM")
nhs <- readRDS("nhs_estan.rds")
#ASEGURARSE DE METER EL ESTANDARIZADO
#eliminamos id
nhs$id <- NULL
#-----2. Acelerar procesos-----
library(parallel)
library(doParallel)
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing
#-----3. Redes neuronales - Busqueda mejores parametros------
#-----3.1. AIC-----
#Seleccion variables AIC

#Calculamos los nodos segun la formula
#h(k+1)+h+1=14273/20-30 obs por parametro
#k=52 con las variables que nos da AIC
#20 obs por parametro: 53h+h+1=713 -> h=13.43
#30 obs por parametro: 53h+h+1=475 -> h=8.77

set.seed(1234)
nn_statistics <- data.frame(size = integer(),
                            decay = integer(),
                            iter = integer(),
                            accuracy    = double(),
                            auc         = double(),
                            sensitivity = double(),
                            specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (hln in seq(from=9, to=13, by=1))
{
  for (dec in c(0.001,0.01,0.1,0.2))
  {
    for (iter in c(50,100,300,1000))
    {
      set.seed(1234)
      print(hln)
      print(dec)
      print(iter)
      cat("/n")
      #Definimos el grid de par醡etros
      nnetgrid<-expand.grid(size=hln,decay=dec,bag=F)
      
      rednnet<- train(diabetes ~ age + weight + healthgen.Fair + race1.White + 
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
                      data=nhs,method="avNNet",linout = TRUE,maxit=iter,
                      trControl=control,tuneGrid=nnetgrid,trace=TRUE)
      #prediccion
      sal <- rednnet$pred
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
      nn_statistics[nrow(nn_statistics) + 1,] <- c(hln,dec,iter,
                                                   accuracy,auc,sensitivity,specificity)
    }
  }
}

# saveRDS(sal, file = "sal_aic.rds")
# saveRDS(salconfu, file = "salconfu_aic.rds")
# saveRDS(curvaroc, file = "curvaroc_aic.rds")
# saveRDS(nn_statistics, file = "nn_statistics_aic.rds")
# saveRDS(rednnet, file = "rednnet_aic.rds")

#-----3.1.1. Importar AIC----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/PROGRAMAS/Param_redneruronal_AIC")
sal_AIC <- readRDS("sal_aic.rds")
salconfu_AIC <- readRDS("salconfu_aic.rds")
curvaroc_AIC <- readRDS("curvaroc_aic.rds")
nn_statistics_AIC <- readRDS("nn_statistics_aic.rds")
rednnet_AIC <- readRDS("rednnet_aic.rds")

#Elegimos mejores conjuntos de parametros
cross_val_simple_nn <- nn_statistics_AIC %>% arrange(desc(sensitivity))
mejores_nn <- head(cross_val_simple_nn,n=5)

par(cex.axis=1.5, family="lato")
ggplot(cross_val_simple_nn, aes(x=factor(iter), y=sensitivity, 
                                color=factor(size),pch=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de par醡etros con selecci髇 de variables AIC") +
  theme(text = element_text(family = "lato", size=12))

#Los mejores modelos con selecci髇 de variables AIC son:
#size=13, decay=0.001 con 300 iteraciones
#size=12, decay=0.001 con 300 iteraciones
#size=13, decay=0.01 con 300 iteraciones

#-----3.2. Boruta----
#Seleccion variables Boruta

#Calculamos los nodos segun la formula
#h(k+1)+h+1=14273/20-30 obs por parametro
#k=108 con las variables que nos da boruta
#20 obs por parametro: 109h+h+1=713 -> h=6.47
#30 obs por parametro: 109h+h+1=475 -> h=4.30

set.seed(1234)
nn_statistics <- data.frame(size = integer(),
                            decay = integer(),
                            iter = integer(),
                            accuracy    = double(),
                            auc         = double(),
                            sensitivity = double(),
                            specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (hln in seq(from=4, to=6, by=1))
{
  for (dec in c(0.001,0.01,0.1,0.2))
  {
    for (iter in  c(50,100,300,1000))
    {
      set.seed(1234)
      print(hln)
      print(dec)
      print(iter)
      cat("/n")
      #Definimos el grid de par醡etros
      nnetgrid<-expand.grid(size=hln,decay=dec,bag=F)
      
      rednnet<- train(diabetes ~ .,
                      data=nhs,method="avNNet",linout = TRUE,maxit=iter,
                      trControl=control,tuneGrid=nnetgrid,trace=TRUE)
      #prediccion
      sal <- rednnet$pred
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
      nn_statistics[nrow(nn_statistics) + 1,] <- c(hln,dec,iter,
                                                   accuracy,auc,sensitivity,specificity)
    }
  }
}

saveRDS(sal, file = "sal_boruta.rds")
saveRDS(salconfu, file = "salconfu_boruta.rds")
saveRDS(curvaroc, file = "curvaroc_boruta.rds")
saveRDS(nn_statistics, file = "nn_statistics_boruta.rds")
saveRDS(rednnet, file = "rednnet_boruta.rds")

#-----3.2.1. Importar Boruta-----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/PROGRAMAS/Param_redneuronal_boruta")
sal_boruta <- readRDS("sal_boruta.rds")
salconfu_boruta <- readRDS("salconfu_boruta.rds")
curvaroc_boruta <- readRDS("curvaroc_boruta.rds")
nn_statistics_boruta <- readRDS("nn_statistics_boruta.rds")
rednnet_boruta <- readRDS("rednnet_boruta.rds")

#Elegimos mejores conjuntos de parametros
cross_val_simple_nn <- nn_statistics_boruta %>% arrange(desc(sensitivity))
mejores_nn <- head(cross_val_simple_nn,n=10)

ggplot(cross_val_simple_nn, aes(x=factor(iter), y=sensitivity, 
                                color=factor(size),pch=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de par醡etros con selecci髇 de variables Boruta") +
  theme(text = element_text(family = "lato", size=12))

#Los mejores modelos con selecci髇 de variables Boruta son:
#size=6, decay=0.001 con 300 iteraciones
#size=5, decay=0.001 con 300 iteraciones

#-----4. Funcion vcrep redes-----

cruzadaavnnetbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=TRUE)
  { 
    
    # Preparaci贸n del archivo
    
    # b)pasar las categ贸ricas a dummies
    
    if  (listclass!=c(""))
    {
      databis<-data[,c(vardep,listconti,listclass)]
      databis<- dummy.data.frame(databis, listclass, sep = ".")
    }  else   {
      databis<-data[,c(vardep,listconti)]
    }
    
    # c)estandarizar las variables continuas
    
    # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
    
    means <-apply(databis[,listconti],2,mean)
    sds<-sapply(databis[,listconti],sd)
    
    # Estandarizo solo las continuas y uno con las categoricas
    
    datacon<-scale(databis[,listconti], center = means, scale = sds)
    numerocont<-which(colnames(databis)%in%listconti)
    databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
    
    databis[,vardep]<-as.factor(databis[,vardep])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=databis,
                   method="avNNet",linout = FALSE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid,trace=trace)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
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
    
    # Aplicamos funci贸n sobre cada Repetici贸n
    
    # medias<-preditest %>%
    #   group_by(Rep) %>%
    #   summarize(tasa=1-tasafallos(pred,obs))
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repetici贸n de cv 
    # Definimnos funci贸n
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      auc<-as.numeric(auc)
      return(auc)
    }
    
    # Aplicamos funci贸n sobre cada Repetici贸n
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }


#-----5. vcrep de los mejores conjuntos de parametros y seleccion AIC----
#Los mejores modelos con selecci髇 de variables AIC son:
#size=13, decay=0.001 con 300 iteraciones
#size=12, decay=0.001 con 300 iteraciones

set.seed(1234)
cruzadann_s13_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",listconti=c("age", "weight", "healthgen.Fair", "race1.White",
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
 listclass=c(""),grupos=4,sinicio=1234,repe=5,
  size=13,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s13_dec001$modelo="nn_s13_dec001"

set.seed(1234)
cruzadann_s12_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",
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
                                         "littleinterest.None"),
                                         listclass=c(""),grupos=4,sinicio=1234,repe=5,
                                         size=12,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s12_dec001$modelo="nn_s12_dec001"

#Los mejores modelos con selecci髇 de variables Boruta son:
#size=6, decay=0.001 con 300 iteraciones
#size=5, decay=0.001 con 300 iteraciones

set.seed(1234)
cruzadann_s6_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",listconti=
                                          c("age", "poverty", "homerooms", "weight", "height", "pulse",
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
                                         listclass=c(""),grupos=4,sinicio=1234,repe=5,
                                         size=6,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s6_dec001$modelo="nn_s6_dec001"

set.seed(1234)
cruzadann_s5_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",listconti=
                                          c("age", "poverty", "homerooms", "weight", "height", "pulse",
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
                                        listclass=c(""),grupos=4,sinicio=1234,repe=5,
                                        size=5,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s5_dec001$modelo="nn_s5_dec001"

#-----6. Boxplots mejor selecci髇 de variables----

union<-rbind(cruzadann_s13_dec001,cruzadann_s12_dec001,
             cruzadann_s6_dec001,cruzadann_s5_dec001)

par(cex.axis=1)
boxplot(data=union, sen~modelo, col='#9698ed', main="SENSIBILIDAD")
boxplot(data=union, auc~modelo, col='#9698ed', main="AUC")

#Mejor modelo size=13, decay=0.001 con seleccion de variables AIC

#Guardo los modelos 
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/PROGRAMAS/comparacion_redneuronal")
union <- readRDS("union_redesneuronales.rds")
#saveRDS(union, file = "union_redesneuronales.rds")
