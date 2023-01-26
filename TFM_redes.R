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
      #Definimos el grid de par?metros
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
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nn_statistics_aic <- readRDS("nn_statistics_aic.rds")

#Elegimos mejores conjuntos de parametros
cross_val_simple_nn <- nn_statistics_aic %>% arrange(desc(sensitivity))
mejores_nn <- head(cross_val_simple_nn,n=5)

par(cex.axis=1.5, family="lato")
ggplot(cross_val_simple_nn, aes(x=factor(iter), y=sensitivity, 
                                color=factor(size),pch=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de parámetros con selección de variables AIC") +
  theme(text = element_text(family = "lato", size=12),
        legend.title=element_text(family='lato')) +
  labs(x='iter',color='size', pch='decay')

#Los mejores modelos con selecci?n de variables AIC son:
#size=13, decay=0.001 con 300 iteraciones
#size=12, decay=0.001 con 300 iteraciones
#size=13, decay=0.01 con 300 iteraciones

#-----3.2. BIC----
#Seleccion variables BIC

#Calculamos los nodos segun la formula
#h(k+1)+h+1=14273/20-30 obs por parametro
#k=22 con las variables que nos da BIC
#20 obs por parametro: 23h+h+1=713 -> h=29.6
#30 obs por parametro: 23h+h+1=475 -> h=19.75

set.seed(1234)
nn_statistics <- data.frame(size = integer(),
                            decay = integer(),
                            iter = integer(),
                            accuracy    = double(),
                            auc         = double(),
                            sensitivity = double(),
                            specificity = double())

control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all")

for (hln in seq(from=20, to=30, by=1))
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
      #Definimos el grid de par?metros
      nnetgrid<-expand.grid(size=hln,decay=dec,bag=F)
      
      rednnet<- train(diabetes ~ age+weight+healthgen.Fair+race1.White+ 
                        totchol+healthgen.Vgood+harddrugs.NotAsked+directchol+
                        sleeptrouble.Yes+pulse+height+sexnumpartnlife.6to10+
                        surveyyr.2009_10+sexnumpartyear.None+smokenow.No+alcohol12plusyr.No+
                        bpsys+bpdia+maritalstatus.Separated+agefirstmarij.18orMore+
                        smokeage.NA+education.High_School,
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

saveRDS(nn_statistics, file = "nn_statistics_bic.rds")

#-----3.2.1. Importar BIC-----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nn_statistics_bic <- readRDS("nn_statistics_bic.rds")

#Elegimos mejores conjuntos de parametros
cross_val_simple_nn <- nn_statistics_bic %>% arrange(desc(sensitivity))
mejores_nn <- head(cross_val_simple_nn,n=10)

ggplot(cross_val_simple_nn, aes(x=factor(iter), y=sensitivity, 
                                color=factor(size),pch=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3)+
  ggtitle("Sensibilidad para grids de parámetros con selección de variables BIC") +
  theme(text = element_text(family = "lato", size=12),
        legend.title=element_text(family='lato')) +
  labs(x='iter',color='size', pch='decay')

#Los mejores modelos con selecci?n de variables BIC son:
#size=30, decay=0.001 con 300 iteraciones
#size=28, decay=0.001 con 300 iteraciones

#-----4. Funcion vcrep redes-----

cruzadaavnnetbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=TRUE)
  { 
    
    # Preparación del archivo
    
    # b)pasar las categóricas a dummies
    
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
    
    # Aplicamos función sobre cada Repetición
    
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


#-----5. vcrep de los mejores conjuntos de parametros----
#Los mejores modelos con selecci?n de variables AIC son:
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
 listclass=c(""),grupos=4,sinicio=1234,repe=10,
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
                                         listclass=c(""),grupos=4,sinicio=1234,repe=10,
                                         size=12,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s12_dec001$modelo="nn_s12_dec001"

#Los mejores modelos con selecci?n de variables BIC son:
#size=30, decay=0.001 con 300 iteraciones
#size=28, decay=0.001 con 300 iteraciones

set.seed(1234)
cruzadann_s30_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",listconti=
                                           c("age", "weight", "healthgen.Fair", "race1.White",
                                             "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                             "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                             "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                             "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                             "smokeage.NA", "education.High_School"),
                                         listclass=c(""),grupos=4,sinicio=1234,repe=10,
                                         size=30,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s30_dec001$modelo="nn_s30_dec001"

set.seed(1234)
cruzadann_s28_dec001 <- cruzadaavnnetbin(data=nhs, vardep="diabetes",listconti=
                                           c("age", "weight", "healthgen.Fair", "race1.White",
                                             "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
                                             "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
                                             "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
                                             "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
                                             "smokeage.NA", "education.High_School"),
                                          listclass=c(""),grupos=4,sinicio=1234,repe=10,
                                          size=28,decay=0.001,repeticiones=5,itera=300,trace=TRUE)

cruzadann_s28_dec001$modelo="nn_s28_dec001"

#-----6. Boxplots mejor selecci?n de variables----

union<-rbind(cruzadann_s13_dec001,cruzadann_s12_dec001,
             cruzadann_s30_dec001,cruzadann_s28_dec001)

par(cex.axis=1)
boxplot(data=union, sen~modelo, col='#9698ed', main="SENSIBILIDAD")
boxplot(data=union, auc~modelo, col='#9698ed', main="AUC")

#Mejor modelo size=13, decay=0.001 con seleccion de variables AIC

#Guardo los modelos 
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
#saveRDS(union, file = "union_redesneuronales.rds")
