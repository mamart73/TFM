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
library(MASS)
library(reshape)
library(explore)
library(imbalance)
library(randomForest)

#-----1.2. Importamos dataset nhs----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nhs <- readRDS("nhs.rds")

#eliminamos id
nhs$id <- NULL
#-----2.1. Vemos en que numero de iteraciones converge el error-----
set.seed(1234)
control<-trainControl(method = "cv",number=4,savePredictions = "all", classProbs=TRUE)
statistics <-   data.frame(n_tree = integer(),
                           accuracy    = double(),
                           auc         = double(),
                           sensitivity = double())

# Valores evaluados
for (n_tree in seq(from=1, to=2000, by=100)){
  set.seed(1234)
  # Ajustamos
  rf<- train(data=nhs,
             diabetes ~.,
             method="rf",trControl=control,
             linout = FALSE,ntree=n_tree,nodesize=1150,replace=TRUE)
  # Predicciones para  obtener m?tricas
  sal<-rf$pred
  salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
  curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
  auc<-curvaroc$auc
  # Estadisticos
  auc<-round(curvaroc$auc,4)
  accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
  sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
  
  #Insertamos en Dataframe
  statistics[nrow(statistics) + 1,] <- c(n_tree,accuracy,auc,sensitivity)
}

#saveRDS(statistics, file = "randomforest_itera.rds")
#statistics <- readRDS("randomforest_itera.rds")

ggplot(data=statistics) +
  geom_line(aes(x=n_tree, y=sensitivity), color='#9698ed', size = 1)+
  geom_point(aes(x=n_tree, y=sensitivity))+
  labs(title="N?mero de iteraciones vs. valor sensibilidad",
       x="N?mero de ?rboles", y="Sensibilidad") +
  geom_vline(xintercept = 601, color = '#6F70AA', size=1) +
  theme(text = element_text(family = "lato", size=12), 
        plot.title = element_text(hjust = 0.35),
        plot.subtitle = element_text(hjust=0.35))

ggplot(data=statistics) +
  geom_line(aes(x=n_tree, y=auc), color='#9698ed', size = 1)+
  geom_point(aes(x=n_tree, y=auc))+
  labs(title="N?mero de iteraciones vs. valor AUC",
       x="N?mero de ?rboles", y="AUC") +
  geom_vline(xintercept = 101, color = '#6F70AA', size=1) +
  theme(text = element_text(family = "lato", size=12), 
        plot.title = element_text(hjust = 0.35),
        plot.subtitle = element_text(hjust=0.35))

#Se estabilizan las medidas de bondad de ajuste sobre las 600 iteraciones

#-----2.2. Random forest - Busqueda mejores parametros-----
set.seed(1234)
#saveRDS(rf_statistics, file = "rf_statistics.rds")

rf_statistics <- data.frame(samplesize = integer(),
                            nodesize = integer(),
                            mtry = integer(),
                            accuracy    = double(),
                            auc         = double(),
                            sensitivity = double(),
                            specificity = double())

for (muestrabag in seq(from=trunc(nrow(nhs)*0.15), to=trunc(nrow(nhs)*0.75), by=1000))
{
  for (minbu in seq(from=round(muestrabag*0.05), to=round(muestrabag*0.15), by=150))
  {
    for (mtrysize in seq(from=18, to=ncol(nhs)-1, by=18))
    {
      print(muestrabag)
      print(minbu)
      print(mtrysize)
      cat("/n")
      rfgrid<-expand.grid(mtry=mtrysize)
    
      set.seed(1234)
      control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 
    
      rf<- train(data=nhs,diabetes ~.,
               method="rf",trControl=control,tuneGrid=rfgrid,
               linout = FALSE,sample_size=muestrabag,ntree=600,
               nodesize=minbu,replace=TRUE)
    
      #prediccion
      sal <- rf$pred
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
      rf_statistics[nrow(rf_statistics) + 1,] <- c(muestrabag,minbu,mtrysize,
                                              accuracy,auc,sensitivity,specificity)
    }
  }
}

mejores_rf <- head(rf_statistics[order(rf_statistics$sensitivity,
                                               decreasing = TRUE), ], n=5)
#Los mejores modelos de random forest son:
#muestrabag=2140, minbucket=107, mtrysize=108
#muestrabag=2140, minbucket=107, mtrysize=72
#muestrabag=2140, minbucket=107, mtrysize=90

#Ahora hacemos validacion cruzada repetida con los 3 mejores grids de par?metros

#-----3. Funcion vcrep rf-----
cruzadarfbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,nodesize=20,
           mtry=2,ntree=50,replace=TRUE,sampsize=1)
  { 
    # if  (sampsize==1)
    # {
    #  sampsize=floor(nrow(data)/(grupos-1))
    # }
    
    # Preparacion del archivo
    
    # b)pasar las categoricas a dummies
    
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
    
    rfgrid <-expand.grid(mtry=mtry)
    
    if  (sampsize==1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,ntree=ntree)
    }
    
    else  if  (sampsize!=1)
    {
      rf<- train(formu,data=databis,
                 method="rf",trControl=control,
                 tuneGrid=rfgrid,nodesize=nodesize,replace=replace,sampsize=sampsize,
                 ntree=ntree)
    }
    
    
    print(rf$results)
    
    preditest<-rf$pred
    
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
    # Aplicamos funcion sobre cada Repeticion
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(sen=sensibilidad(pred,obs))
    
    # CalculamoS AUC  por cada Repeticion de cv 
    # Definimnos funcion
    
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
#-----4. Mejor RF----
#Los mejores modelos de random forest son:
#muestrabag=2140, minbucket=107, mtrysize=108
#muestrabag=2140, minbucket=107, mtrysize=72
#muestrabag=2140, minbucket=107, mtrysize=90
set.seed(1234)

#muestrabag=2140, minbucket=107, mtrysize=108
cruzadarf1<-cruzadarfbin(data=nhs, vardep="diabetes", listconti=c(
  'surveyyr.2009_10','gender.male','age','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','education.8th_Grade','education.9_11th_Grade',
  'education.College_Grad','education.High_School','education.Some_College',
  'maritalstatus.Married','maritalstatus.NeverMarried','maritalstatus.Separated',
  'poverty','homerooms','homeown.Own','work.Working','weight','height','pulse',
  'directchol','totchol','urinevol1','urineflow1','healthgen.Fair',
  'healthgen.Good','healthgen.Vgood','daysphyshlthbad.Few','daysphyshlthbad.Many',
  'daysphyshlthbad.NA','daysphyshlthbad.None','daysmenthlthbad.Few',
  'daysmenthlthbad.Many','daysmenthlthbad.NA','daysmenthlthbad.None',
  'littleinterest.NA','littleinterest.None','littleinterest.Several',
  'depressed.NA','depressed.None','depressed.Several','npregnancies.1or2',
  'npregnancies.MoreThan2','npregnancies.NA','nbabies.0to2','nbabies.MoreThan2',
  'nbabies.NA','age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA',
  'sleephrsnight','sleeptrouble.Yes','physactivedays','tvhrsday.0to1hr',
  'tvhrsday.2to3hr','tvhrsday.from4hr','comphrsday.0_hrs','comphrsday.0_to_1_hr',
  'comphrsday.from1hr','alcohol12plusyr.No','alcohol12plusyr.Yes','alcoholday.1',
  'alcoholday.2or3','alcoholday.NA','alcoholyear','smokenow.No','smokenow.Yes',
  'smoke100.No','smoke100.Yes','smokeage.18orMore','smokeage.Before18',
  'smokeage.NA','agefirstmarij.18orMore','agefirstmarij.Before18',
  'agefirstmarij.NA','ageregmarij.18orMore','ageregmarij.Before18',
  'ageregmarij.NotApply','harddrugs.NA','harddrugs.No','harddrugs.NotAsked',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','bpsys','bpdia','marij.HasTried','marij.MarijUser',
  'marij.Never','marij.NotAsked'),
  listclass=c(""),grupos=4,sinicio=1234,repe=10,nodesize=107,replace=TRUE, 
  sampsize=2140,ntree=600,mtry=108)

#muestrabag=2140, minbucket=107, mtrysize=72
cruzadarf2<-cruzadarfbin(data=nhs, vardep="diabetes", listconti=c(
  'surveyyr.2009_10','gender.male','age','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','education.8th_Grade','education.9_11th_Grade',
  'education.College_Grad','education.High_School','education.Some_College',
  'maritalstatus.Married','maritalstatus.NeverMarried','maritalstatus.Separated',
  'poverty','homerooms','homeown.Own','work.Working','weight','height','pulse',
  'directchol','totchol','urinevol1','urineflow1','healthgen.Fair',
  'healthgen.Good','healthgen.Vgood','daysphyshlthbad.Few','daysphyshlthbad.Many',
  'daysphyshlthbad.NA','daysphyshlthbad.None','daysmenthlthbad.Few',
  'daysmenthlthbad.Many','daysmenthlthbad.NA','daysmenthlthbad.None',
  'littleinterest.NA','littleinterest.None','littleinterest.Several',
  'depressed.NA','depressed.None','depressed.Several','npregnancies.1or2',
  'npregnancies.MoreThan2','npregnancies.NA','nbabies.0to2','nbabies.MoreThan2',
  'nbabies.NA','age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA',
  'sleephrsnight','sleeptrouble.Yes','physactivedays','tvhrsday.0to1hr',
  'tvhrsday.2to3hr','tvhrsday.from4hr','comphrsday.0_hrs','comphrsday.0_to_1_hr',
  'comphrsday.from1hr','alcohol12plusyr.No','alcohol12plusyr.Yes','alcoholday.1',
  'alcoholday.2or3','alcoholday.NA','alcoholyear','smokenow.No','smokenow.Yes',
  'smoke100.No','smoke100.Yes','smokeage.18orMore','smokeage.Before18',
  'smokeage.NA','agefirstmarij.18orMore','agefirstmarij.Before18',
  'agefirstmarij.NA','ageregmarij.18orMore','ageregmarij.Before18',
  'ageregmarij.NotApply','harddrugs.NA','harddrugs.No','harddrugs.NotAsked',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','bpsys','bpdia','marij.HasTried','marij.MarijUser',
  'marij.Never','marij.NotAsked'),
  listclass=c(""),grupos=4,sinicio=1234,repe=10,nodesize=107,replace=TRUE, 
  sampsize=2140,ntree=600,mtry=72)

#muestrabag=2140, minbucket=107, mtrysize=90
cruzadarf3<-cruzadarfbin(data=nhs, vardep="diabetes", listconti=c(
  'surveyyr.2009_10','gender.male','age','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','education.8th_Grade','education.9_11th_Grade',
  'education.College_Grad','education.High_School','education.Some_College',
  'maritalstatus.Married','maritalstatus.NeverMarried','maritalstatus.Separated',
  'poverty','homerooms','homeown.Own','work.Working','weight','height','pulse',
  'directchol','totchol','urinevol1','urineflow1','healthgen.Fair',
  'healthgen.Good','healthgen.Vgood','daysphyshlthbad.Few','daysphyshlthbad.Many',
  'daysphyshlthbad.NA','daysphyshlthbad.None','daysmenthlthbad.Few',
  'daysmenthlthbad.Many','daysmenthlthbad.NA','daysmenthlthbad.None',
  'littleinterest.NA','littleinterest.None','littleinterest.Several',
  'depressed.NA','depressed.None','depressed.Several','npregnancies.1or2',
  'npregnancies.MoreThan2','npregnancies.NA','nbabies.0to2','nbabies.MoreThan2',
  'nbabies.NA','age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA',
  'sleephrsnight','sleeptrouble.Yes','physactivedays','tvhrsday.0to1hr',
  'tvhrsday.2to3hr','tvhrsday.from4hr','comphrsday.0_hrs','comphrsday.0_to_1_hr',
  'comphrsday.from1hr','alcohol12plusyr.No','alcohol12plusyr.Yes','alcoholday.1',
  'alcoholday.2or3','alcoholday.NA','alcoholyear','smokenow.No','smokenow.Yes',
  'smoke100.No','smoke100.Yes','smokeage.18orMore','smokeage.Before18',
  'smokeage.NA','agefirstmarij.18orMore','agefirstmarij.Before18',
  'agefirstmarij.NA','ageregmarij.18orMore','ageregmarij.Before18',
  'ageregmarij.NotApply','harddrugs.NA','harddrugs.No','harddrugs.NotAsked',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','bpsys','bpdia','marij.HasTried','marij.MarijUser',
  'marij.Never','marij.NotAsked'),
  listclass=c(""),grupos=4,sinicio=1234,repe=10,nodesize=107,replace=TRUE, 
  sampsize=2140,ntree=600,mtry=90)

cruzadarf1$modelo="rf_m108"
cruzadarf2$modelo="rf_m72"
cruzadarf3$modelo="rf_m90"

unionrf <- rbind(cruzadarf1,cruzadarf2,cruzadarf3)
par(cex.axis=1.5, family="lato")
boxplot(data=unionrf, sen~modelo, col='#9698ed', main="SENSIBILIDAD")
boxplot(data=unionrf, auc~modelo, col='#9698ed', main="AUC")

#El mejor modelo de random forest es cruzadarf1$modelo="rf_m108"
#-----6. Variables importantes random forest-----
library(randomForest)
set.seed(1234)
imp_best_rf <- randomForest(diabetes~., 
                          data=nhs, mtry=108, ntree=600, sampsize=2140, 
                          nodesize=107, replace=TRUE)

imp <- as.data.frame(imp_best_rf[["importance"]])
imp_data <- tibble::rownames_to_column(imp, "Variables") %>% arrange(desc(MeanDecreaseGini))
imp_data_graf <- imp_data[1:10,]
  
imp_data_graf %>%
  mutate(imp = MeanDecreaseGini) %>% 
  mutate(Variables = fct_reorder(as.factor(Variables), imp)) %>%
  ggplot(aes(x=Variables, y=imp)) +
  theme(text = element_text(family = "lato", size=14))+
  geom_bar(stat="identity", fill='#9698ed', alpha=.6, width=.4)+
  coord_flip()+
  labs(title = "Importancia de las variables en el random forest ganador")

