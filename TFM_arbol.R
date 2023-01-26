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

#-----1.2. Importamos dataset nhs----
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
nhs <- readRDS("nhs.rds")

#eliminamos id
nhs$id <- NULL
#-----2. Arboles de clasificacion-----
simple_cross_validation <- function(list_of_minbucket) {
  
  #dataframe vac?o con la estructura indicada
  statistics <- data.frame(minbucket   = integer(),
                           accuracy    = double(),
                           auc         = double(),
                           sensitivity = double(),
                           specificity = double())

  #validaci?n cruzada simple
  set.seed(1234)
  control <- trainControl(method = "cv",number=4, classProbs=TRUE, savePredictions = "all") 
  arbolgrid <- expand.grid(cp=c(0))
  
  #comienza el bucle
  for (bucket in list_of_minbucket)
  {
    #?rbol
    set.seed(1234)
    arbolcaret <- train(diabetes ~ ., data=nhs,
                       method="rpart",
                       trControl=control,
                       tuneGrid=arbolgrid,
                       control=rpart.control(minbucket = bucket))
    #prediccion
    sal <- arbolcaret$pred
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
    statistics[nrow(statistics) + 1,] <- c(bucket,accuracy,auc,sensitivity,specificity)
  }
  return(statistics)
}

list_minbucket <- seq(from=700, to=2150, by=150)
set.seed(1234)
cross_val_simple <- simple_cross_validation(list_minbucket)

#ordenamos los mejores modelos de arboles por sensibilidad
#usaremos esta medida de bondad de ajuste para elegir los mejores parametros
#para cada modelo 
#esto se hace porque se considera que teniendo en cuenta las caracteristicas
#de la variale objetivo, la cual se trata de predecir una enfermedad, lo mas 
#importante es predecir bien las personas enfermas
#-----3. Mejores parametros-----
mejores_arboles <- head(cross_val_simple[order(cross_val_simple$sensitivity,
                                               decreasing = TRUE), ], n=5)

#Visualizaci?n sensibilidad para los distintos minbuckets
size_optimo <- rev(cross_val_simple$minbucket)[which.max(rev(cross_val_simple$sensitivity))]
paste("Tama?o ?ptimo encontrado:", size_optimo)
ggplot(data = cross_val_simple, aes(x = minbucket, y = sensitivity)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = size_optimo, color = "red") +
  labs(title = "Sensibilidad vs tama?o del ?rbol") +
  theme_bw()

arbol1150 <- rpart(factor(diabetes)~., data = nhs, 
                  minbucket =1150, method = "class", maxsurrogate=0, cp=0, 
                  parms=list(split="gini")) #division gini
rpart.plot(arbol1150,extra=1)

rpart.plot(arbol1150,
           extra = 104,# show fitted class, probs, percentages
           cex=0.65,
           #box.palette = "GnBu", 
           nn = TRUE)  

#-----4. Funcion vcrep arbolbin-----
#Ahora hacemos validacion cruzada repetida con los 3 mejores minbuckets

cruzadaarbolbin<-
  function(data=data,vardep="vardep",
           listconti="listconti",listclass="listclass",
           grupos=4,sinicio=1234,repe=5,
           cp=c(0),minbucket =20)
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
    
    arbolgrid <- expand.grid(cp=cp)
    
    arbol<- train(formu,data=databis,
                  method="rpart",trControl=control,
                  tuneGrid=arbolgrid,control = rpart.control(minbucket = minbucket))
    
    print(arbol$results)
    
    preditest<-arbol$pred
    
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

#-----5. Mejor ?rbol-----
#Para sacar todas las variables m?s f?cil (QUITAR LA VARIABLE OBJETIVO)

t <- names(nhs)
a <- c()
for (i in 1:length(t)){
  a <- paste(a,t[i], sep = "','")
}

#Validacion cruzada repetida con los mejores minbuckets

set.seed(1234)
cruzadaarbol700<-cruzadaarbolbin(data=nhs, vardep="diabetes", listconti=c(
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
  listclass=c(""),grupos=4,sinicio=1234,repe=10,cp=0,minbucket=700)
  
cruzadaarbol700$modelo="arbol700"

cruzadaarbol1000<-cruzadaarbolbin(data=nhs, vardep="diabetes", listconti=c(
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
  listclass=c(""),grupos=4,sinicio=1234,repe=10,cp=0,minbucket=1000)

cruzadaarbol1000$modelo="arbol1000"

cruzadaarbol1150<-cruzadaarbolbin(data=nhs, vardep="diabetes", listconti=c(
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
  listclass=c(""),grupos=4,sinicio=1234,repe=10,cp=0,minbucket=1150)

cruzadaarbol1150$modelo="arbol1150"

unionarbol <- rbind(cruzadaarbol700,cruzadaarbol1000,cruzadaarbol1150)
par(cex.axis=1.5, family="lato")
boxplot(data=unionarbol, sen~modelo, col='#9698ed', main="SENSIBILIDAD")
boxplot(data=unionarbol, auc~modelo, col='#9698ed', main="AUC")

#Mejor modelo minbucket=1150

#-----6. Variables importantes arbol-----

arbol1150 <- rpart(factor(diabetes)~., data = nhs, 
                   minbucket =1150, method = "class", maxsurrogate=0, cp=0, 
                   parms=list(split="gini")) #division gini
arbol1150$variable.importance

imp <- arbol1150$variable.importance
imp_data <- as.data.frame(imp)
imp_data <- tibble::rownames_to_column(imp_data, "Variables") %>% arrange(desc(imp))
imp_data %>%
  mutate(Variables = fct_reorder(as.factor(Variables), imp)) %>%
  ggplot(aes(x=Variables, y=imp)) +
  theme(text = element_text(family = "lato", size=15))+
  geom_bar(stat="identity", fill='#9698ed', alpha=.6, width=.4)+
  coord_flip()+
  labs(title = "Importancia de las variables en el árbol ganador")

#Las 5 variables mas importantes son 
#age, totchol, healthgen.Fair