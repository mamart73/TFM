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
nhs <- readRDS("nhs.rds")

#eliminamos id
nhs$id <- NULL

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
  
means <- apply(nhs[,numericas],2,mean,na.rm=TRUE)
sds <- sapply(nhs[,numericas],sd,na.rm=TRUE)
numericas_estan <- scale(nhs[,numericas],center=means,scale=sds)

nhs <- data.frame(cbind(numericas_estan,nhs[,c(categoricas,objetivo)]))
saveRDS(nhs, file = "nhs_estan.rds")

#-----2. SELECCI?N DE VARIABLES-----
#-----2.1.AIC-----
#AIC
full <- glm(diabetes~., data=nhs, family = binomial(link = "logit"))
null <- glm(diabetes~1, data=nhs, family = binomial(link = "logit"))

#Partimos de una lista vacia y vamos metiendo y sacando
selec_AIC <- stepAIC(null, scope=list(upper=full), direction='both', trace=FALSE)
#saveRDS(selec_AIC, file = "selec_AIC.rds")
selec_AIC <- readRDS("selec_AIC.rds")

summary(selec_AIC)
vec <- (names(selec_AIC[[1]]))
length(vec)
#53 variables
dput(vec)

# c("(Intercept)", "age", "weight", "healthgen.Fair", "race1.White",
#   "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
#   "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
#   "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
#   "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
#   "smokeage.NA", "education.High_School", "daysmenthlthbad.Few",
#   "sexage.15orLess", "sexnumpartyear.MoreThan1", "marij.NotAsked",
#   "comphrsday.0_hrs", "age1stbaby.22orMore", "smokenow.Yes", "smoke100.No",
#   "tvhrsday.from4hr", "race1.Hispanic", "race1.Mexican", "homerooms",
#   "samesex.Yes", "sexnumpartnlife.MoreThan25", "sexnumpartnlife.11to25",
#   "poverty", "urinevol1", "ageregmarij.Before18", "marij.Never",
#   "daysmenthlthbad.NA", "littleinterest.NA", "depressed.NA", "alcoholyear",
#   "nbabies.NA", "maritalstatus.Married", "education.Some_College",
#   "race1.Black", "nbabies.MoreThan2", "littleinterest.Several",
#   "littleinterest.None")

#-----2.2.BIC-----
#BIC
#Partimos de una lista vacia y vamos metiendo y sacando
selec_BIC <- stepAIC(null, scope=list(upper=full), direction='both', trace=FALSE, k=log(nrow(nhs)))
#saveRDS(selec_BIC, file = "selec_BIC.rds")
selec_BIC <- readRDS("selec_BIC.rds")

summary(selec_BIC)
vec <- (names(selec_BIC[[1]]))
length(vec)
#23 variables
dput(vec)

# c("(Intercept)", "age", "weight", "healthgen.Fair", "race1.White",
#   "totchol", "healthgen.Vgood", "harddrugs.NotAsked", "directchol",
#   "sleeptrouble.Yes", "pulse", "height", "sexnumpartnlife.6to10",
#   "surveyyr.2009_10", "sexnumpartyear.None", "smokenow.No", "alcohol12plusyr.No",
#   "bpsys", "bpdia", "maritalstatus.Separated", "agefirstmarij.18orMore",
#   "smokeage.NA", "education.High_School")

#-----2.3.AIC, BIC repetido-----
#funcion steprepetido

# La funcion steprepetido permite realizar el proceso training test
# varias veces obteniendo el modelo por stepwise sobre datos train
# y la tabla de frecuencias de los modelos escogidos.
# 
# Previamente hay que pre procesar y meter en dummies las variables
# categoricas y meterlas en la lista listconti.

steprepetido<- function(data=data,vardep="vardep",
                        listconti="listconti",sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")
{
  
  library(MASS)
  library(dplyr)
  
  resultados<-data.frame(c())
  data<-data[,c(listconti,vardep)]
  formu1<-formula(paste(vardep,"~.",sep=""))
  formu2<-formula(paste(vardep,"~1",sep=""))
  listamodelos<-list()
  
  for (semilla in sinicio:sfinal)
  {
    
    set.seed(semilla)
    sample <- sample.int(n = nrow(data),
                         size = floor(porcen*nrow(data)), replace = F)
    
    train <- data[sample, ]
    test  <- data[-sample, ]
    
    full<-glm(formu1,data=train, family = binomial(link = "logit"))
    null<-glm(formu2,data=train, family = binomial(link = "logit"))
    
    
    if  (criterio=='AIC')
    {
      selec1<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)
    } 
    else   if  (criterio=='BIC')
    {
      k1=log(nrow(train))
      selec1<-stepAIC(null,scope=list(upper=full),direction="both",k=k1,trace=FALSE)
    }
    
    vec<-(names(selec1[[1]]))
    
    
    # CAMBIOS
    
    cosa<-as.data.frame(table(vec))
    cosa<-as.data.frame(t(cosa))
    colnames(cosa)<-vec
    
    # 1) creo un vector con todas las variables input y ceros
    # 2) voy añadiendo
    
    cosa<-cosa[2,]
    cosa<-cosa[,-c(1)]
    cosa<- data.frame(lapply(cosa, function(x) as.numeric(as.character(x))))
    cosa$id<-semilla
    
    vectormodelo<-list(names(cosa),semilla)
    listamodelos<-append(listamodelos,vectormodelo)  
    
    if (semilla==sinicio)
    {
      listamod<-cosa
    }
    
    else if (semilla!=sinicio)
    {
      listamod<-suppressMessages(full_join(cosa,listamod,by = NULL, copy =TRUE))
    }
    
  }
  
  listamod[is.na(listamod)] <- 0
  
  nom<-names(listamod)
  listamod$modelo<-""
  for (i in 1:nrow(listamod))
  {
    listamod[i,c("modelo")]<-""
    listamod[i,c("contador")]=0
    
    for (vari in nom)
    { 
      if (listamod[i,vari]==1)
      {
        listamod[i,c("modelo")]<-paste(listamod[i,c("modelo")],vari,collapse="",sep="+")
        listamod[i,c("contador")]=listamod[i,c("contador")]+1
      }
      
    }
    
  }
  
  listamod$modelo<-substring(listamod$modelo, 2)
  
  tablamod<-as.data.frame(table(listamod$modelo))
  names(tablamod)<-c("modelo","Freq")
  
  tablamod<-tablamod[order(-tablamod$Freq,tablamod$modelo),]
  
  nuevo<-listamod[,c("modelo","id","contador")]
  
  uni<-full_join(tablamod,nuevo,by ="modelo", copy =TRUE)
  
  uni= uni[!duplicated(uni$modelo),]
  uni$semilla<-semilla
  
  li1<-list()
  # str(listamodelos)
  for (i in 1:nrow(uni))
  {
    for (j in 1:length(listamodelos))
    {
      if (uni[i,c("id")]==listamodelos[j][[1]])
      {
        k<-as.vector(listamodelos[j-1][[1]])
        length(k)<-length(k)-1
        li1<-c(li1,list(k))
        j=length(listamodelos)
      }
    } 
    
  }
  
  uni$semilla<-NULL
  uni$id<-NULL
  return(list(uni,li1))
  
}


#AIC repetido
listaAIC <- steprepetido(data=nhs, vardep=c("diabetes"), listconti=c(
  'surveyyr.2009_10','gender.male','age','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','race1.Other','education.8th_Grade',
  'education.9_11th_Grade','education.College_Grad','education.High_School',
  'education.NotAsked','education.Some_College','maritalstatus.Married',
  'maritalstatus.NeverMarried','maritalstatus.Separated','maritalstatus.Widowed',
  'poverty','homerooms','homeown.Own','work.Working','weight','height','pulse',
  'directchol','totchol','urinevol1','urineflow1','healthgen.Fair',
  'healthgen.Good','healthgen.NA','healthgen.Vgood','daysphyshlthbad.Few',
  'daysphyshlthbad.Many','daysphyshlthbad.NA','daysphyshlthbad.None',
  'daysphyshlthbad.Several','daysmenthlthbad.Few','daysmenthlthbad.Many',
  'daysmenthlthbad.NA','daysmenthlthbad.None','daysmenthlthbad.Several',
  'littleinterest.Most','littleinterest.NA','littleinterest.None',
  'littleinterest.Several','depressed.Most','depressed.NA','depressed.None',
  'depressed.Several','npregnancies.1or2','npregnancies.MoreThan2',
  'npregnancies.NA','nbabies.0to2','nbabies.MoreThan2','nbabies.NA',
  'age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA','sleephrsnight',
  'sleeptrouble.Yes','physactivedays','tvhrsday.0to1hr','tvhrsday.2to3hr',
  'tvhrsday.from4hr','comphrsday.0_hrs','comphrsday.0_to_1_hr',
  'comphrsday.from1hr','alcohol12plusyr.NA','alcohol12plusyr.No',
  'alcohol12plusyr.Yes','alcoholday.1','alcoholday.2or3','alcoholday.MoreThan3',
  'alcoholday.NA','alcoholyear','smokenow.No','smokenow.Yes','smoke100.No',
  'smoke100.Yes','smokeage.18orMore','smokeage.Before18','smokeage.NA',
  'agefirstmarij.18orMore','agefirstmarij.Before18','agefirstmarij.NA',
  'ageregmarij.18orMore','ageregmarij.Before18','ageregmarij.NotApply',
  'harddrugs.NA','harddrugs.No','harddrugs.NotAsked','harddrugs.Yes',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.18orMore','sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','bpsys','bpdia','marij.HasTried','marij.MarijUser','marij.NA',
  'marij.Never','marij.NotAsked'), 
  sinicio=12345, sfinal=12355, porcen=0.8, criterio="AIC")

tablaAIC <- listaAIC[[1]]
dput(listaAIC[[2]][[1]])

# c("age", "weight", "healthgen.Fair", "race1.White", "totchol", 
#   "healthgen.Vgood", "harddrugs.NotAsked", "directchol", "pulse", 
#   "sleeptrouble.Yes", "surveyyr.2009_10", "height", "sexnumpartyear.None", 
#   "agefirstmarij.18orMore", "smokenow.No", "alcohol12plusyr.No", 
#   "maritalstatus.Separated", "sexnumpartnlife.6to10", "bpsys", 
#   "bpdia", "education.Some_College", "marij.NotAsked", "sexnumpartyear.MoreThan1", 
#   "daysmenthlthbad.Few", "sexage.15orLess", "smokenow.Yes", "race1.Other", 
#   "tvhrsday.from4hr", "smokeage.NA", "education.NotAsked", "comphrsday.0_hrs", 
#   "age1stbaby.22orMore", "urinevol1", "marij.MarijUser", "sexage.16to18", 
#   "alcoholyear", "homerooms", "littleinterest.Several", "daysmenthlthbad.Several", 
#   "depressed.Most", "marij.Never", "daysmenthlthbad.None", "nbabies.0to2", 
#   "gender.male", "work.Working", "littleinterest.None", "daysmenthlthbad.Many", 
#   "littleinterest.Most", "depressed.NA", "daysphyshlthbad.None")

#-----2.4.SBF-----
# variable dependiente y
y<-nhs[,"diabetes"]
# variables input
x<-nhs[,c(
  'surveyyr.2009_10','gender.male','age','race1.Black','race1.Hispanic',
  'race1.Mexican','race1.White','education.8th_Grade',
  'education.9_11th_Grade','education.College_Grad','education.High_School',
  'education.Some_College','maritalstatus.Married',
  'maritalstatus.NeverMarried','maritalstatus.Separated',
  'poverty','homerooms','homeown.Own','work.Working','weight','height','pulse',
  'directchol','totchol','urinevol1','urineflow1','healthgen.Fair',
  'healthgen.Good','healthgen.Vgood','daysphyshlthbad.Few',
  'daysphyshlthbad.Many','daysphyshlthbad.NA','daysphyshlthbad.None',
  'daysmenthlthbad.Few','daysmenthlthbad.Many',
  'daysmenthlthbad.NA','daysmenthlthbad.None',
  'littleinterest.NA','littleinterest.None',
  'littleinterest.Several','depressed.NA','depressed.None',
  'depressed.Several','npregnancies.1or2','npregnancies.MoreThan2',
  'npregnancies.NA','nbabies.0to2','nbabies.MoreThan2','nbabies.NA',
  'age1stbaby.21orLess','age1stbaby.22orMore','age1stbaby.NA','sleephrsnight',
  'sleeptrouble.Yes','physactivedays','tvhrsday.0to1hr','tvhrsday.2to3hr',
  'tvhrsday.from4hr','comphrsday.0_hrs','comphrsday.0_to_1_hr',
  'comphrsday.from1hr','alcohol12plusyr.No',
  'alcohol12plusyr.Yes','alcoholday.1','alcoholday.2or3',
  'alcoholday.NA','alcoholyear','smokenow.No','smokenow.Yes','smoke100.No',
  'smoke100.Yes','smokeage.18orMore','smokeage.Before18','smokeage.NA',
  'agefirstmarij.18orMore','agefirstmarij.Before18','agefirstmarij.NA',
  'ageregmarij.18orMore','ageregmarij.Before18','ageregmarij.NotApply',
  'harddrugs.NA','harddrugs.No','harddrugs.NotAsked',
  'sexever.NA','sexever.No','sexever.Yes','sexage.15orLess','sexage.16to18',
  'sexage.NA','sexnumpartnlife.11to25','sexnumpartnlife.1to5',
  'sexnumpartnlife.6to10','sexnumpartnlife.MoreThan25','sexnumpartnlife.NA',
  'sexnumpartnlife.None','sexnumpartyear.1','sexnumpartyear.MoreThan1',
  'sexnumpartyear.NA','sexnumpartyear.None','samesex.NA','samesex.No',
  'samesex.Yes','bpsys','bpdia','marij.HasTried','marij.MarijUser',
  'marij.Never','marij.NotAsked')]

#SBF
filtro<-sbf(x,y,sbfControl = sbfControl(functions = rfSBF, 
                                        method = "cv", verbose = FALSE))
#saveRDS(filtro, file = "selec_SBF.rds")
filtro <- readRDS("selec_SBF.rds")

a<-dput(filtro$optVariables)
length(a)

#95 variables
# c("surveyyr.2009_10", "age", "race1.Black", "race1.White", "education.8th_Grade",
#   "education.9_11th_Grade", "education.College_Grad", "maritalstatus.Married",
#   "maritalstatus.NeverMarried", "maritalstatus.Separated", "poverty",
#   "homeown.Own", "work.Working", "weight", "height", "pulse", "directchol",
#   "totchol", "urinevol1", "urineflow1", "healthgen.Fair", "healthgen.Vgood",
#   "daysphyshlthbad.Few", "daysphyshlthbad.Many", "daysphyshlthbad.NA",
#   "daysphyshlthbad.None", "daysmenthlthbad.Many", "daysmenthlthbad.NA",
#   "daysmenthlthbad.None", "littleinterest.NA", "littleinterest.None",
#   "depressed.NA", "depressed.None", "npregnancies.1or2", "npregnancies.MoreThan2",
#   "npregnancies.NA", "nbabies.0to2", "nbabies.MoreThan2", "nbabies.NA",
#   "age1stbaby.21orLess", "age1stbaby.NA", "sleephrsnight", "sleeptrouble.Yes",
#   "physactivedays", "tvhrsday.0to1hr", "tvhrsday.from4hr", "comphrsday.0_hrs",
#   "comphrsday.from1hr", "alcohol12plusyr.No", "alcohol12plusyr.Yes",
#   "alcoholday.1", "alcoholday.2or3", "alcoholday.NA", "alcoholyear",
#   "smokenow.No", "smokenow.Yes", "smoke100.No", "smoke100.Yes",
#   "smokeage.18orMore", "smokeage.Before18", "smokeage.NA", "agefirstmarij.18orMore",
#   "agefirstmarij.Before18", "agefirstmarij.NA", "ageregmarij.18orMore",
#   "ageregmarij.Before18", "ageregmarij.NotApply", "harddrugs.NA",
#   "harddrugs.No", "harddrugs.NotAsked", "sexever.NA", "sexever.No",
#   "sexever.Yes", "sexage.15orLess", "sexage.16to18", "sexage.NA",
#   "sexnumpartnlife.11to25", "sexnumpartnlife.1to5", "sexnumpartnlife.6to10",
#   "sexnumpartnlife.MoreThan25", "sexnumpartnlife.NA", "sexnumpartnlife.None",
#   "sexnumpartyear.1", "sexnumpartyear.MoreThan1", "sexnumpartyear.NA",
#   "sexnumpartyear.None", "samesex.NA", "samesex.No", "samesex.Yes",
#   "bpsys", "bpdia", "marij.HasTried", "marij.MarijUser", "marij.Never",
#   "marij.NotAsked")

#-----2.5.RFE-----
#RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=4)
#funcion=rfFuncs es usando el algoritmo random forest

results <- rfe(x, y, sizes=c(50:55), rfeControl=control)
#sizes=k tama?o de los subconjuntos de variables input en los cuales se
#ordenan las vriables por orden de importancia 
#saveRDS(results, file = "selec_RFE.rds")
results <- readRDS("selec_RFE.rds")

selecrfe<-results$optVariables
length(selecrfe)
#55 variables con k=50:55 con metrica por defecto
#dput(selecrfe)
# c("age", "totchol", "height", "weight", "urineflow1", "urinevol1", 
#   "pulse", "directchol", "homerooms", "bpdia", "poverty", "healthgen.Fair", 
#   "bpsys", "healthgen.Vgood", "sleephrsnight", "marij.NotAsked", 
#   "race1.White", "alcoholyear", "education.9_11th_Grade", "education.Some_College", 
#   "race1.Hispanic", "maritalstatus.Married", "comphrsday.0_to_1_hr", 
#   "race1.Mexican", "smokeage.18orMore", "gender.male", "homeown.Own", 
#   "smoke100.No", "tvhrsday.2to3hr", "alcohol12plusyr.Yes", "sleeptrouble.Yes", 
#   "smokeage.Before18", "education.High_School", "maritalstatus.Separated", 
#   "daysmenthlthbad.Few", "comphrsday.0_hrs", "daysmenthlthbad.None", 
#   "comphrsday.from1hr", "smokenow.Yes", "daysphyshlthbad.Few", 
#   "maritalstatus.NeverMarried", "surveyyr.2009_10", "alcoholday.1", 
#   "daysmenthlthbad.Many", "smoke100.Yes", "sexnumpartnlife.11to25", 
#   "sexage.15orLess", "sexage.16to18", "sexnumpartnlife.1to5", "age1stbaby.22orMore", 
#   "daysphyshlthbad.None", "healthgen.Good", "education.College_Grad", 
#   "nbabies.0to2", "physactivedays")

#-----2.6.Boruta-----
# BORUTA
library(Boruta)
out.boruta <- Boruta(diabetes~., data = nhs)
#saveRDS(out.boruta, file = "selec_boruta.rds")
out.boruta <- readRDS("selec_boruta.rds")

print(out.boruta)
summary(out.boruta)
sal<-data.frame(out.boruta$finalDecision)
sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
dput(row.names(sal2))
length(dput(row.names(sal2)))
#108 variables
# 
# c("age", "poverty", "homerooms", "weight", "height", "pulse", 
#   "directchol", "totchol", "urinevol1", "urineflow1", "sleephrsnight", 
#   "physactivedays", "alcoholyear", "bpdia", "bpsys", "surveyyr.2009_10", 
#   "gender.male", "race1.Black", "race1.Hispanic", "race1.Mexican", 
#   "race1.White", "education.8th_Grade", "education.9_11th_Grade", 
#   "education.College_Grad", "education.High_School", "education.Some_College", 
#   "maritalstatus.Married", "maritalstatus.NeverMarried", "maritalstatus.Separated", 
#   "homeown.Own", "work.Working", "healthgen.Fair", "healthgen.Good", 
#   "healthgen.Vgood", "daysphyshlthbad.Few", "daysphyshlthbad.Many", 
#   "daysphyshlthbad.NA", "daysphyshlthbad.None", "daysmenthlthbad.Few", 
#   "daysmenthlthbad.Many", "daysmenthlthbad.NA", "daysmenthlthbad.None", 
#   "littleinterest.NA", "littleinterest.None", "littleinterest.Several", 
#   "depressed.NA", "depressed.None", "depressed.Several", "npregnancies.1or2", 
#   "npregnancies.MoreThan2", "npregnancies.NA", "nbabies.0to2", 
#   "nbabies.MoreThan2", "nbabies.NA", "age1stbaby.21orLess", "age1stbaby.22orMore", 
#   "age1stbaby.NA", "sleeptrouble.Yes", "tvhrsday.0to1hr", "tvhrsday.2to3hr", 
#   "tvhrsday.from4hr", "comphrsday.0_hrs", "comphrsday.0_to_1_hr", 
#   "comphrsday.from1hr", "alcohol12plusyr.No", "alcohol12plusyr.Yes", 
#   "alcoholday.1", "alcoholday.2or3", "alcoholday.NA", "smokenow.No", 
#   "smokenow.Yes", "smoke100.No", "smoke100.Yes", "smokeage.18orMore", 
#   "smokeage.Before18", "smokeage.NA", "agefirstmarij.18orMore", 
#   "agefirstmarij.Before18", "agefirstmarij.NA", "ageregmarij.18orMore", 
#   "ageregmarij.Before18", "ageregmarij.NotApply", "harddrugs.NA", 
#   "harddrugs.No", "harddrugs.NotAsked", "sexever.NA", "sexever.No", 
#   "sexever.Yes", "sexage.15orLess", "sexage.16to18", "sexage.NA", 
#   "sexnumpartnlife.11to25", "sexnumpartnlife.1to5", "sexnumpartnlife.6to10", 
#   "sexnumpartnlife.MoreThan25", "sexnumpartnlife.NA", "sexnumpartnlife.None", 
#   "sexnumpartyear.1", "sexnumpartyear.MoreThan1", "sexnumpartyear.NA", 
#   "sexnumpartyear.None", "samesex.NA", "samesex.No", "samesex.Yes", 
#   "marij.HasTried", "marij.MarijUser", "marij.Never", "marij.NotAsked"
# )

#-----2.7.MMPC-----
#MMPC
library(MXM)

mmpc1 <- MMPC(y, x, max_k = 3, hash = TRUE, test = "testIndFisher")
#saveRDS(mmpc1, file = "selec_mmpc.rds")
mmpc1 <- readRDS("selec_mmpc.rds")
mmpc1@selectedVars

a<-dput(names(x[,c(mmpc1@selectedVars)]))

length(a)
#16 variables

# c("age", "race1.Black", "maritalstatus.Separated", "work.Working",
#   "weight", "directchol", "totchol", "healthgen.Fair", "healthgen.Vgood",
#   "daysphyshlthbad.Many", "sleeptrouble.Yes", "physactivedays",
#   "tvhrsday.from4hr", "alcoholyear", "harddrugs.NotAsked", "bpsys")

#-----2.8.SES-----
#SES
#cambio variable objetivo a numerica
y1<-ifelse(y=="Yes",1,0)
SES1 <- SES(y1, x, max_k = 3, hash = TRUE, test = "testIndFisher")
#saveRDS(SES1, file = "selec_SES.rds")
SES1 <- readRDS("selec_SES.rds")
SES1@selectedVars

dput(names(x[,c(SES1@selectedVars)]))
# c("age", "race1.Black", "maritalstatus.Separated", "work.Working",
#   "weight", "directchol", "totchol", "healthgen.Fair", "healthgen.Vgood",
#   "daysphyshlthbad.Many", "sleeptrouble.Yes", "physactivedays",
#   "tvhrsday.from4hr", "alcoholyear", "harddrugs.NotAsked", "bpsys")

a<-dput(names(x[,c(SES1@selectedVars)]))

length(a)
#16 variables


#-----2.9.Random forest y arboles------
#Las 3 variables mas importantes de arboles son:
#age, totchol y healthgen.Fair

#Las 10 variables mas importantes de random forest son:
#age, totchol, weight, healthgen.Fair, healthgen.Vgood, directchol, height, 
#bpdia, poverty y bpsys

#-----3. Algunas conclusiones-----

#AIC selecciona 52 variables
#BIC selecciona 22 variables
#SBF selecciona 95 vaariables
#RFE selecciona 55 variables
#Boruta selecciona 108 variables, es decir, todas las input
#SES y MMPC seleccionan las mismas variables (16 variables)
#Arbol nos quedamos con las 3 más relevantes
#Random Forest nos quedaos con las 10 más relevantes


