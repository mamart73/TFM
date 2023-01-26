#----1.Librerias----
rm(list = ls())
library(tidyverse)
library(tidymodels)
library(dplyr)
#library(plyr)
library(skimr)
library(ggplot2)
library(ggthemes)
library(showtext)
library(showtextdb)
library(extrafont)
library(patchwork)
library(viridis)
library(dummy)
library(corrplot)
library(stringr)
library(NHANES)
library(modeest)
library(VIM)
library(mice)
library(ggThemeAssist)

#----2.Importar dataset----
nhanes <- NHANESraw
#Pasamos el nombre de todas las variables a min?scula y borramos NA en la variable objetivo
nhanes <- nhanes %>% rename_all (tolower) %>% drop_na(diabetes)
#Guardamos dataset completo nhanes en un csv
#write.csv(nhanes, "C:/Users/MediaService/Desktop/MASTER/TFM/nhanesSAS.csv")
nhanes %>% skim()

#----3.Exploracion comun-----

### race3 -> 
#Se elimina. Tiene NA y es la misma variable que race1 (esta no tiene
#NA), pero con la diferencia de que race3 tiene una categor?a m?s (Asian), la 
#cual est? contemplada en el grupo Other en race1. Al estar recogida ?nicamente 
#en una de las dos encuestas, nos quedamos solo con race1.

### hhincome -> 
#Se elimina. Es una variable categ?rica que representa el ingreso bruto anual
#por intervalos. Existe variable an?loga num?rica (hhincomemid), la cu?l tiene 
#como valor elpunto intermedio de los extremos de los intervalos de esta 
#variable. Nos conviene quedarnos con la versi?n numerica

### bmicatunder20yrs ->
#Se elimina. Es una categorizaci?n para menores de 20 a?os de la variable BMI, 
#la cual se obtiene a partir del peso y la altura.

### bmi_who ->
#Se elimina. Es una categorizaci?n de la variable BMI, la cual se obtiene a 
#partir del peso y la altura.

### physactive -> *fusion*
#Se trata de una variable binaria en la que se expone si el participante hace
#deporte vigoroso-intenso. Esta variable ha sido recogida para participantes de 
#12 o m?s a?os. Existe una variable num?rica (physactivedays), la cual refleja 
#el n?mero medio de d?as en una semana en que el participante realiza ejercicio 
#de intensidad moderada o vigorosa. Procederemos a fusionar estas 2 variables, 
#imputando en la num?rica como valor 0 (d?as) a los participantes que hayan
#respondido No a physactive. De esta forma podremos prescindir de esta variable
#manteniendo toda la informacion que nos proporciona

### pregnantnow ->
#Esta variable refleja el estado de embarazo de una participante. No obstante, 
#por la forma en que ha sido recogida no es fiable la informaci?n que 
#proporciona (solo se revela el resultado en un porcentaje de edad determinado 
#a pesar de hacer pruebas a un rango de edad mayor, si la participante dice que
#no est? embarazada, prevalece lo que ella dice a una prueba m?dica...), 
#por lo que optamos por no usarla en este trabajo

### agemonths ->
#Se elimina. Tiene NA y representa la edad en meses de un partipante. Tenemos
#otra variable que representa la edad en a?os y no tiene NA, por lo que optamos 
#por quedarnos con ella.

### hhincomemid y poverty ->
#La variable hhincomemid refleja el ingreso bruto anual y tiene como valor el 
#punto medio de los extremos de los intervalos de hhincome (variable eliminada).
#Tiene un 10% de NA.
#La variable poverty refleja un ratio entre el ingreso familiar y las pautas de 
#pobreza. Tiene un 9% de NA

#Veamos la correlaci?n de estas variables ya que a priori parece que van a tener
#alta correlaci?n

# pobreza <-  nhanes %>% select(hhincomemid, poverty, homerooms)
# pobreza <- pobreza %>% drop_na()
# 
# correlacion<-round(cor(pobreza), 1)
# corrplot(correlacion, method="number", type="upper")

#Las variables hhincomemid y poverty tienen una correlacion muy alta (0.9), por 
#lo que eliminaremos una de las 2 variables

# nhanes %>% count(is.na(hhincomemid) & is.na(poverty))
#1737 NAs en ambas variables
# nhanes %>% count(is.na(hhincomemid))
#1990 NAs en hhincomemid -> 253 NAs en hhincomemid con valor en poverty
# nhanes %>% count(is.na(poverty))
#1764 NAs en poverty -> 27 NAs en poverty con valor en hhincomemid

#Se decide eliminar hhincomemid ya que tiene mayor n?mero de NA y se trata de 
#una variable discreta cuyos valores representan puntos medios de intervalos.
#Sin embargo, poverty, a pesar de ser menos interpretable, nos porporciona
#datos m? excatos sobre el nivel de vida del participante, lo que nos puede
#ayudar a la hora de predecir la variable objetivo.

#nhanes %>% group_by(hhincomemid) %>% summarise(media=mean(poverty, na.rm=TRUE))

### headcirc ->
#Se elimina. Refleja la longitud de beb?s menores a 6 meses. Tiene un 100% de NA
#tras eliminar los NA en la variable objetivo, por lo que se elimina

### bmi -> 
#Se elimina. Se calcula a partir de la altura y el peso de una persona 
#(variables que tenemos), por lo que se elimina

### urinevol2 ->
#Se elimina. Tiene casi 90% de NA y refleja el volumen de orina de un
#participante. Adem?s, hay otra variable (urinevol1) que refleja lo mismo pero
#en otra lectura.

### urineflow2 ->
#Se elimina. Tiene casi 90% de NA y refleja el flujo de orina de un
#participante. Adem?s, hay otra variable (urineflow2) que refleja lo mismo pero
#en otra lectura.

### diabetesage ->
#Se elimina. No tiene sentido mantener como variable de entrada la edad a la que
#se le ha diagnosticado diabetes a un participante cuando tener o no diabetes
#es la variable que se pretende predecir.

### bpsysave, bpsys1, bpsys2, bpsys2 ->
#Estas variables reflejan distintas lecturas de la presi?n arterial sist?lica.
#Tiene coeficientes de correlaci?n de Pearson muy cercanos a 1 entre ellas dos a
#dos, por lo que estar?amos d?ndole 4 veces m?s importancia a esta variable que 
#al resto si mantenemos las 4 variables. Para no excluir ninguna de las 4 
#lectuas, crearemos una variable nueva que ser? la media de todas las variables,
#siempre y cuando no tengan valor NA. En este caso, se calcular? la media de
#aquellas cuyo valor no sea NA

### bpdiaave, bpdia1, bpdia2, bpdia3 ->
#Estas variables reflejan distintas lecturas de la presi?n arterial diast?lica.
#Tiene coeficientes de correlaci?n de Pearson muy cercanos a 1 entre ellas dos a
#dos, por lo que estar?amos d?ndole 4 veces m?s importancia a esta variable que 
#al resto si mantenemos las 4 variables. Para no excluir ninguna de las 4 
#lectuas, crearemos una variable nueva que ser? la media de todas las variables,
#siempre y cuando no tengan valor NA. En este caso, se calcular? la media de
#aquellas cuyo valor no sea NA

### wtint2yr, wtme2yr, sdmvpsu, sdmvpsu ->
#Las eliminamos. Son variables de peso estad?stico de las cuales no haremos uso
#en el desarrollo de este trabajo

#Imputamos como NA valores incoherentes en physactive, 
#incluimos info de physactive en physactivedays,
#creamos las variables media de la presi?on arterial dias?oolica y presi?n 
#arterial diast?lica y
#eliminamos variables comunes a los 2 datasets cuyo motivo hemos descrito
#en las lineas anteriores

#----4.Depuracion comun y creacion datasets-----
nhanes <- nhanes %>% mutate(physactivedays=ifelse(physactivedays>7,NA,physactivedays),
                            physactivedays=ifelse(physactive=='No',0,physactivedays),
                            bpsys = apply(nhanes[ ,c("bpsysave","bpsys1","bpsys2","bpsys3")], 1, mean, na.rm = TRUE),
                            bpdia = apply(nhanes[ ,c("bpdiaave","bpdia1","bpdia2","bpdia3")], 1, mean, na.rm = TRUE)) %>%
                     dplyr::select(-race3, -hhincome, -bmicatunder20yrs, -bmi_who, 
                            -physactive, -pregnantnow, -agemonths, -hhincomemid, 
                            -headcirc, -bmi, 
                            -urinevol2, -urineflow2, -diabetesage, -bpsysave, 
                            -bpsys1, -bpsys2, -bpsys3, -bpdiaave, -bpdia1, -bpdia2, 
                            -bpdia3, -wtint2yr, -wtmec2yr, -sdmvpsu, -sdmvstra)


#Creamos datasets
nhanesyoungraw <- nhanes %>% filter (age<18)
nhanesoldraw <- nhanes %>% filter(age>17)

#----5.1.Exploracion nhanesyoung-----
#En este dataset tenemos participantes de hasta 17 a?os
nhanesyoungraw %>% skim()

#CATEGORICAS

### surveyyr ->
#Refleja el a?o de la encuesta. No tiene NA

### gender ->
#Refleja el genero del participante. No tiene NA

### race1 ->
#Refleja la raza del partipante. No tiene NA

### education ->
#Esta variable refleja el nivel educativo del participante. Tiene 100% de NA.
#Se debe a que no esta recogida para el rango de edad sobre el que estamos 
#trabajando, por lo que la ELIMINAMOS

### maritalstatus ->
#Esta variable refleja el estado civil del participante. Tiene 100% de NA.
#Se debe a que no esta recogida para el rango de edad sobre el que estamos 
#trabajando, por lo que la ELIMINAMOS

### homeown -> 
#Se trata de la tipologia de propiedad de la vivienda donde vive el participante
#Teniendo en cuenta los porcentajes de observaciones en cada categor?a 
#decidimos reagrupar las categor?as Rent y Other.
#Tiene menos de 1% de NA. Se imputan

### work ->
#Refleja la relaci?n con respecto al trabajo. Tiene m?s de un 90% de NA. Esto
#se puede deber a que en EEUU la edad m?nima para trabajar es 14 a?os. 
#Se podr?an imputar como NotWorking en menores de 14 a?os. Veamos como quedar?a
#la distribucion

# nhanesyoungraw %>% mutate(work= case_when((age<=13) ~ "NotWorking",
#                                          TRUE ~ as.character(work))) %>%
#                             count(work, diabetes) %>% group_by(work) %>% 
#                             mutate(porc=n/sum(n))

# A tibble: 7 x 4
# Groups:   work [4]
# work       diabetes     n    porc
# <chr>      <fct>    <int>   <dbl>
#   1 Looking    No          33 0.943  
# 2 Looking    Yes          2 0.0571 
# 3 NotWorking No        6256 0.997  
# 4 NotWorking Yes         20 0.00319
# 5 Working    No         112 1      
# 6 NA         No         647 0.988  
# 7 NA         Yes          8 0.0122

#Debido a la poca variabilidad de esta variable y su relaci?n con la variable 
#objetivo, la ELIMINAMOS

### diabetes ->
#Variable objetivo

### healthgen ->
#Refleja una calificaci?n categ?rica del estado de salud autopercibido y cuenta 
#con 5 categor?as. Tiene 74% de NA. Esta variable ha sido recogida para 
#participantes de 12 o m?s a?os, por lo que imputaremos este rango de edad no 
#considerado en una nueva categoria NotAsked. Ademas, habria que considerar los
#NA reales de esta variable, dado que al ser subjetiva el no haber contestado 
#podria significar algo

# change <- nhanesyoungraw %>% mutate(healthgen=case_when(age<12 ~ "NotAsked",
#                                               TRUE ~ as.character(healthgen))) 
# 
# change %>% group_by(healthgen) %>%
#   count(healthgen, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 13 x 4
# Groups:   healthgen [7]
# healthgen diabetes     n    porc
# <chr>     <fct>    <int>   <dbl>
#   1 Excellent No         270 1      
# 2 Fair      No         195 0.970  
# 3 Fair      Yes          6 0.0299 
# 4 Good      No         708 0.990  
# 5 Good      Yes          7 0.00979
# 6 NotAsked  No        5070 0.998  
# 7 NotAsked  Yes         10 0.00197
# 8 Poor      No          20 0.909  
# 9 Poor      Yes          2 0.0909 
# 10 Vgood     No         631 0.997  
# 11 Vgood     Yes          2 0.00316
# 12 NA        No         154 0.981  
# 13 NA        Yes          3 0.0191

# prop.table(table(change$healthgen))
# 
# Excellent        Fair        Good    NotAsked        Poor       Vgood 
# 0.039011704 0.029042046 0.103308770 0.733997977 0.003178731 0.091460772 

#Teniendo en cuenta los porcentajes de observaciones en cada categor?a y la
#relaci?n de cada una de estas con la variable objetivo decidimos reagrupar las
#categorias:
#Excellent y Vgood
#Fair, Poor y Good

#Por otro lado, a pesar de que los NA puedan tener algun significado, al tratarse
#de una cantidad tan peque?a los imputamos

### littleinterest ->
#Refleja una calificaci?n categ?rica del n?mero de d?as que el participante 
#ha tenido inter?s en hacer cosas. Tiene un 100% de NA debiendose esto a que
#esta variable ha sido recogida para participantes de 18 o m?s a?os, por lo que
#en este dataset la ELIMINAMOS

### depressed ->
#Refleja una calificaci?n categ?rica del n?mero de d?as que el participante 
#se ha sentido depresivo. Tiene un 100% de NA debiendose esto a que esta 
#variable ha sido recogida para participantes de 18 o m?s a?os, por lo que
#en este dataset la ELIMINAMOS

### sleeptrouble ->
#Se trata de una variable binaria en la que se expone si el participante le ha
#dicho a alg?n profesional de la salud si ha tenido problemas para dormir. 
#Tiene m?s de un 90% de NA debiendose esto a que esta variable ha sido recogida 
#para participantes de 16 o m?s a?os

#nhanesyoungraw %>% count(sleeptrouble, diabetes) %>% group_by(sleeptrouble) %>%
#  mutate(porc=n/sum(n))
# A tibble: 6 x 4
# Groups:   sleeptrouble [3]
# sleeptrouble diabetes     n    porc
# <fct>        <fct>    <int>   <dbl>
#   1 No           No         605 0.993  
# 2 No           Yes          4 0.00657
# 3 Yes          No          62 0.984  
# 4 Yes          Yes          1 0.0159 
# 5 NA           No        6381 0.996  
# 6 NA           Yes         25 0.00390

#Debido a su gran cantidad de NA y poca relaci?n con la variable objetivo, la
#ELIMINAMOS

### tvhrsday ->
#Refleja una calificaci?n categ?rica del n?mero medio de horas diarias que el 
#participante ha visto la televisi?n durante los ?ltimos 30 d?as y cuenta con
#6 categor?as. Esta variable ha sido recogida para participantes de 2 o m?s 
#a?os en la encuesta 2011-2012, mientras que no ha sido recogida en la encuesta
#2009-2010. Existe una variable num?rica (tvhrsdaychild), la cual refleja 
#el n?mero medio de horas al d?a en los ?ltimos 30 d?as en las que el 
#participante vio la televisi?n, recogida para participantes entre 2 y 11 a?os 
#en la encuesta del a?o 2009-2010. Haremos una fusi?n entre ambas variables para
#recoger la maxima informacion posible y crearemos una categoria nueva NotAsked
#para aquellos participantes que no hayan respondido a esta variable. La nueva 
#variable "fusion" sera categorica. Ademas, habria que considerar los NA reales 
#de esta variable, dado que al ser subjetiva el no haber contestado podria 
#significar algo

# change <- nhanesyoungraw %>% mutate(tvhrsdaychild=cut(tvhrsdaychild,
#                                       breaks=c(-Inf,0,1,2,3,4,Inf),
#                                       labels=c("0_hrs","1_hr","2_hr","3_hr","4_hr","More_4_hr")),
#                                   tvhrsday= case_when(surveyyr=="2009_10" & age>11 ~ "NotAsked",
#                                                       surveyyr=="2009_10" & age<2 ~ "NotAsked",
#                                                       is.na(tvhrsday) ~ as.character(tvhrsdaychild),
#                                                       TRUE ~ as.character(tvhrsday))) 
# change %>% group_by(tvhrsday) %>%
#   count(tvhrsday, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 16 x 4
# Groups:   tvhrsday [9]
# tvhrsday  diabetes     n    porc
# <chr>     <fct>    <int>   <dbl>
#   1 0_hrs     No         332 1      
# 2 0_to_1_hr No         471 0.998  
# 3 0_to_1_hr Yes          1 0.00212
# 4 1_hr      No        1232 0.998  
# 5 1_hr      Yes          2 0.00162
# 6 2_hr      No        1677 0.997  
# 7 2_hr      Yes          5 0.00297
# 8 3_hr      No         834 0.996  
# 9 3_hr      Yes          3 0.00358
# 10 4_hr      No         379 0.992  
# 11 4_hr      Yes          3 0.00785
# 12 More_4_hr No         493 0.994  
# 13 More_4_hr Yes          3 0.00605
# 14 NotAsked  No        1338 0.990  
# 15 NotAsked  Yes         13 0.00962
# 16 NA        No         292 1 

# prop.table(table(change$tvhrsday))
# 0_hrs  0_to_1_hr       1_hr       2_hr       3_hr       4_hr  More_4_hr   NotAsked 
# 0.04892426 0.06955497 0.18184497 0.24786325 0.12334218 0.05629237 0.07309166 0.19908635 

#Debido a las cantidades tan peque?as en cada categoria, decidimos hacer las 
#siguientes reagrupaciones:
#0_hrs, 0_to_1_hr y 1_hr
#2_hr y 3_hr
#4_hr y More_4_hr
#La categoria NotAsked creada se mantiene intacta
#Los NA se imputan

### comphrsday ->
#Refleja una calificaci?n categ?rica del n?mero medio de horas diarias que el 
#participante ha usado el ordenador durante los ?ltimos 30 d?as y cuenta con
#6 categor?as. Esta variable ha sido recogida para participantes de 2 o m?s 
#a?os en la encuesta 2011-2012, mientras que no ha sido recogida en la encuesta
#2009-2010. Existe una variable num?rica (comphrsdaychild), la cual refleja 
#el n?mero medio de horas al d?a en los ?ltimos 30 d?as en las que el 
#participante uso el ordenador, recogida para participantes entre 2 y 11 a?os 
#en la encuesta del a?o 2009-2010. Haremos una fusi?n entre ambas variables para
#recoger la maxima informacion posible y crearemos una categoria nueva NotAsked
#para aquellos participantes que no hayan respondido a esta variable. La nueva 
#variable "fusion" sera categorica. Ademas, habria que considerar los NA reales 
#de esta variable, dado que al ser subjetiva el no haber contestado podria 
#significar algo

# change <- nhanesyoungraw %>% mutate(comphrsdaychild=cut(comphrsdaychild,
#                                                     breaks=c(-Inf,0,1,2,3,4,Inf),
#                                                     labels=c("0_hrs","1_hr","2_hr","3_hr","4_hr","More_4_hr")),
#                                     comphrsday= case_when(surveyyr=="2009_10" & age>11 ~ "NotAsked",
#                                                       surveyyr=="2009_10" & age<2 ~ "NotAsked",
#                                                       is.na(comphrsday) ~ as.character(comphrsdaychild),
#                                                       TRUE ~ as.character(comphrsday)))
# change %>% group_by(comphrsday) %>%
#   count(comphrsday, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 15 x 4
# Groups:   comphrsday [9]
# comphrsday diabetes     n    porc
# <chr>      <fct>    <int>   <dbl>
#   1 0_hrs      No        1563 0.997  
# 2 0_hrs      Yes          4 0.00255
# 3 0_to_1_hr  No         935 0.999  
# 4 0_to_1_hr  Yes          1 0.00107
# 5 1_hr       No        1088 0.996  
# 6 1_hr       Yes          4 0.00366
# 7 2_hr       No         549 0.989  
# 8 2_hr       Yes          6 0.0108 
# 9 3_hr       No         244 1      
# 10 4_hr       No          99 1      
# 11 More_4_hr  No         945 0.998  
# 12 More_4_hr  Yes          2 0.00211
# 13 NotAsked   No        1338 0.990  
# 14 NotAsked   Yes         13 0.00962
# 15 NA         No         287 1

# prop.table(table(change$comphrsday))
# 
# 0_hrs  0_to_1_hr       1_hr       2_hr       3_hr       4_hr  More_4_hr   NotAsked 
# 0.23074658 0.13782948 0.16080106 0.08172581 0.03592991 0.01457812 0.13944927 0.19893977 

#Debido a las cantidades tan peque?as en cada categoria, decidimos hacer las 
#siguientes reagrupaciones:
#1_hr,2_hr, 3_hr, 4_hr y More_4_hr
#La categoria NotAsked creada se mantiene intacta, al igual que 0_hrs y 0_to_1_hr
#Los NA se imputan

### alcohol12plusyr ->
#Se trata de una variable binaria en la que se expone si el participante ha 
#bebido al menos 12 copas en cualquier a?o. Tiene 100% de NA ya que ha sido 
#recogida para participantes de 18 a?os o m?s, por lo que la ELIMINAMOS

### smokenow ->
#Se trata de una variable binaria en la que se expone si el participante fuma 
#actualmente de manera regular. Tiene 100% de NA ya que ha sido recogida para 
#participantes de 20 a?os o m?s, por lo que la ELIMINAMOS

### smoke100 ->
#Se trata de una variable binaria en la que se expone si el participante ha 
#fumado mas de 100 cigarrillos en toda su vida. Tiene 100% de NA ya que ha sido 
#recogida para participantes de 20 a?os o m?s, por lo que la ELIMINAMOS

### marijuana ->
#Se trata de una variable binaria en la que se expone si el participante ha 
#probado la marihuana. Tiene 100% de NA ya que ha sido recogida para 
#participantes de entre 18 y 59 a?os, por lo que la ELIMINAMOS

### regularmarij ->
#Se trata de una variable binaria en la que se expone si el participante es o 
#no consumidor de marihuana. Tiene 100% de NA ya que ha sido recogida para 
#participantes de entre 18 y 59 a?os, por lo que la ELIMINAMOS

### harddrugs ->
#Se trata de una variable binaria en la que se expone si el participante ha
#probado coca?na, crack, hero?na o metanfetamina. Tiene 100% de NA ya que ha 
#sido recogida para participantes de entre 18 y 69 a?os, por lo que la ELIMINAMOS

### sexever ->
#Se trata de una variable binaria en la que se expone si el participante ha
#tenido sexo vaginal, anal u oral. Tiene 100% de NA ya que ha sido recogida para
#participantes de entre 18 y 69 a?os, por lo que la ELIMINAMOS

### samesex ->
#Se trata de una variable binaria en la que se expone si el participante ha
#tenido sexo con alguien de su mismo sexo. Tiene 100% de NA ya que ha sido 
#recogida para participantes de entre 18 y 69 a?os, por lo que la ELIMINAMOS

### sexorientation ->
#Esta variable refleja la orientaci?n sexual del participante. Tiene 100% de NA
#ya que ha sido recogida para participantes de entre 18 y 59 a?os, por lo que la
#ELIMINAMOS

#NUMERICAS

### id->
#ID del participante

### age ->
#Esta variable refleja la edad del participante. No tiene NA

### poverty ->
#Esta variable refleja un ratio entre el ingreso familiar y las pautas de 
#pobreza. Tiene menos de 10% de NA. Se imputan

# nhanesyoungraw %>% group_by(is.na(poverty)) %>%
#   count(is.na(poverty), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(poverty) [2]
# `is.na(poverty)` diabetes     n    porc
# <lgl>            <fct>    <int>   <dbl>
#   1 FALSE            No        6472 0.996  
# 2 FALSE            Yes         28 0.00431
# 3 TRUE             No         576 0.997  
# 4 TRUE             Yes          2 0.00346

### homerooms ->
#Esta variable refleja el numero de habitaciones que tiene la  casa donde vive
#el participante. Tiene menos de 1% de NA. Se imputan

# nhanesyoungraw %>% group_by(is.na(homerooms)) %>%
#   count(is.na(homerooms), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 3 x 4
# Groups:   is.na(homerooms) [2]
# `is.na(homerooms)` diabetes     n    porc
# <lgl>              <fct>    <int>   <dbl>
#   1 FALSE              No        7001 0.996  
# 2 FALSE              Yes         30 0.00427
# 3 TRUE               No          47 1  

### weight ->
#Esta variable refleja el peso del partipante. Tiene menos de 5% de NA

# nhanesyoungraw %>% group_by(is.na(weight)) %>%
#   count(is.na(weight), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 3 x 4
# Groups:   is.na(weight) [2]
# `is.na(weight)` diabetes     n    porc
# <lgl>           <fct>    <int>   <dbl>
#   1 FALSE           No        6742 0.996  
# 2 FALSE           Yes         30 0.00443
# 3 TRUE            No         306 1   

#Todos los participantes con NA en la variable weight no tienen diabetes, por lo
#que no se puede observar una relacion especial. Por ello, imputamos la variable
#weight

### length ->
#Esta variable refleja el largo del partipante para participantes de 3 o menos
#a?os. Se fusionar? con la variable height

### height ->
#Esta variable refleja la altura del partipante. Tiene menos de 10% de NA

# change <- nhanesyoungraw %>% mutate(height=ifelse(is.na(height), length, height)) 
# 
# change %>% group_by(is.na(height)) %>%
#   count(is.na(height), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 3 x 4
# Groups:   is.na(height) [2]
# `is.na(height)` diabetes     n    porc
# <lgl>           <fct>    <int>   <dbl>
#   1 FALSE           No        6726 0.996  
# 2 FALSE           Yes         30 0.00444
# 3 TRUE            No         322 1  

#Ninguna de los participantes con NA en la variable height tienen diabetes, por 
#lo que tener NA en esta variable no guarda una relacion con la variable 
#objetivo. Se imputan

### pulse ->
#Esta variable refleja la frecuencia del pulso del participante. Tiene m?s de un
#50% de NA

# nhanesyoungraw %>% filter(!is.na(pulse)) %>% count(diabetes)
# diabetes    n
# 1       No 3421
# 2      Yes   28
# nhanesyoungraw %>% filter(is.na(pulse)) %>% count(diabetes)
#   diabetes    n
# 1       No 3627
# 2      Yes    2

# nhanesyoungrawnodiabeticos <- nhanesyoungraw %>% filter(diabetes=="No")
# nhanesyoungrawdiabeticos <- nhanesyoungraw %>% filter(diabetes=="Yes")
# median(nhanesyoungrawnodiabeticos$pulse, na.rm=TRUE)
# [1] 78
# median(nhanesyoungrawdiabeticos$pulse, na.rm=TRUE)
# [1] 87
# mad(nhanesyoungrawdiabeticos$pulse, na.rm=TRUE)
# [1] 17.7912
# mad(nhanesyoungrawnodiabeticos$pulse, na.rm=TRUE)
# [1] 11.8608

# ggplot(data = nhanesyoungraw, aes(x = diabetes, y = pulse, fill=diabetes)) +
#   stat_boxplot(geom = "errorbar", Vwidth = 0.2) +
#   geom_boxplot(alpha = 0.9, outlier.colour = "#3D74CF") +
#   scale_fill_manual(values=c("#739EE5","#F4C46B"))+
#   scale_y_continuous(name = "Pulso") +
#   theme(text = element_text(family = "ban", size=20), plot.title = element_text(hjust = 0.35),
#         legend.position='none')

#A pesar de que 28 de los 30 diabeticos tengan valor en esta variable, las 
#diferencias entre estadisticos anteriores no se consideran suficientemente 
#relevantes para imputar m?s del 50% de los valores que requiere esta variable.
#Por ello, la eliminamos

### testosterone ->
#Esta variable refleja el nivel de testosterona total en ng/dL y ha sido 
#recogida ?nicamente en la encuesta del a?o 2011-2012. Tiene m?s de 75% de NA.
#Esto en parte se debe a que ha sido recogida unicamente en una de las encuestas

# nhanesyoungraw %>% filter(is.na(testosterone)) %>% count(diabetes)
# diabetes    n
# 1       No 5336
# 2      Yes   24
# nhanesyoungraw %>% filter(!is.na(testosterone)) %>% count(diabetes)
# diabetes    n
# 1       No 1712
# 2      Yes    6

# median(nhanesyoungrawnodiabeticos$testosterone, na.rm=TRUE)
# [1] 13.985
# median(nhanesyoungrawdiabeticos$testosterone, na.rm=TRUE)
# [1] 53.275
# mad(nhanesyoungrawdiabeticos$testosterone, na.rm=TRUE)
# [1] 44.85606
# mad(nhanesyoungrawnodiabeticos$testosterone, na.rm=TRUE)
# [1] 17.51692

#Unicamente se tiene valor en esta variable para 6 de los 30 diabeticos.
#A pesar de que existan diferencias notables entre estad?sticos, el hecho de 
#tener que imputar mas de un 75% de los valores que requiere esta variable hace 
#que se tome la decision de eliminarla

### directchol ->
#Esta variable refleja el nivel de colesterol HDL directo en mmol/L. Tiene 48% 
#de NA

# nhanesyoungraw %>% filter(is.na(directchol)) %>% count(diabetes)
# diabetes    n
# 1       No 3396
# 2      Yes    7
# nhanesyoungraw %>% filter(!is.na(directchol)) %>% count(diabetes)
# diabetes    n
# 1       No 3652
# 2      Yes   23

# median(nhanesyoungrawnodiabeticos$directchol, na.rm=TRUE)
# [1] 1.34
# median(nhanesyoungrawdiabeticos$directchol, na.rm=TRUE)
# [1] 1.22
# mad(nhanesyoungrawdiabeticos$directchol, na.rm=TRUE)
# [1] 0.355824
# mad(nhanesyoungrawnodiabeticos$directchol, na.rm=TRUE)
# [1] 0.29652

#No hay mucha diferencia entre los estadisticos de directchol para diabeticos y
#no diabeticos. A pesar de ello, imputamos los NA

### totchol ->
#Esta variable refleja el nivel de colesterol HDL total en mmol/L. Tiene 48% de 
#NA

# nhanesyoungraw %>% filter(is.na(totchol)) %>% count(diabetes)
# diabetes    n
# 1       No 3397
# 2      Yes    7
# nhanesyoungraw %>% filter(!is.na(totchol)) %>% count(diabetes)
# diabetes    n
# 1       No 3651
# 2      Yes   23

# median(nhanesyoungrawnodiabeticos$totchol, na.rm=TRUE)
# [1] 4.06
# median(nhanesyoungrawdiabeticos$totchol, na.rm=TRUE)
# [1] 4.5
# mad(nhanesyoungrawdiabeticos$totchol, na.rm=TRUE)
# [1] 0.993342
# mad(nhanesyoungrawnodiabeticos$totchol, na.rm=TRUE)
# [1] 0.652344

#No hay mucha diferencia entre los estadisticos de directchol para diabeticos y
#no diabeticos. A pesar de ello, imputamos los NA

### urinevol1 ->
#Esta variable refleja el volumen de orina de un participante en una lectura. 
#Tiene un 39% de NA

# nhanesyoungraw %>% filter(is.na(urinevol1)) %>% count(diabetes)
# diabetes    n
# 1       No 2738
# 2      Yes    3
# nhanesyoungraw %>% filter(!is.na(urinevol1)) %>% count(diabetes)
# diabetes    n
# 1       No 4310
# 2      Yes   27

# median(nhanesyoungrawnodiabeticos$urinevol1, na.rm=TRUE)
# [1] 81
# median(nhanesyoungrawdiabeticos$urinevol1, na.rm=TRUE)
# [1] 131
# mad(nhanesyoungrawdiabeticos$urinevol1, na.rm=TRUE)
# [1] 120.0906
# mad(nhanesyoungrawnodiabeticos$urinevol1, na.rm=TRUE)
# [1] 69.6822

#Se imputan los NA

### urineflow1 ->
#Esta variable refleja el flujo de orina de un participante en una lectura. 
#Tiene menos de un 48% de NA

# nhanesyoungraw %>% filter(is.na(urineflow1)) %>% count(diabetes)
# diabetes    n
# 1       No 3372
# 2      Yes    8
# nhanesyoungraw %>% filter(!is.na(urineflow1)) %>% count(diabetes)
# diabetes    n
# 1       No 3676
# 2      Yes   22

# median(nhanesyoungrawnodiabeticos$urineflow1, na.rm=TRUE)
# [1] 0.5125
# median(nhanesyoungrawdiabeticos$urineflow1, na.rm=TRUE)
# [1] 0.504
# mad(nhanesyoungrawdiabeticos$urineflow1, na.rm=TRUE)
# [1] 0.5915574
# mad(nhanesyoungrawnodiabeticos$urineflow1, na.rm=TRUE)
# [1] 0.3958542

#Se imputan los NA

### daysphyshlthbad ->
#Esta variable refleja el n?mero de d?as que el participante ha considerado
#que su salud fisica no fue buena en los ?ltimos 30 d?as. Tiene 60% de NA y ha
#sido recogida para participantes de 12 o m?s a?os. Debido a la cantidad de NA
#y la informaci?n que representa convertiremos entro variable a categ?rica
#d?ndole distintaas categor?as. Adem?s, imputaremos el rango de edad no 
#contemplado en una nueva categoria a la que llamaremos NotAsked

# nhanesyoungraw %>% filter(is.na(daysphyshlthbad)) %>% count(diabetes)
# diabetes    n
# 1       No 5227
# 2      Yes   13
# nhanesyoungraw %>% filter(!is.na(daysphyshlthbad)) %>% count(diabetes)
# diabetes    n
# 1       No 1821
# 2      Yes   17
# 
# nhanesyoungraw %>% filter(diabetes=="Yes") %>% count(daysphyshlthbad)
# daysphyshlthbad  n
# 1               0 12
# 2               1  1
# 3              10  1
# 4              20  1
# 5              24  1
# 6              30  1
# 7              NA 13

#Debido a la cantidad de NA de esta variable y su relaci?n con la variable
#objetivo, se elimina

### daysmenthlthbad ->
#Esta variable refleja el n?mero de d?as que el participante ha considerado
#que su salud mental no fue buena en los ?ltimos 30 d?as. Tiene 60% de NA y ha
#sido recogida para participantes de 12 o m?s a?os. Debido a la cantidad de NA
#y la informaci?n que representa convertiremos entro variable a categ?rica
#d?ndole distintaas categor?as. Adem?s, imputaremos el rango de edad no 
#contemplado en una nueva categoria a la que llamaremos NotAsked

# nhanesyoungraw %>% filter(is.na(daysmenthlthbad)) %>% count(diabetes)
# diabetes    n
# 1       No 5227
# 2      Yes   13
# nhanesyoungraw %>% filter(!is.na(daysmenthlthbad)) %>% count(diabetes)
# diabetes    n
# 1       No 1821
# 2      Yes   17
# 
# nhanesyoungraw %>% filter(diabetes=="Yes") %>% count(daysmenthlthbad)
# daysmenthlthbad  n
# 1               0  7
# 2               1  2
# 3               2  1
# 4               3  2
# 5               5  1
# 6              14  2
# 7              15  1
# 8              30  1
# 9              NA 13

#Debido a la cantidad de NA de esta variable y su relaci?n con la variable
#objetivo, se elimina

### npregnancies ->
#Esta variable refleja el numero de veces que la participante ha estado 
#embarazada y ha sido recogida para mujeres de 20 a?os o mas. Tiene un 100% de
#NA, por lo que la ELIMINAMOS

### nbabies ->
#Esta variable refleja el numero de veces que la participante ha tenido beb?s y 
#ha sido recogida para mujeres de 20 a?os o mas. Tiene un 100% de NA, por lo que
#la ELIMINAMOS

### age1stbaby ->
#Esta variable refleja la edad en la que la partipante ha sido madre por 1? vez 
#y ha sido recogida para mujeres de 20 a?os o mas. Tiene un 100% de NA, por lo 
#que la ELIMINAMOS

### sleephrsnight ->
#Esta variable refleja el n?mero de horas de sue?o que el participante tiene por
#la noche en d?as de trabajo o fin de semana. Tiene un 85% de NA y se ha 
#preguntado a participantes de 16 a?os o mas. Debido a la alta cantidad de NA,
#procedemos a eliminarla

### physactivedays ->
#Esta variable refleja el n?mero medio de d?as en una semana en que el 
#participante realiza ejercicio de intensidad moderada o vigorosa. Tiene un 
#59% de NA y se ha recogido para participantes de 12 o mas a?os. 
#Debido a la cantidad de NA y la informaci?n que representa 
#convertiremos esta variable a categ?rica d?ndole distintaas categor?as. 

# nhanesyoungraw %>% filter(is.na(physactivedays)) %>% count(diabetes)
# diabetes    n
# 1       No 5179
# 2      Yes   13
# nhanesyoungraw %>% filter(!is.na(physactivedays)) %>% count(diabetes)
# diabetes    n
# 1       No 1869
# 2      Yes   17
# 
# nhanesyoungraw %>% filter(diabetes=="Yes") %>% count(physactivedays)
# physactivedays  n
# 1              0  4
# 2              1  2
# 3              2  2
# 4              3  3
# 5              4  1
# 6              5  3
# 7              7  2
# 8             NA 13

#En este caso, al tener esta variable menos valores posibles que las variables 
#daysphyshlthbad y daysmenthlthbad, se considera convertirla a categorica e
#imputar los menores de 12 a?os como NotAsked
#Se categorizara de la siguiente manera
#0: None
#1, 2 o 3: Some
#4, 5, 6, 7: Several

### tvhrsdaychild -> 
#Esta variable se fusiona con tvhrsday (antes explicada)

### comphrsdaychild ->
#Esta variable se fusiona con comphrsday (antes explicada)

### alcoholday ->
#Esta variable refleja el n?mero medio de bebidas consumidas por el participante
#al d?a en aquellos d?as que consume bebidas alcoh?licas y ha sido recogida para 
#particpantes de 18 o m?s a?os. Tiene un 100% de NA, por lo que la ELIMINAMOS

### alcoholyear ->
#Esta variable refleja el n?mero de d?as del a?o pasado en los que el 
#participante consumi? bebidas alcoh?licas y ha sido recogida para 
#participantes de 18 o m?s a?os. Tiene un 100% de NA, por lo que la ELIMINAMOS

### smokeage -> 
#Esta variable refleja la edad en la que el participante comenz? a fumar de 
#manera regular y ha sido recogida para participantes de 20 o m?s a?os. Tiene 
#un 100% de NA, por lo que la ELIMINAMOS

### agefirstmarij -> 
#Esta variable refleja la edad en la que el participante prob? la marihuana por
#primera vez y ha sido recogida para participantes de entre 18 y 59 a?os. Tiene 
#un 100% de NA, por lo que la ELIMINAMOS

### ageregmarij ->
#Esta variable refleja la edad a la que el participante comenz? a consumir 
#marihuana de manera regular y ha sido recogida para participantes de entre 18 y
#59 a?os. Tiene un 100% de NA, por lo que la ELIMINAMOS

### sexage ->
#Esta variable refleja la edad en la que el participante tuvo sexo por primera
#vez y ha sido recogida para participantes de entre 18 y 69 a?os. Tiene un 100% 
#de NA, por lo que la ELIMINAMOS

### sexnumpartnlife ->
#Esta variable refleja el n?mero de parejas del sexo opuesto con las que el
#participante ha tenido alg?n tipo de relaci?n sexual en toda su vida y ha sido 
#recogida para participantes de entre 18 y 69 a?os. Tiene un 100% de NA, por lo 
#que la ELIMINAMOS

### sexnumpartyear ->
#Esta variable refleja el n?mero de parejas del sexo opuesto con las que el
#participante ha tenido alg?n tipo de relaci?n sexual en los ?ltimos 12 meses y 
#ha sido recogida para participantes de entre 18 y 69 a?os. Tiene un 100% de NA,
#por lo que la ELIMINAMOS

### bpsys ->
#Esta variable refleja la media de distintas mediciones de la presion arterial
#sistolica. Tiene 25% de NA

### bpdia ->
#Esta variable refleja la media de distintas mediciones de la presion arterial
#diastolica. Tiene 25% de NA

#----5.2.Depuracion nhanesyoung----
nhanesyoung <- nhanesyoungraw %>%
#Eliminamos variables con complete_rate=0. Ademas sleeptrouble y sleephrsnight
  #Creacion NotAsked u otras categorias en variables categoricas
  mutate(healthgen= case_when((age<12) ~ "NotAsked",
                               healthgen %in% c("Excellent","Vgood") ~ "Vgood",
                               healthgen %in% c("Fair","Poor","Good") ~ "NotVgood",
                               TRUE ~ as.character(healthgen)),
  #Eliminar datos erroneos tvhrsdaychild, comphrsdaychild (numericas), 
  #convertimos a categoricas
         tvhrsdaychild=ifelse(tvhrsdaychild >24,NA,tvhrsdaychild),
         comphrsdaychild=ifelse(comphrsdaychild >24,NA,comphrsdaychild),
         tvhrsdaychild=cut(tvhrsdaychild,
                           breaks=c(-Inf,0,1,2,3,4,Inf),
                           labels=c("0_hrs","1_hr","2_hr","3_hr","4_hr","More_4_hr")),
         comphrsdaychild=cut(comphrsdaychild,
                             breaks=c(-Inf,0,1,2,3,4,Inf),
                             labels=c("0_hrs","1_hr","2_hr","3_hr","4_hr","More_4_hr")),
  #Fusionar con tvhrsday y comphrsday
         tvhrsday= case_when(is.na(tvhrsday) ~ as.character(tvhrsdaychild),
                             TRUE ~ as.character(tvhrsday)),
         tvhrsday= case_when((surveyyr=="2009_10" & age>11) ~ "NotAsked",
                             (surveyyr=="2009_10" & age<2) ~ "NotAsked",
                              tvhrsday %in% c("0_hrs","0_to_1_hr","1_hr") ~ "0to1hr",
                              tvhrsday %in% c("2_hr","3_hr") ~ "2to3hr",
                              tvhrsday %in% c("4_hr","More_4_hr") ~ "from4hr",
                              TRUE ~ as.character(tvhrsday)),
         comphrsday= case_when(is.na(comphrsday) ~ as.character(comphrsdaychild),
                               TRUE ~ as.character(comphrsday)),
         comphrsday= case_when((surveyyr=="2009_10" & age>11) ~ "NotAsked",
                               (surveyyr=="2009_10" & age<2) ~ "NotAsked",
                                comphrsday %in% c("1_hr","2_hr","3_hr","4_hr","More_4_hr") ~ "from1hr",
                                TRUE ~ as.character(comphrsday)),
  #Fusionamos height y length
         height= ifelse(is.na(height),length, height),
  #Convertimos variables numericas a categoricas
         physactivedays=cut(physactivedays,
                            breaks=c(-Inf,0,3,Inf),
                            labels=c("None","Some","Several")),
  #Incluimos categorias NotAsked en estas nuevas variables categoricas
         physactivedays= case_when(age<12 ~ "NotAsked",
                                   TRUE ~ as.character(physactivedays)),
  #Outliers
         weight= ifelse(abs(weight - median(weight, na.rm=TRUE)) > 
                      6 * mad(weight, na.rm = TRUE), NA, weight),
         height= ifelse(abs(height - median(height, na.rm=TRUE)) > 
                      6 * mad(height, na.rm = TRUE), NA, height),
    # pulse= ifelse(abs(pulse - median(pulse, na.rm=TRUE)) > 
    #                   6 * mad(pulse, na.rm = TRUE), NA, pulse),
    # testosterone= ifelse(abs(testosterone - median(testosterone, na.rm=TRUE)) > 
    #                        6 * mad(testosterone, na.rm = TRUE), NA, testosterone),
         directchol= ifelse(abs(directchol - median(directchol, na.rm=TRUE)) > 
                           6 * mad(directchol, na.rm = TRUE), NA, directchol),
         totchol= ifelse(abs(totchol - mean(totchol, na.rm=TRUE)) > 
                         1.5 * sd(totchol, na.rm = TRUE), NA, totchol),
         urinevol1= ifelse(abs(urinevol1 - median(urinevol1, na.rm=TRUE)) > 
                      6 * mad(urinevol1, na.rm = TRUE), NA, urinevol1),
         urineflow1= ifelse(abs(urineflow1 - median(urineflow1, na.rm=TRUE)) > 
                      6 * mad(urineflow1, na.rm = TRUE), NA, urineflow1),
         bpsys= ifelse(abs(bpsys - mean(bpsys, na.rm=TRUE)) > 
                     1.5 * sd(bpsys, na.rm = TRUE), NA, bpsys),
         bpdia= ifelse(abs(bpdia - median(bpdia, na.rm=TRUE)) > 
                 6 * mad(bpdia, na.rm = TRUE), NA, bpdia)) %>%
  dplyr::select(
    #categoricas
    -education, -maritalstatus, -work, -littleinterest, -depressed, -sleeptrouble,
    -alcohol12plusyr, -smokenow, -smoke100, -marijuana, -regularmarij, -harddrugs, 
    -sexever, -samesex, -sexorientation,
    #numericas
    -pulse, -testosterone, -daysphyshlthbad, -daysmenthlthbad,
    -npregnancies, -nbabies, -age1stbaby, -sleephrsnight,
    -alcoholday, -alcoholyear, -smokeage, -agefirstmarij, -ageregmarij, -sexage, 
    -sexnumpartnlife, -sexnumpartyear,
    #reagrupaciones
    -tvhrsdaychild, -comphrsdaychild, -length)

#Variable numericas discretas (para imputar las pasamos a factor y luego
#deshacemos cambios)
nhanesyoung <- nhanesyoung %>% mutate(homerooms=as.factor(homerooms),
                                      #pulse=as.factor(pulse),
                                      #categoricas
                                      across(where(is.character),as.factor))

#Imputamos mediante ?rboles
imputacionyoung <- mice(data=nhanesyoung, m=1, method="cart")
nhanesyoungdep <- complete(imputacionyoung)

nhanesyoungdep <- nhanesyoungdep %>% mutate(homerooms=as.numeric(homerooms))
                                            #pulse=as.numeric(pulse))

#saveRDS(nhanesyoungdep, file = "nhanesyoungdep.rds")
nhanesyoungdep <- readRDS("nhanesyoungdep.rds")
write.csv(nhanesyoungdep, "nhanesyoungdep.csv")
#----6.1.Exploracion nhanesold----
#En este dataset tenemos participantes de 18 o mas a?os
nhanesoldraw %>% skim()
#CATEGORICAS

### surveyyr ->
#Refleja el a?o de la encuesta. No tiene NA

### gender ->
#Refleja el genero del participante. No tiene NA

### race1 ->
#Refleja la raza del partipante. No tiene NA

### education->
#Esta variable refleja el nivel educativo del participante. Tiene 5% de NA y ha 
#sido recogida para participantes de 20 a?os o mas, por lo que imputaremos este 
#rango de edad no considerado en una nueva categoria NotAsked. Veamos como queda
#la distribuci?n

# nhanesoldraw %>% mutate(education= case_when(age<20 ~ "NotAsked",
#                                              TRUE ~ as.character(education))) %>%
#   group_by(education) %>%
#   count(education, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 14 x 4
# Groups:   education [7]
# education      diabetes     n   porc
# <chr>          <fct>    <int>  <dbl>
#   1 8th Grade      No        1005 0.762 
# 2 8th Grade      Yes        314 0.238 
# 3 9 - 11th Grade No        1483 0.831 
# 4 9 - 11th Grade Yes        302 0.169 
# 5 College Grad   No        2390 0.901 
# 6 College Grad   Yes        264 0.0995
# 7 High School    No        2252 0.868 
# 8 High School    Yes        341 0.132 
# 9 NotAsked       No         605 0.987 
# 10 NotAsked       Yes          8 0.0131
# 11 Some College   No        2958 0.870 
# 12 Some College   Yes        441 0.130 
# 13 NA             No          13 0.684 
# 14 NA             Yes          6 0.316

# change <- nhanesoldraw %>% mutate(education=case_when(age<20 ~ "NotAsked",
#                                                       TRUE ~ as.character(education)))
# prop.table(table(change$education))
# 
# 8th Grade 9 - 11th Grade   College Grad    High School       NotAsked   Some College 
# 0.10668931     0.14438243     0.21467281     0.20973874     0.04958343     0.27493327 

#Dado que la categoria NotAsked presenta un valor cercano al 100% de no 
#diabeticos, a pesar de que su n?mero de observaciones no es tan grande como el
#resto de categor?as se mantiene. Veamos como queda la distribuci?n

### maritalstatus ->
#Esta variable refleja el estado civil del participante. Tiene 5% de NA y ha 
#sido recogida para participantes de 20 a?os o mas, por lo que imputaremos este 
#rango de edad no considerado en una nueva categoria NotAsked. Veamos como queda
#la distribuci?n

# nhanesoldraw %>% mutate(maritalstatus= case_when(age<20 ~ "NotAsked",
#                                              TRUE ~ as.character(maritalstatus))) %>%
#   group_by(maritalstatus) %>%
#   count(maritalstatus, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 16 x 4
# Groups:   maritalstatus [8]
# maritalstatus diabetes     n   porc
# <chr>         <fct>    <int>  <dbl>
#   1 Divorced      No        1011 0.809 
# 2 Divorced      Yes        238 0.191 
# 3 LivePartner   No         867 0.940 
# 4 LivePartner   Yes         55 0.0597
# 5 Married       No        5004 0.853 
# 6 Married       Yes        861 0.147 
# 7 NeverMarried  No        2121 0.928 
# 8 NeverMarried  Yes        164 0.0718
# 9 NotAsked      No         605 0.987 
# 10 NotAsked      Yes          8 0.0131
# 11 Separated     No         345 0.839 
# 12 Separated     Yes         66 0.161 
# 13 Widowed       No         747 0.728 
# 14 Widowed       Yes        279 0.272 
# 15 NA            No           6 0.545 
# 16 NA            Yes          5 0.455

# change <- nhanesoldraw %>% mutate(maritalstatus=case_when(age<20 ~ "NotAsked",
#                                                       TRUE ~ as.character(maritalstatus)))
# prop.table(table(change$maritalstatus))

# Divorced  LivePartner      Married NeverMarried     NotAsked    Separated      Widowed 
# 0.10096193   0.07452914   0.47409264   0.18470617   0.04955137   0.03322286   0.08293590 

#Teniendo en cuenta la cantidad de observaciones de cada categor?a y las 
#relaciones con la variable objetivo decidimos reagrupar las siguientes 
#categorias:
#Divorced y Separated, ya que ambos presentan estados civiles similares.
#LivePartner, NeverMarried y NotAsked, ya que los dos primeros representan 
#estados de solter?a y la tercera categor?a representa participantes de 18 o 19
#a?os, por lo que es probable que la mayor?a de participantes se correspondan 
#con un estado de solter?a antes que estar viudos, casados o habiendo finalizado
#un matrimonio.
#Se mantienen intactas las categorias Married y Widowed.
#Los NA se imputan

### homeown -> 
#Se trata de la tipologia de propiedad de la vivienda donde vive el participante.
#Tiene menos de 1% de NA. Veamos la distribucion

# nhanesoldraw %>% 
#   group_by(homeown) %>%
#   count(homeown, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   homeown [4]
# homeown diabetes     n   porc
# <fct>   <fct>    <int>  <dbl>
#   1 Own     No        6098 0.853 
# 2 Own     Yes       1054 0.147 
# 3 Rent    No        4223 0.880 
# 4 Rent    Yes        575 0.120 
# 5 Other   No         311 0.889 
# 6 Other   Yes         39 0.111 
# 7 NA      No          74 0.902 
# 8 NA      Yes          8 0.0976

# prop.table(table(change$homeown))

# Own       Rent      Other 
# 0.58146341 0.39008130 0.02845528 

#Teniendo en cuenta los porcentajes de observaciones en cada categor?a 
#decidimos reagrupar las categor?as Rent y Other.
#Los NA se imputan

### work ->
#Refleja la relaci?n con respecto al trabajo. Tiene menos de 1% de NA.

# nhanesoldraw %>%
#   group_by(work) %>%
#   count(work, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 7 x 4
# Groups:   work [4]
# work       diabetes     n   porc
# <fct>      <fct>    <int>  <dbl>
#   1 Looking    No         497 0.919 
# 2 Looking    Yes         44 0.0813
# 3 NotWorking No        4268 0.796 
# 4 NotWorking Yes       1093 0.204 
# 5 Working    No        5939 0.917 
# 6 Working    Yes        539 0.0832
# 7 NA         No           2 1     

# prop.table(table(change$work))
# 
# Looking NotWorking    Working 
# 0.04369952 0.43303716 0.52326333 

#Teniendo en cuenta los porcentajes de observaciones en cada categor?a 
#decidimos reagrupar las categor?as Looking y NotWorking
#Los NA se imputan

### healthgen ->
#Refleja una calificaci?n categ?rica del estado de salud autopercibido y cuenta 
#con 5 categor?as. Tiene 15% de NA. Esta variable ha sido preguntada a todos los
#participantes de la encuesta

# nhanesoldraw %>%
#   group_by(healthgen) %>%
#   count(healthgen, diabetes) %>% mutate(porc=n/sum(n))
#
# # A tibble: 12 x 4
# # Groups:   healthgen [6]
# healthgen diabetes     n   porc
# <fct>     <fct>    <int>  <dbl>
#   1 Excellent No         992 0.956
# 2 Excellent Yes         46 0.0443
# 3 Vgood     No        2643 0.935
# 4 Vgood     Yes        183 0.0648
# 5 Good      No        3676 0.866
# 6 Good      Yes        567 0.134
# 7 Fair      No        1548 0.744
# 8 Fair      Yes        534 0.256
# 9 Poor      No         248 0.600
# 10 Poor      Yes        165 0.400
# 11 NA        No        1599 0.898
# 12 NA        Yes        181 0.102

# prop.table(table(change$healthgen))
#
# Excellent      Vgood       Good       Fair       Poor
# 0.09790606 0.26655348 0.40020751 0.19637804 0.03895491

#Teniendo en cuenta los porcentajes de observaciones en cada categor?a y la
#relaci?n de cada una de estas con la variable objetivo decidimos reagrupar las
#categorias:
#Excellent y Vgood
#Fair y Poor

#Por otro lado, debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion, teniendo en cuenta la 
#relaci?n de los NA con la variable objetivo y la cantidad de estos, decidimos 
#imputarlos como una nueva categor?a "NA"

### littleinterest ->
#Refleja una calificaci?n categ?rica del n?mero de d?as que el participante 
#ha tenido inter?s en hacer cosas. Tiene un 15% de NA. Esta variable ha sido 
#preguntada a todos los participantes de la encuesta

# nhanesoldraw %>%
#   group_by(littleinterest) %>%
#   count(littleinterest, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   littleinterest [4]
# littleinterest diabetes     n  porc
# <fct>          <fct>    <int> <dbl>
#   1 None           No        6828 0.873
# 2 None           Yes        995 0.127
# 3 Several        No        1524 0.852
# 4 Several        Yes        265 0.148
# 5 Most           No         688 0.772
# 6 Most           Yes        203 0.228
# 7 NA             No        1666 0.887
# 8 NA             Yes        213 0.113

# prop.table(table(change$littleinterest))

# None   Several      Most 
# 0.7448348 0.1703323 0.0848329 

#Debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion, teniendo en cuenta la 
#relaci?n de los NA con la variable objetivo y la cantidad de estos, decidimos 
#imputarlos como una nueva categor?a "NA"

### depressed ->
#Refleja una calificaci?n categ?rica del n?mero de d?as que el participante 
#se ha sentido depresivo. Tiene un 15% de NA. 

# nhanesoldraw %>%
#   group_by(depressed) %>%
#   count(depressed, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   depressed [4]
# depressed diabetes     n  porc
# <fct>     <fct>    <int> <dbl>
#   1 None      No        6910 0.872
# 2 None      Yes       1014 0.128
# 3 Several   No        1505 0.849
# 4 Several   Yes        267 0.151
# 5 Most      No         628 0.772
# 6 Most      Yes        185 0.228
# 7 NA        No        1663 0.888
# 8 NA        Yes        210 0.112

# prop.table(table(change$depressed))
# 
# None    Several       Most 
# 0.75402036 0.16861738 0.07736226

#Debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

### sleeptrouble ->
#Se trata de una variable binaria en la que se expone si el participante le ha
#dicho a alg?n profesional de la salud si ha tenido problemas para dormir. 
#Tiene menos de 1% de NA

# nhanesoldraw %>%
#   group_by(sleeptrouble) %>%
#   count(sleeptrouble, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 5 x 4
# Groups:   sleeptrouble [3]
# sleeptrouble diabetes     n  porc
# <fct>        <fct>    <int> <dbl>
#   1 No           No        8376 0.885
# 2 No           Yes       1088 0.115
# 3 Yes          No        2326 0.798
# 4 Yes          Yes        588 0.202
# 5 NA           No           4 1   

# prop.table(table(change$sleeptrouble))
# 
# No       Yes 
# 0.7645823 0.2354177 

#Se imputan los NA

### tvhrsday ->
#Refleja una calificaci?n categ?rica del n?mero medio de horas diarias que el 
#participante ha visto la televisi?n durante los ?ltimos 30 d?as y cuenta con
#6 categor?as. Esta variable ha sido recogida para participantes de 2 o m?s 
#a?os en la encuesta 2011-2012, mientras que no ha sido recogida en la encuesta
#2009-2010, por lo que crearemos una categoria nueva NotAsked para aquellos 
#participantes que no hayan respondido a esta variable.

# nhanesoldraw %>% mutate(tvhrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
#                                             TRUE ~ as.character(tvhrsday))) %>%
#   group_by(tvhrsday) %>%
#   count(tvhrsday, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 18 x 4
# Groups:   tvhrsday [9]
# tvhrsday  diabetes     n   porc
# <chr>     <fct>    <int>  <dbl>
#   1 0_hrs     No         126 0.863 
# 2 0_hrs     Yes         20 0.137 
# 3 0_to_1_hr No         634 0.902 
# 4 0_to_1_hr Yes         69 0.0982
# 5 1_hr      No         848 0.923 
# 6 1_hr      Yes         71 0.0773
# 7 2_hr      No        1276 0.886 
# 8 2_hr      Yes        164 0.114 
# 9 3_hr      No         850 0.838 
# 10 3_hr      Yes        164 0.162 
# 11 4_hr      No         548 0.829 
# 12 4_hr      Yes        113 0.171 
# 13 More_4_hr No         747 0.771 
# 14 More_4_hr Yes        222 0.229 
# 15 NotAsked  No        5672 0.870 
# 16 NotAsked  Yes        851 0.130 
# 17 NA        No           5 0.714 
# 18 NA        Yes          2 0.286 

# change <- nhanesoldraw %>% mutate(tvhrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
#                                                        TRUE ~ as.character(tvhrsday)))
# prop.table(table(change$tvhrsday))

# 0_hrs  0_to_1_hr       1_hr       2_hr       3_hr       4_hr  More_4_hr   NotAsked 
# 0.01179798 0.05680808 0.07426263 0.11636364 0.08193939 0.05341414 0.07830303 0.52711111

#Debido a las cantidades tan peque?as en cada categoria, decidimos hacer las 
#siguientes reagrupaciones:
#0_hrs, 0_to_1_hr y 1_hr
#2_hr y 3_hr
#4_hr y More_4_hr
#La categoria NotAsked creada se mantiene intacta
#Los NA se imputan

### comphrsday ->
#Refleja una calificaci?n categ?rica del n?mero medio de horas diarias que el 
#participante ha usado el ordenador durante los ?ltimos 30 d?as y cuenta con
#6 categor?as. Esta variable ha sido recogida para participantes de 2 o m?s 
#a?os en la encuesta 2011-2012, mientras que no ha sido recogida en la encuesta
#2009-2010, por lo que crearemos una categoria nueva NotAsked para aquellos 
#participantes que no hayan respondido a esta variable

# nhanesoldraw %>% mutate(comphrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
#                                             TRUE ~ as.character(comphrsday))) %>%
#   group_by(comphrsday) %>%
#   count(comphrsday, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 18 x 4
# Groups:   comphrsday [9]
# comphrsday diabetes     n   porc
# <chr>      <fct>    <int>  <dbl>
#   1 0_hrs      No        1415 0.795 
# 2 0_hrs      Yes        365 0.205 
# 3 0_to_1_hr  No        1231 0.869 
# 4 0_to_1_hr  Yes        185 0.131 
# 5 1_hr       No         860 0.895 
# 6 1_hr       Yes        101 0.105 
# 7 2_hr       No         664 0.900 
# 8 2_hr       Yes         74 0.100 
# 9 3_hr       No         345 0.903 
# 10 3_hr       Yes         37 0.0969
# 11 4_hr       No         186 0.886 
# 12 4_hr       Yes         24 0.114 
# 13 More_4_hr  No         332 0.900 
# 14 More_4_hr  Yes         37 0.100 
# 15 NotAsked   No        5672 0.870 
# 16 NotAsked   Yes        851 0.130 
# 17 NA         No           1 0.333 
# 18 NA         Yes          2 0.667 

# change <- nhanesoldraw %>% mutate(comphrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
#                                                        TRUE ~ as.character(comphrsday)))
# prop.table(table(change$comphrsday))

# 0_hrs  0_to_1_hr       1_hr       2_hr       3_hr       4_hr  More_4_hr   NotAsked 
# 0.14379191 0.11438727 0.07763147 0.05961709 0.03085871 0.01696421 0.02980855 0.52694079 

#Debido a las cantidades tan peque?as en cada categoria, decidimos hacer las 
#siguientes reagrupaciones:
#1_hr,2_hr, 3_hr, 4_hr y More_4_hr
#La categoria NotAsked creada se mantiene intacta, al igual que 0_hrs y 0_to_1_hr
#Los NA se imputan

### alcohol12plusyr ->
#Se trata de una variable binaria en la que se expone si el participante ha 
#bebido al menos 12 copas en cualquier a?o. Tiene 17% de NA

# nhanesoldraw %>%
#   group_by(alcohol12plusyr) %>%
#   count(alcohol12plusyr, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 6 x 4
# Groups:   alcohol12plusyr [3]
# alcohol12plusyr diabetes     n   porc
# <fct>           <fct>    <int>  <dbl>
#   1 No              No        2297 0.815 
# 2 No              Yes        522 0.185 
# 3 Yes             No        6519 0.872 
# 4 Yes             Yes        960 0.128 
# 5 NA              No        1890 0.907 
# 6 NA              Yes        194 0.0931

# prop.table(table(change$alcohol12plusyr))
# 
# No       Yes 
# 0.2737425 0.7262575 

#Debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

### smokenow ->
#Se trata de una variable binaria en la que se expone si el participante fuma 
#actualmente de manera regular. Tiene 58% de NA y ha sido recogida para 
#participantes de 20 a?os o m?s y ademas han respondido Yes a la variable 
#Smoke100. Por lo que crearemos una categoria nueva NotAsked para aquellos 
#participantes que no hayan respondido a esta variable

# nhanesoldraw %>% mutate(smokenow= case_when(age<20 | smoke100=="No" ~ "NotAsked",
#                                             TRUE ~ as.character(smokenow))) %>%
#   group_by(smokenow) %>%
#   count(smokenow, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 7 x 4
# Groups:   smokenow [4]
# smokenow diabetes     n  porc
# <chr>    <fct>    <int> <dbl>
#   1 No       No        2200 0.793
# 2 No       Yes        575 0.207
# 3 NotAsked No        6318 0.884
# 4 NotAsked Yes        828 0.116
# 5 Yes      No        2180 0.889
# 6 Yes      Yes        273 0.111
# 7 NA       No           8 1 

# change <- nhanesoldraw %>% mutate(smokenow= case_when(age<20 | smoke100=="No" ~ "NotAsked",
#                                                       TRUE ~ as.character(smokenow)))
# prop.table(table(change$smokenow))

# No  NotAsked       Yes 
# 0.2242605 0.5775012 0.1982382 

#Los NA se imputan

### smoke100 ->
#Se trata de una variable binaria en la que se expone si el participante ha 
#fumado mas de 100 cigarrillos en toda su vida. Tiene 5% de NA y ha sido 
#recogida para participantes de 20 a?os o m?s, por lo que crearemos una 
#categoria nueva NotAsked para aquellos participantes que no hayan respondido a 
#esta variable

# nhanesoldraw %>% mutate(smoke100= case_when(age<20 ~ "NotAsked",
#                                             TRUE ~ as.character(smoke100))) %>%
#   group_by(smoke100) %>%
#   count(smoke100, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 7 x 4
# Groups:   smoke100 [4]
# smoke100 diabetes     n   porc
# <chr>    <fct>    <int>  <dbl>
#   1 No       No        5713 0.874 
# 2 No       Yes        820 0.126 
# 3 NotAsked No         605 0.987 
# 4 NotAsked Yes          8 0.0131
# 5 Yes      No        4382 0.838 
# 6 Yes      Yes        848 0.162 
# 7 NA       No           6 1  

# change <- nhanesoldraw %>% mutate(smoke100= case_when(age<20 ~ "NotAsked",
#                                                       TRUE ~ as.character(smoke100)))
# prop.table(table(change$smoke100))

# No   NotAsked        Yes 
# 0.52787654 0.04953135 0.42259211 

#A pesar de que la categoria NotAsked no llegue al 5% del total de las 
#observaciones, con el objetivo de no distorsionar las otras dos categor?as,
#se mantiene como categoria
#Los NA se imputan

### marijuana y regularmarij ->
#La variable marijuana se trata de una variable binaria en la que se expone si 
#el participante ha probado la marihuana. Tiene 43% de NA. Por otro lado, la 
#variable regularmarij se trata de una variable binaria en la que se expone si 
#el participante es o ha sido consumidor habitual de marihuana (consumido al 
#menos una vez al mes durante un a?o). Tiene 70% de NA. Ambas han sido recogidas
#para participantes de entre 18 y 59 a?os. 
#Dado que lo dos variables son objetias, vamos a proceder a fusionarlas creando 
#una nueva llamada marij en la que se contemplen las siguientes categorias:
# marijuana="No" & regularmarij="No": "Never"
# marijuana="Yes" & regularmarij="No": "HasTried"
# marijuana="Yes" & regularmarij="Yes": "MarijUser"
# marijuana="Yes" & regularmarij=NA: "Tried_NAUser"
# Age>59: "NotAsked"

# nhanesoldraw %>% mutate(marij= case_when(age>59 ~ "NotAsked",
#                                          marijuana=="No" ~ "Never",
#                                          marijuana=="Yes" & regularmarij=="No" ~ "HasTried",
#                                          marijuana=="Yes" & regularmarij=="Yes" ~ "MarijUser",
#                                          marijuana=="Yes" & is.na(regularmarij)==TRUE ~ "Tried_NAUser",
#                                          TRUE ~ as.character(regularmarij))) %>%
# group_by(marij) %>%
# count(marij, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 11 x 4
# Groups:   marij [6]
# marij        diabetes     n   porc
# <chr>        <fct>    <int>  <dbl>
#   1 HasTried     No        1766 0.934 
# 2 HasTried     Yes        124 0.0656
# 3 MarijUser    No        1693 0.930 
# 4 MarijUser    Yes        127 0.0698
# 5 Never        No        3067 0.915 
# 6 Never        Yes        285 0.0850
# 7 NotAsked     No        2825 0.732 
# 8 NotAsked     Yes       1035 0.268 
# 9 Tried_NAUser No           7 1     
# 10 NA           No        1348 0.928 
# 11 NA           Yes        105 0.0723

# change <- nhanesoldraw %>% mutate(marij= case_when(age>59 ~ "NotAsked",
#                                                    marijuana=="No" ~ "Never",
#                                                    marijuana=="Yes" & regularmarij=="No" ~ "HasTried",
#                                                    marijuana=="Yes" & regularmarij=="Yes" ~ "MarijUser",
#                                                    marijuana=="Yes" & is.na(regularmarij)==TRUE ~ "Tried_NAUser",
#                                                    TRUE ~ as.character(regularmarij)))
# prop.table(table(change$marij))

# HasTried    MarijUser        Never     NotAsked Tried_NAUser 
# 0.1729343947 0.1665294171 0.3067069265 0.3531887638 0.0006404978 

#Debido a la baja cantidad de observaciones en la categoria Tried_NAUser creada,
#decidimos reagrupar dicha categoria con "HasTried", dado que tiene un mayor 
#porcentaje de no diabeticos dentro de las categorias que agrupan a 
#participantes que han probado la marihuana.
#Por otro lado, debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

## harddrugs ->
#Se trata de una variable binaria en la que se expone si el participante ha
#probado coca?na, crack, hero?na o metanfetamina. Tiene 30% de NA y ha sido 
#recogida para participantes de entre 18 y 69 a?os, por lo que crearemos una 
#categoria nueva NotAsked para aquellos participantes que no hayan respondido a 
#esta variable

# nhanesoldraw %>% mutate(harddrugs= case_when(age>69 ~ "NotAsked",
#                                              TRUE ~ as.character(harddrugs))) %>%
#   group_by(harddrugs) %>%
#   count(harddrugs, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   harddrugs [4]
# harddrugs diabetes     n  porc
# <chr>     <fct>    <int> <dbl>
#   1 No        No        6396 0.888
# 2 No        Yes        807 0.112
# 3 NotAsked  No        1467 0.736
# 4 NotAsked  Yes        527 0.264
# 5 Yes       No        1279 0.893
# 6 Yes       Yes        154 0.107
# 7 NA        No        1564 0.893
# 8 NA        Yes        188 0.107

# change <- nhanesoldraw %>% mutate(harddrugs= case_when(age>69 ~ "NotAsked",
#                                                        TRUE ~ as.character(harddrugs)))
# prop.table(table(change$harddrugs))

# No  NotAsked       Yes 
# 0.6776105 0.1875823 0.1348071 

#Por otro lado, debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

### sexever ->
#Se trata de una variable binaria en la que se expone si el participante ha
#tenido sexo vaginal, anal u oral. Tiene 30% de NA ya que ha sido recogida para
#participantes de entre 18 y 69 a?os, por lo que crearemos una categoria nueva 
#NotAsked para aquellos participantes que no hayan respondido a esta variable

# nhanesoldraw %>% mutate(sexever= case_when(age>69 ~ "NotAsked",
#                                            TRUE ~ as.character(sexever))) %>%
#   group_by(sexever) %>%
#   count(sexever, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   sexever [4]
# sexever  diabetes     n  porc
# <chr>    <fct>    <int> <dbl>
#   1 No       No         420 0.892
# 2 No       Yes         51 0.108
# 3 NotAsked No        1467 0.736
# 4 NotAsked Yes        527 0.264
# 5 Yes      No        7248 0.888
# 6 Yes      Yes        914 0.112
# 7 NA       No        1571 0.895
# 8 NA       Yes        184 0.105

# change <- nhanesoldraw %>% mutate(sexever= case_when(age>69 ~ "NotAsked",
#                                                      TRUE ~ as.character(sexever)))
# prop.table(table(change$sexever))

# No   NotAsked        Yes 
# 0.04432107 0.18763527 0.76804366

#Por otro lado, debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

### samesex ->
#Se trata de una variable binaria en la que se expone si el participante ha
#tenido sexo con alguien de su mismo sexo. Tiene 30% de NA ya que ha sido 
#recogida para participantes de entre 18 y 69 a?os, por lo que crearemos una 
#categoria nueva NotAsked para aquellos participantes que no hayan respondido a 
#esta variable

# nhanesoldraw %>% mutate(samesex= case_when(age>69 ~ "NotAsked",
#                                            TRUE ~ as.character(samesex))) %>%
#   group_by(samesex) %>%
#   count(samesex, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   samesex [4]
# samesex  diabetes     n   porc
# <chr>    <fct>    <int>  <dbl>
#   1 No       No        7144 0.887 
# 2 No       Yes        908 0.113 
# 3 NotAsked No        1467 0.736 
# 4 NotAsked Yes        527 0.264 
# 5 Yes      No         526 0.908 
# 6 Yes      Yes         53 0.0915
# 7 NA       No        1569 0.893 
# 8 NA       Yes        188 0.107

# change <- nhanesoldraw %>% mutate(samesex= case_when(age>69 ~ "NotAsked",
#                                                      TRUE ~ as.character(samesex)))
# prop.table(table(change$samesex))

# No   NotAsked        Yes 
# 0.75783529 0.18767059 0.05449412 

#Por otro lado, debido a que desconocemos si hay alg?n tipo de informaci?n 
#detr?s de no haber contestado a esta variable, a pesar de que pueda tratarse de
#un simple error a la hora de recoger la informacion y teniendo en cuenta la 
#relaci?n de estos con la variable objetivo decidimos imputar los NA como
#una nueva categor?a "NA"

### sexorientation ->
#Esta variable refleja la orientaci?n sexual del participante. Tiene 45% de NA
#y ha sido recogida para participantes de entre 18 y 59 a?os, por lo que 
#crearemos una categoria nueva NotAsked para aquellos participantes que no hayan
#respondido aesta variable

# nhanesoldraw %>% mutate(sexorientation= case_when(age>59 ~ "NotAsked",
#                                            TRUE ~ as.character(sexorientation))) %>%
#   group_by(sexorientation) %>%
#   count(sexorientation, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   sexorientation [5]
# sexorientation diabetes     n   porc
# <chr>          <fct>    <int>  <dbl>
#   1 Bisexual       No         188 0.931 
# 2 Bisexual       Yes         14 0.0693
# 3 Heterosexual   No        6036 0.924 
# 4 Heterosexual   Yes        495 0.0758
# 5 Homosexual     No         107 0.964 
# 6 Homosexual     Yes          4 0.0360
# 7 NotAsked       No        2825 0.732 
# 8 NotAsked       Yes       1035 0.268 
# 9 NA             No        1550 0.924 
# 10 NA             Yes        128 0.0763

# change <- nhanesoldraw %>% mutate(sexorientation= case_when(age>59 ~ "NotAsked",
#                                                             TRUE ~ as.character(sexorientation)))
# prop.table(table(change$sexorientation))

# Bisexual Heterosexual   Homosexual     NotAsked 
# 0.01887145   0.61014574   0.01036996   0.36061286 

#Existe tan poca variabilidad en las categor?as de la variable que la eliminamos.
#El porcentaje de diab?ticos en NotAsked aumenta considerablemente pero esto
#ya se contempla en otras variables dado que en esta categor?a se recoge a los 
#participantes de 60 o m?s a?os. Por este motivo, eliminamos esta variable

#NUMERICAS

### id->
#ID del participante

### age ->
#Esta variable refleja la edad del participante. No tiene NA

### poverty ->
#Esta variable refleja un ratio entre el ingreso familiar y las pautas de 
#pobreza. Tiene menos de 10% de NA. Se imputan

# nhanesoldraw %>% group_by(is.na(poverty)) %>%
#   count(is.na(poverty), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(poverty) [2]
# `is.na(poverty)` diabetes     n  porc
# <lgl>            <fct>    <int> <dbl>
#   1 FALSE            No        9704 0.867
# 2 FALSE            Yes       1492 0.133
# 3 TRUE             No        1002 0.845
# 4 TRUE             Yes        184 0.155

### homerooms ->
#Esta variable refleja el numero de habitaciones que tiene la casa donde vive
#el participante. Tiene menos de 1% de NA. Se imputan

# nhanesoldraw %>% group_by(is.na(homerooms)) %>%
#   count(is.na(homerooms), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(homerooms) [2]
# `is.na(homerooms)` diabetes     n  porc
# <lgl>              <fct>    <int> <dbl>
#   1 FALSE              No       10622 0.864
# 2 FALSE              Yes       1666 0.136
# 3 TRUE               No          84 0.894
# 4 TRUE               Yes         10 0.106

### weight ->
#Esta variable refleja el peso del partipante. Tiene menos de 5% de NA. Se 
#imputan

# nhanesoldraw %>% group_by(is.na(weight)) %>%
#   count(is.na(weight), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(weight) [2]
# `is.na(weight)` diabetes     n  porc
# <lgl>           <fct>    <int> <dbl>
#   1 FALSE           No       10230 0.865
# 2 FALSE           Yes       1595 0.135
# 3 TRUE            No         476 0.855
# 4 TRUE            Yes         81 0.145

### length ->
#Esta variable refleja el largo del partipante para participantes de 3 o menos
#a?os. Tiene un 100% de NA, por lo que la ELIMINAMOS

### height ->
#Esta variable refleja la altura del partipante. Tiene menos de 5% de NA. Se 
#imputan

# nhanesoldraw %>% group_by(is.na(height)) %>% count(is.na(height), diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(height) [2]
# `is.na(height)` diabetes     n  porc
# <lgl>           <fct>    <int> <dbl>
#   1 FALSE           No       10226 0.865
# 2 FALSE           Yes       1597 0.135
# 3 TRUE            No         480 0.859
# 4 TRUE            Yes         79 0.141

### pulse ->
#Esta variable refleja la frecuencia del pulso del partipante. Tiene un 8% de 
#NA. Se imputan

# nhanesoldraw %>% group_by(is.na(pulse)) %>% count(is.na(pulse), diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(pulse) [2]
# `is.na(pulse)` diabetes     n  porc
# <lgl>          <fct>    <int> <dbl>
#   1 FALSE          No        9859 0.862
# 2 FALSE          Yes       1581 0.138
# 3 TRUE           No         847 0.899
# 4 TRUE           Yes         95 0.101

### testosterone ->
#Esta variable refleja el nivel de testosterona total en ng/dL y ha sido 
#recogida ?nicamente en la encuesta del a?o 2011-2012. Tiene un 59% de NA.
#Esto se debe a haber sido recogida ?nicamente en una de las encuestas

# ggplot(data = nhanesoldraw, aes(x = diabetes, y = testosterone, fill=diabetes)) +
#   stat_boxplot(geom = "errorbar", Vwidth = 0.2) +
#   geom_boxplot(alpha = 0.9, outlier.colour = "#3D74CF") +
#   scale_fill_manual(values=c("#739EE5","#F4C46B"))+
#   scale_y_continuous(name = "Testosterona") +
#   theme(text = element_text(family = "ban", size=20), plot.title = element_text(hjust = 0.35),
#         legend.position='none')

# ggplot(data = nhanesoldraw, aes(x = diabetes, y = testosterone, fill=diabetes)) +
#   stat_boxplot(geom = "errorbar", Vwidth = 0.2) +
#   geom_boxplot(alpha = 0.9, outlier.colour = "#3D74CF") +
#   scale_fill_manual(values=c("#739EE5","#F4C46B"))+
#   scale_y_continuous(limits = c(0, 500), name = "Testosterona") +
#   theme(text = element_text(family = "ban", size=20), plot.title = element_text(hjust = 0.35),
#         legend.position='none')

# nhanesoldrawnodiabeticos <- nhanesoldraw %>% filter(diabetes=="No")
# nhanesoldrawdiabeticos <- nhanesoldraw %>% filter(diabetes=="Yes")
# median(nhanesoldrawnodiabeticos$testosterone, na.rm=TRUE)
# [1] 76.38
# median(nhanesoldrawdiabeticos$testosterone, na.rm=TRUE)
# [1] 78.26
# mad(nhanesoldrawdiabeticos$testosterone, na.rm=TRUE)
# [1] 107.2735
# mad(nhanesoldrawnodiabeticos$testosterone, na.rm=TRUE)
# [1] 103.7079

#Debido a la cantidad de NA que hay en esta variable y la relacion que tiene con
#la variable objetivo, decidimos eliminar esta variable

### directchol ->
#Esta variable refleja el nivel de colesterol HDL directo en mmol/L. Tiene menos 
#de 10% de NA. Se imputan

### totchol ->
#Esta variable refleja el nivel de colesterol HDL total en mmol/L. Tiene menos 
#de 10% de NA. Se imputan

### urinevol1 ->
#Esta variable refleja el volumen de orina de un participante en una lectura. 
#Tiene un 5% de NA. Se imputan

### urineflow1 ->
#Esta variable refleja el flujo de orina de un participante en una lectura. 
#Tiene menos de un 11% de NA. Se imputan

### daysphyshlthbad ->
#Esta variable refleja el n?mero de d?as que el participante ha considerado
#que su salud fisica no fue buena en los ?ltimos 30 d?as. Tiene 15% de NA.
#En el resumen skim se puede ver que p0=p25=p50=0, p75=3 y p100=30. Por ello y
#debido a la informaci?n que representa la convertiremos a categ?rica agrupando 
#los valores en distintas categor?as

# nhanesoldraw %>% mutate(daysphyshlthbad=cut(daysphyshlthbad,
#                                         breaks=c(-Inf,0,4,10,Inf),
#                                         labels=c("None","Few","Several","Many"))) %>%
#        group_by(daysphyshlthbad) %>%
#   count(daysphyshlthbad, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   daysphyshlthbad [5]
# daysphyshlthbad diabetes     n  porc
# <fct>           <fct>    <int> <dbl>
#   1 None            No        5945 0.880
# 2 None            Yes        807 0.120
# 3 Few             No        1340 0.883
# 4 Few             Yes        178 0.117
# 5 Several         No         815 0.845
# 6 Several         Yes        149 0.155
# 7 Many            No         996 0.736
# 8 Many            Yes        357 0.264
# 9 NA              No        1610 0.897
# 10 NA              Yes        185 0.103

#Por otro lado, al tratarse de una variable subjetiva, viendo la relaci?n de los
#NA con la variable objetivo y teniendo en cuenta que el hecho de no haber 
#respondido puede significar algo, imputaremos los NA como una categoria m?s 
#("NA")

### daysmenthlthbad ->
#Esta variable refleja el n?mero de d?as que el participante ha considerado
#que su salud mental no fue buena en los ?ltimos 30 d?as. Tiene 15% de NA.
#En el resumen skim se puede ver que p0=p25=p50=0, p75=3 y p100=30. Por ello y
#debido a la informaci?n que representa la convertiremos a categ?rica agrupando 
#los valores en distintas categor?as

# nhanesoldraw %>% mutate(daysmenthlthbad=cut(daysmenthlthbad,
#                                         breaks=c(-Inf,0,4,10,Inf),
#                                         labels=c("None","Few","Several","Many"))) %>%
#   group_by(daysmenthlthbad) %>%
# count(daysmenthlthbad, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   daysmenthlthbad [5]
# daysmenthlthbad diabetes     n  porc
# <fct>           <fct>    <int> <dbl>
#   1 None            No        5264 0.858
# 2 None            Yes        874 0.142
# 3 Few             No        1628 0.876
# 4 Few             Yes        230 0.124
# 5 Several         No        1013 0.873
# 6 Several         Yes        148 0.127
# 7 Many            No        1186 0.833
# 8 Many            Yes        238 0.167
# 9 NA              No        1615 0.897
# 10 NA              Yes        186 0.103

#Por otro lado, al tratarse de una variable subjetiva, viendo la relaci?n de los
#NA con la variable objetivo y teniendo en cuenta que el hecho de no haber 
#respondido puede significar algo, imputaremos los NA como una categoria m?s 
#("NA")

#Miraremos la correlacion entre daysphyshlthbad y daysmenthlthbad

# nhanesoldraw %>% count(is.na(daysphyshlthbad) & is.na(daysmenthlthbad))
# 
# dias_malos <-  nhanesoldraw %>% select(daysphyshlthbad, daysmenthlthbad)
# dias_malos <- dias_malos %>% drop_na()
# 
# correlacion<-round(cor(dias_malos), 1)
# corrplot(correlacion, method="number", type="upper")

#No estan correlacionadas (correlacion=0.3)

### npregnancies ->
#Esta variable refleja el numero de veces que la participante ha estado 
#embarazada. Tiene 66% de NA y ha sido recogida para mujeres de 20 a?os o mas. 
#Debido a la informaci?n que representa la convertiremos a categ?rica agrupando 
#los valores en distintas categor?as. Adem?s, incluiremos una categor?a
#especifica para los hombres los cuales ser?n representados por la categoria
#"NotApply" y las edades de mujeres que no correponden seran representadas por
#"NotAsked". Veamos la relaci?n de los NA con la variable objetivo

# nhanesoldraw %>% mutate(npregnancies=cut(npregnancies,
#                         breaks=c(-Inf,0,2,Inf),
#                         labels=c("None","1or2","MoreThan2")),
#                         npregnancies= case_when(gender=='male' ~ "NotApply",
#                         gender=='female' & age<20 ~ "NotAsked",
#                         TRUE ~ as.character(npregnancies))) %>%
# group_by(npregnancies) %>% count(npregnancies, diabetes) %>%
# mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   npregnancies [5]
# npregnancies diabetes     n   porc
# <chr>        <fct>    <int>  <dbl>
#   1 1or2         No        1421 0.885 
# 2 1or2         Yes        185 0.115 
# 3 MoreThan2    No        2112 0.814 
# 4 MoreThan2    Yes        482 0.186 
# 5 NotApply     No        5219 0.860 
# 6 NotApply     Yes        849 0.140 
# 7 NotAsked     No         282 0.983 
# 8 NotAsked     Yes          5 0.0174
# 9 NA           No        1672 0.915 
# 10 NA           Yes        155 0.0848

#En esta variable, los NA podrian significar algo dado que las personas a las 
#cuales se les ha preguntado por esta variable y de las cuales no tenemos 
#informacion tienen una relacion menor con el hecho de padecer diabetes. 
#Por ello, en esta variable imputaremos los NA como una categoria mas ("NA")

#Por otro lado, dadas las pocas observaciones que hay en NotAsked, se considera
#imputarlas en la categor?a NA

### nbabies ->
#Esta variable refleja el numero de veces que la participante ha tenido beb?s.
#Tiene 68% de NA y ha sido recogida para mujeres de 20 a?os o mas. Debido a la 
#informaci?n que representa la convertiremos en categ?rica agrupando 
#los valores en distintas categor?as. Adem?s incluiremos una categor?a
#especifica para los hombres los cuales ser?n representados por la categoria
#"No procede" y las edades de mujeres que no correponden seran representadas por
#"NotAsked"

# nhanesoldraw %>% mutate(nbabies= cut(nbabies,
#                         breaks=c(-Inf,0,2,Inf),
#                         labels=c("None","1or2","MoreThan2")),
#                         nbabies= case_when(gender=='male' ~ "NotApply",
#                         gender=='female' & age<20 ~ "NotAsked",
#                         TRUE ~ as.character(nbabies))) %>%
# group_by(nbabies) %>% count(nbabies, diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 12 x 4
# Groups:   nbabies [6]
# nbabies   diabetes     n   porc
# <chr>     <fct>    <int>  <dbl>
#   1 1or2      No        1813 0.878 
# 2 1or2      Yes        252 0.122 
# 3 MoreThan2 No        1466 0.790 
# 4 MoreThan2 Yes        389 0.210 
# 5 None      No          14 0.824 
# 6 None      Yes          3 0.176 
# 7 NotApply  No        5219 0.860 
# 8 NotApply  Yes        849 0.140 
# 9 NotAsked  No         282 0.983 
# 10 NotAsked  Yes          5 0.0174
# 11 NA        No        1912 0.915 
# 12 NA        Yes        178 0.0852

#En esta variable, los NA podrian significar algo dado que las personas a las 
#cuales se les ha preguntado por esta variable y de las cuales no tenemos 
#informacion tienen una relacion menor con el hecho de padecer diabetes. 
#Por ello, en esta variable imputaremos los NA como una categoria mas ("NA")

#Por otro lado, dadas las pocas observaciones que hay en NotAsked, se considera
#imputarlas en la categor?a NA

### age1stbaby ->
#Esta variable refleja la edad en la que la participante ha sido madre por 1? 
#vez y ha sido recogida para mujeres de 20 a?os o mas. Tiene un 74% de NA. 
#Debido a la informaci?n que representa la convertiremos en categ?rica agrupando 
#los valores en distintas categor?as. Adem?s incluiremos una categor?a
#especifica para los hombres los cuales ser?n representados por la categoria
#"No procede" y las edades de mujeres que no correponden seran representadas por
#"NotAsked"

# nhanesoldraw %>% mutate(age1stbaby= cut(age1stbaby,
#                         breaks=c(-Inf,20,Inf),
#                         labels=c("20orLess","21orMore")),
#                         age1stbaby= case_when(gender=='male' ~ "NotApply",
#                         gender=='female' & age<20 ~ "NotAsked",
#                         TRUE ~ as.character(age1stbaby))) %>%
# group_by(age1stbaby) %>% count(age1stbaby, diabetes) %>%
#     mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   age1stbaby [5]
# age1stbaby diabetes     n   porc
# <chr>      <fct>    <int>  <dbl>
#   1 20orLess   No        1195 0.810 
# 2 20orLess   Yes        281 0.190 
# 3 21orMore   No        1417 0.843 
# 4 21orMore   Yes        263 0.157 
# 5 NotApply   No        5219 0.860 
# 6 NotApply   Yes        849 0.140 
# 7 NotAsked   No         282 0.983 
# 8 NotAsked   Yes          5 0.0174
# 9 NA         No        2593 0.903 
# 10 NA         Yes        278 0.0968

#En esta variable, los NA podrian significar algo dado que las personas a las 
#cuales se les ha preguntado por esta variable y de las cuales no tenemos 
#informacion tienen una relacion menor con el hecho de padecer diabetes. 
#Por ello, en esta variable imputaremos los NA como una categoria mas ("NA")

#Por otro lado, dadas las pocas observaciones que hay en NotAsked, se considera
#imputarlas en la categor?a NA

### sleephrsnight ->
#Esta variable refleja el n?mero de horas de sue?o que el participante tiene por
#la noche en d?as de trabajo o fin de semana. Tiene menos de un 1% de NA. Se 
#imputan

### physactivedays ->
#Esta variable refleja el n?mero medio de d?as en una semana en que el 
#participante realiza ejercicio de intensidad moderada o vigorosa. Tiene menos 
#de un 1% de NA. En el resumen skim se puede ver que p0=p25=p50=0, p75=3 y 
#p100=7. Debido a la cantidad tan baja de NA decidimos mantenerla como num?rica
#e imputar los valores NA

# nhanesoldraw %>% count(physactivedays)

# A tibble: 9 x 2
# physactivedays     n
# <dbl> <int>
#   1              0  6505
# 2              1   720
# 3              2  1066
# 4              3  1455
# 5              4   730
# 6              5   857
# 7              6   257
# 8              7   789
# 9             NA     3

### tvhrsdaychild -> 
#Esta variable tiene 100% de NA en este dataset por lo que la eliminamos

### comphrsdaychild ->
#Esta variable tiene 100% de NA en este dataset por lo que la eliminamos

### alcoholday ->
#Esta variable refleja el n?mero medio de bebidas consumidas por el participante
#al d?a en aquellos d?as que consume bebidas alcoh?licas. Tiene un 45% de NA.
#En el resumen skim se puede ver que p0=p25=1, p50=2, p75=4 y p100=82. Por ello 
#y debido a la informaci?n que representa la convertiremos a categ?rica 
#agrupando los valores en distintas categor?as

# nhanesoldraw %>% mutate(alcoholday=cut(alcoholday,
#                         breaks=c(-Inf,1,3,Inf),
#                         labels=c("1","2or3","MoreThan3"))) %>%
#  group_by(alcoholday) %>% count(alcoholday, diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   alcoholday [4]
# alcoholday diabetes     n   porc
# <fct>      <fct>    <int>  <dbl>
#   1 1          No        2039 0.861 
# 2 1          Yes        329 0.139 
# 3 2or3       No        2583 0.903 
# 4 2or3       Yes        277 0.0969
# 5 MoreThan3  No        1610 0.914 
# 6 MoreThan3  Yes        151 0.0857
# 7 NA         No        4474 0.830 
# 8 NA         Yes        919 0.170 

#En esta variable, los NA podrian significar algo dado que las personas a las 
#cuales se les ha preguntado por esta variable y no tenemos informacion tienen
#una relacion mayor con el hecho de padecer diabetes. Por ello, convertiremos 
#esta variable en categorica e imputaremos los NA como una categoria mas ("NA")

### alcoholyear ->
#Esta variable refleja el n?mero de d?as del a?o pasado en los que el 
#participante consumi? bebidas alcoh?licas. Tiene un 29% de NA

# nhanesoldraw %>% group_by(is.na(alcoholyear)) %>%
#   count(is.na(alcoholyear), diabetes) %>% mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(alcoholyear) [2]
# `is.na(alcoholyear)` diabetes     n  porc
# <lgl>                <fct>    <int> <dbl>
#   1 FALSE                No        7604 0.861
# 2 FALSE                Yes       1223 0.139
# 3 TRUE                 No        3102 0.873
# 4 TRUE                 Yes        453 0.127

#Los NA en esta variable no parece a priori que tengan una relaci?n especial con
#el hecho de padecer diabetes. Por ello, se decide mantener esta variable como
#num?rica y se imputan los NA

### smokeage -> 
#Esta variable refleja la edad en la que el participante comenz? a fumar de 
#manera regular y ha sido recogida para participantes de 20 o m?s a?os. Tiene 
#un 59% de NA. Debido a la informacion que representa convertiremos esta 
#variable a categ?rica d?ndole distintas categor?as. Adem?s, imputaremos el 
#rango de edad no contemplado en una nueva categoria a la que llamaremos 
#NotAsked y las personas que no han respondido no a Smoke100 como "No procede"

# nhanesoldraw %>% mutate(smokeage=cut(smokeage,
#                         breaks=c(-Inf,17,Inf),
#                         labels=c("Antes de los 18","18 o m?s")),
#                         smokeage= case_when(age<20 ~ "NotAsked",
#                         smoke100=="No" ~ "No procede",
#                         TRUE ~ as.character(smokeage))) %>%
# group_by(smokeage) %>% count(smokeage, diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   smokeage [5]
# smokeage        diabetes     n   porc
# <chr>           <fct>    <int>  <dbl>
#   1 18 o m?s        No        2000 0.832 
# 2 18 o m?s        Yes        403 0.168 
# 3 Antes de los 18 No        2216 0.839 
# 4 Antes de los 18 Yes        425 0.161 
# 5 No procede      No        5713 0.874 
# 6 No procede      Yes        820 0.126 
# 7 NotAsked        No         605 0.987 
# 8 NotAsked        Yes          8 0.0131
# 9 NA              No         172 0.896 
# 10 NA              Yes         20 0.104 

#Debido a la baja cantidad de NA tras la imputacion por observaciones que no
#proceden y la baja cantidad de observaciones en NotAsked, se fusionan ambas 
#categorias en una nueva categoria NA

### agefirstmarij -> 
#Esta variable refleja la edad en la que el participante prob? la marihuana por
#primera vez y ha sido recogida para participantes de entre 18 y 59 a?os. Tiene 
#un 70% de NA. Debido a la informacion que representa convertiremos esta 
#variable a categ?rica d?ndole distintas categor?as. Adem?s, imputaremos el 
#rango de edad no contemplado en una nueva categoria a la que llamaremos 
#NotAsked y las personas que nunca han probado la marihuana como "NotApply"

# nhanesoldraw %>% mutate(marij= case_when(age>59 ~ "NotAsked",
#                                          marijuana=="No" ~ "Never",
#                                          marijuana=="Yes" & regularmarij=="No" ~ "HasTried",
#                                          marijuana=="Yes" & regularmarij=="Yes" ~ "MarijUser",
#                                          marijuana=="Yes" & is.na(regularmarij)==TRUE ~ "HasTried",
#                                          is.na(marijuana)==TRUE ~ "NA",
#                                          TRUE ~ as.character(NA)),
#                         agefirstmarij=cut(agefirstmarij,
#                                       breaks=c(-Inf,17,Inf),
#                                       labels=c("Before18","18orMore")),
#                         agefirstmarij=case_when(age>59 ~ "NotAsked",
#                                       marij=="Never" ~ "NotApply",
#                                       TRUE ~ as.character(agefirstmarij))) %>%
# group_by(agefirstmarij) %>% count(agefirstmarij, diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   agefirstmarij [5]
# agefirstmarij diabetes     n   porc
# <chr>         <fct>    <int>  <dbl>
#   1 18orMore      No        1305 0.929 
# 2 18orMore      Yes         99 0.0705
# 3 Before18      No        2156 0.934 
# 4 Before18      Yes        152 0.0659
# 5 NotApply      No        3067 0.915 
# 6 NotApply      Yes        285 0.0850
# 7 NotAsked      No        2825 0.732 
# 8 NotAsked      Yes       1035 0.268 
# 9 NA            No        1353 0.928 
# 10 NA            Yes        105 0.0720

#En esta variable, los NA podrian significar algo. Por ello, imputaremos los NA 
#como una categoria mas ("NA")

### ageregmarij ->
#Esta variable refleja la edad a la que el participante comenz? a consumir 
#marihuana de manera regular y ha sido recogida para participantes de entre 18 y
#59 a?os. Tiene un 85% de NA. Debido a la informacion que representa convertiremos 
#esta variable a categ?rica d?ndole distintas categor?as. Adem?s, imputaremos el 
#rango de edad no contemplado en una nueva categoria a la que llamaremos 
#NotAsked y las personas que no son consumidores (Marij="Nunca" o "Ha probado) 
#como "No procede"

# nhanesoldraw %>%  mutate(marij= case_when(age>59 ~ "NotAsked",
#                                           marijuana=="No" ~ "Never",
#                                           marijuana=="Yes" & regularmarij=="No" ~ "HasTried",
#                                           marijuana=="Yes" & regularmarij=="Yes" ~ "MarijUser",
#                                           marijuana=="Yes" & is.na(regularmarij)==TRUE ~ "HasTried",
#                                           is.na(marijuana)==TRUE ~ "NA",
#                                           TRUE ~ as.character(NA)),
#                          ageregmarij=cut(ageregmarij,
#                                          breaks=c(-Inf,17,Inf),
#                                          labels=c("Antes de los 18","18 o m?s")),
#                          ageregmarij= case_when(age>59 ~ "NotAsked",
#                                                 marij=="Never" | marij=="HasTried" ~ "NotApply",
#                                                 TRUE ~ as.character(ageregmarij))) %>%
#   group_by(ageregmarij) %>% count(ageregmarij, diabetes) %>% 
#   mutate(porc=n/sum(n))

# A tibble: 10 x 4
# Groups:   ageregmarij [5]
# ageregmarij     diabetes     n   porc
# <chr>           <fct>    <int>  <dbl>
#   1 18 o m?s        No         712 0.915 
# 2 18 o m?s        Yes         66 0.0848
# 3 Antes de los 18 No         981 0.941 
# 4 Antes de los 18 Yes         61 0.0585
# 5 NotApply        No        4840 0.922 
# 6 NotApply        Yes        409 0.0779
# 7 NotAsked        No        2825 0.732 
# 8 NotAsked        Yes       1035 0.268 
# 9 NA              No        1348 0.928 
# 10 NA              Yes        105 0.0723

#En esta variable, los NA podrian significar algo. Por ello, imputaremos los NA 
#como una categoria mas ("NA")

### sexage ->
#Esta variable refleja la edad en la que el participante tuvo sexo por primera
#vez y ha sido recogida para participantes de entre 18 y 69 a?os. Tiene un 34% 
#de NA

# nhanesoldraw %>%
# group_by(is.na(sexage)) %>% count(is.na(sexage), diabetes) %>%
# mutate(porc=n/sum(n))

# A tibble: 4 x 4
# Groups:   is.na(sexage) [2]
# `is.na(sexage)` diabetes     n  porc
# <lgl>           <fct>    <int> <dbl>
#   1 FALSE           No        7227 0.889
# 2 FALSE           Yes        904 0.111
# 3 TRUE            No        3479 0.818
# 4 TRUE            Yes        772 0.182

#Los NA en esta variable parece que puedan tener una relaci?n especial con
#el hecho de padecer diabetes. Por ello, se decide pasar esta variable a 
#categ?rica e imputaremos los NA como una categoria m?s ("NA")

# nhanesoldraw %>% mutate(sexage=cut(sexage,
#                                breaks=c(-Inf,15,18,Inf),
#                                labels=c("15orLess","16to18","18orMore"))) %>%
#   group_by(sexage) %>% count(sexage, diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 8 x 4
# Groups:   sexage [4]
# sexage   diabetes     n  porc
# <fct>    <fct>    <int> <dbl>
#   1 15orLess No        2115 0.887
# 2 15orLess Yes        270 0.113
# 3 16to18   No        3168 0.894
# 4 16to18   Yes        374 0.106
# 5 18orMore No        1944 0.882
# 6 18orMore Yes        260 0.118
# 7 NA       No        3479 0.818
# 8 NA       Yes        772 0.182

### sexnumpartnlife ->
#Esta variable refleja el n?mero de parejas del sexo opuesto con las que el
#participante ha tenido alg?n tipo de relaci?n sexual en toda su vida y ha sido 
#recogida para participantes de entre 18 y 69 a?os. Tiene un 31% de NA y debido 
#a la informacion que representa convertiremos esta variable a categ?rica d?ndole 
#distintas categor?as por rangos de intervalo. Adem?s, imputaremos el rango de 
#edad no contemplado en una nueva categoria a la que llamaremos NotAsked

# nhanesoldraw %>% mutate(sexnumpartnlife=cut(sexnumpartnlife,
#                                         breaks=c(-Inf,0,5,10,25,Inf),
#                                         labels=c("None","1to5","6to10","11to25","MoreThan25")),
#                         sexnumpartnlife=case_when(age>69 ~ "NotAsked",
#                                                   TRUE ~ as.character(sexnumpartnlife))) %>%
#   group_by(sexnumpartnlife) %>% count(sexnumpartnlife, diabetes) %>%
#   mutate(porc=n/sum(n))

# A tibble: 14 x 4
# Groups:   sexnumpartnlife [7]
# sexnumpartnlife diabetes     n   porc
# <chr>           <fct>    <int>  <dbl>
#   1 11to25          No        1246 0.887 
# 2 11to25          Yes        158 0.113 
# 3 1to5            No        3521 0.888 
# 4 1to5            Yes        446 0.112 
# 5 6to10           No        1630 0.913 
# 6 6to10           Yes        156 0.0873
# 7 MoreThan25      No         705 0.837 
# 8 MoreThan25      Yes        137 0.163 
# 9 None            No         476 0.902 
# 10 None            Yes         52 0.0985
# 11 NotAsked        No        1467 0.736 
# 12 NotAsked        Yes        527 0.264 
# 13 NA              No        1661 0.893 
# 14 NA              Yes        200 0.107

#En esta variable, los NA podrian significar algo. Por ello, imputaremos los NA 
#como una categoria mas ("NA")

### sexnumpartyear ->
#Esta variable refleja el n?mero de parejas del sexo opuesto con las que el
#participante ha tenido alg?n tipo de relaci?n sexual en los ?ltimos 12 meses y 
#ha sido recogida para participantes de entre 18 y 69 a?os. Tiene un 8% de NA y 
#debido a la informacion que representa convertiremos esta variable a categ?rica
#d?ndole distintas categor?as por rangos de intervalo. Adem?s, imputaremos el 
#rango de edad no contemplado en una nueva categoria a la que llamaremos NotAsked

# nhanesoldraw %>% mutate(sexnumpartyear=cut(sexnumpartyear,
#                                        breaks=c(-Inf,0,1,3,Inf),
#                                        labels=c("None","1","2or3","MoreThan3")),
#                         sexnumpartyear=case_when(age>69 ~ "NotAsked",
#                                        TRUE ~ as.character(sexnumpartyear))) %>%
# group_by(sexnumpartyear) %>% count(sexnumpartyear, diabetes) %>%
# mutate(porc=n/sum(n))

# A tibble: 12 x 4
# Groups:   sexnumpartyear [6]
# sexnumpartyear diabetes     n   porc
# <chr>          <fct>    <int>  <dbl>
#   1 1              No        4134 0.932 
# 2 1              Yes        304 0.0685
# 3 2or3           No         860 0.943 
# 4 2or3           Yes         52 0.0570
# 5 MoreThan3      No         362 0.938 
# 6 MoreThan3      Yes         24 0.0622
# 7 None           No        1146 0.881 
# 8 None           Yes        155 0.119 
# 9 NotAsked       No        1467 0.736 
# 10 NotAsked       Yes        527 0.264 
# 11 NA             No        2737 0.817 
# 12 NA             Yes        614 0.183 

#En esta variable, los NA podrian significar algo. Por ello, imputaremos los NA 
#como una categoria mas ("NA")

### bpsys ->
#Esta variable refleja la media de distintas mediciones de la presion arterial
#sistolica. Tiene 8% de NA

### bpdia ->
#Esta variable refleja la media de distintas mediciones de la presion arterial
#diastolica. Tiene 8% de NA

#----6.2.Depuracion nhanesold-----
nhanesold <- nhanesoldraw %>%
  #CATEGORICAS
  #Creacion NotAsked u otras categorias en variables categoricas
  mutate(education= case_when(education=="8th Grade" ~ "8th_Grade",
                              education=="9 - 11th Grade" ~ "9_11th_Grade",
                              education=="College Grad" ~ "College_Grad",
                              education=="High School" ~ "High_School",
                              education=="Some College" ~ "Some_College",
                              age<20 ~ "NotAsked",
                              TRUE ~ as.character(education)),
         maritalstatus= case_when(maritalstatus %in% c("Divorced","Separated") ~ "Separated",
                                  maritalstatus %in% c("LivePartner","NeverMarried") ~ "NeverMarried",
                                  age<20 ~ "NeverMarried",
                                  TRUE ~ as.character(maritalstatus)),
         homeown= case_when(homeown %in% c("Rent","Other") ~ "NotOwn",
                            TRUE ~ as.character(homeown)),
         work= case_when(work %in% c("Looking") ~ "NotWorking",
                         TRUE ~ as.character(work)),
         healthgen= case_when(healthgen %in% c("Excellent","Vgood") ~ "Vgood",
                              healthgen %in% c("Fair","Poor") ~ "Fair",
                              is.na(healthgen)==TRUE ~ "NA",
                              TRUE ~ as.character(healthgen)),
         littleinterest= case_when(is.na(littleinterest)==TRUE ~ "NA",
                              TRUE ~ as.character(littleinterest)),
         depressed= case_when(is.na(depressed)==TRUE ~ "NA",
                              TRUE ~ as.character(depressed)),
         tvhrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
                              tvhrsday %in% c("0_hrs","0_to_1_hr","1_hr") ~ "0to1hr",
                              tvhrsday %in% c("2_hr","3_hr") ~ "2to3hr",
                              tvhrsday %in% c("4_hr","More_4_hr") ~ "from4hr",
                              TRUE ~ as.character(tvhrsday)),
         comphrsday= case_when((surveyyr=="2009_10") ~ "NotAsked",
                                comphrsday %in% c("1_hr","2_hr","3_hr","4_hr","More_4_hr") ~ "from1hr",
                                TRUE ~ as.character(comphrsday)),
         alcohol12plusyr= case_when(is.na(alcohol12plusyr)==TRUE ~ "NA",
                                    TRUE ~ as.character(alcohol12plusyr)),
         #revisar smokenow
         smokenow= case_when((age<20 | smoke100=="No") ~ "NotAsked",
                              TRUE ~ as.character(smokenow)),
         smoke100= case_when((age<20) ~ "NotAsked",
                              TRUE ~ as.character(smoke100)),
         #creamos variable marij
         marij= case_when(age>59 ~ "NotAsked",
                          marijuana=="No" ~ "Never",
                          marijuana=="Yes" & regularmarij=="No" ~ "HasTried",
                          marijuana=="Yes" & regularmarij=="Yes" ~ "MarijUser",
                          marijuana=="Yes" & is.na(regularmarij)==TRUE ~ "HasTried",
                          is.na(marijuana)==TRUE ~ "NA",
                          TRUE ~ as.character(regularmarij)),
         harddrugs= case_when((age>69) ~ "NotAsked",
                               is.na(harddrugs)==TRUE ~ "NA",
                               TRUE ~ as.character(harddrugs)),
         sexever= case_when((age>69) ~ "NotAsked",
                             is.na(sexever)==TRUE ~ "NA",
                             TRUE ~ as.character(sexever)),
         samesex= case_when((age>69) ~ "NotAsked",
                             is.na(samesex)==TRUE ~ "NA",
                             TRUE ~ as.character(samesex)),
  #NUMERICAS
   #habr?a que ver las siguientes agrupaciones, pero hay que hacer las mismas que en nhanesyoung
   #daysphyshlthbad y daysmenthlthbad
         daysphyshlthbad=cut(daysphyshlthbad,
                             breaks=c(-Inf,0,4,10,Inf),
                             labels=c("None","Few","Several","Many")),
         daysphyshlthbad=case_when((is.na(daysphyshlthbad)) ~ "NA",
                                    TRUE ~ as.character(daysphyshlthbad)),
         daysmenthlthbad=cut(daysmenthlthbad,
                             breaks=c(-Inf,0,4,10,Inf),
                             labels=c("None","Few","Several","Many")),
         daysmenthlthbad=case_when((is.na(daysmenthlthbad)) ~ "NA",
                                    TRUE ~ as.character(daysmenthlthbad)),
  #npregnancies, nbabies y age1stbaby
         npregnancies=cut(npregnancies,
                          breaks=c(-Inf,2,Inf),
                          labels=c("1or2","MoreThan2")),
         npregnancies=case_when(gender=='male' ~ "NotApply",
                                gender=='female' & age<20 ~ "NA",
                                is.na(npregnancies)==TRUE ~ "NA",
                                TRUE ~ as.character(npregnancies)),
         nbabies=cut(nbabies,
                     breaks=c(-Inf,2,Inf),
                     labels=c("0to2","MoreThan2")),
         nbabies=case_when(gender=='male' ~ "NotApply",
                           gender=='female' & age<20 ~ "NA",
                           is.na(nbabies)==TRUE ~ "NA",
                           TRUE ~ as.character(nbabies)),
         age1stbaby=cut(age1stbaby,
                        breaks=c(-Inf,21,Inf),
                        labels=c("21orLess","22orMore")),
         age1stbaby=case_when(gender=='male' ~ "NotApply",
                              gender=='female' & age<20 ~ "NA",
                              is.na(age1stbaby)==TRUE ~ "NA",
                              TRUE ~ as.character(age1stbaby)),
   #physactivedays
         # physactivedays=cut(physactivedays,
         #                     breaks=c(-Inf,0,3,Inf),
         #                     labels=c("Ninguno","Alguno","Muchos")),
  #alcoholday
         alcoholday=cut(alcoholday,
                        breaks=c(-Inf,1,3,Inf),
                        labels=c("1","2or3","MoreThan3")),
         alcoholday=case_when(is.na(alcoholday)==TRUE ~ "NA",
                               TRUE ~ as.character(alcoholday)),
  #smokeage, age1stmarij, ageregmarij y sexage
         smokeage=cut(smokeage,
                      breaks=c(-Inf,17,Inf),
                      labels=c("Before18","18orMore")),
         smokeage=case_when(age<20 ~ "NA",
                            smoke100=="No" ~ "NotApply",
                            is.na(smokeage)==TRUE ~ "NA",
                            TRUE ~ as.character(smokeage)),
         agefirstmarij=cut(agefirstmarij,
                           breaks=c(-Inf,17,Inf),
                           labels=c("Before18","18orMore")),
         agefirstmarij=case_when(age>59 ~ "NotAsked",
                                  marij=="Never" ~ "NotApply",
                                  is.na(agefirstmarij)==TRUE ~ "NA",
                                  TRUE ~ as.character(agefirstmarij)),
         ageregmarij=cut(ageregmarij,
                         breaks=c(-Inf,17,Inf),
                         labels=c("Before18","18orMore")),
         ageregmarij=case_when(age>59 ~ "NotAsked",
                                marij=="Never" | marij=="HasTried" ~ "NotApply",
                                is.na(ageregmarij)==TRUE ~ "NA",
                                TRUE ~ as.character(ageregmarij)),
         sexage=cut(sexage,
                    breaks=c(-Inf,15,18,Inf),
                    labels=c("15orLess","16to18","18orMore")),
         sexage=case_when(is.na(sexage)==TRUE ~ "NA",
                          TRUE ~ as.character(sexage)), 
   #sexnumpartnlife y sexnumpartyear
          sexnumpartnlife=cut(sexnumpartnlife,
                              breaks=c(-Inf,0,5,10,25,Inf),
                              labels=c("None","1to5","6to10","11to25","MoreThan25")),
          sexnumpartnlife=case_when(age>69 ~ "NotAsked",
                                    is.na(sexnumpartnlife)==TRUE ~ "NA",
                                    TRUE ~ as.character(sexnumpartnlife)),
          sexnumpartyear=cut(sexnumpartyear,
                             breaks=c(-Inf,0,1,Inf),
                             labels=c("None","1","MoreThan1")),
          sexnumpartyear=case_when(age>69 ~ "NotAsked",
                                   is.na(sexnumpartyear)==TRUE ~ "NA",
                                   TRUE ~ as.character(sexnumpartyear))) %>%
  #Eliminamos variables
  dplyr::select(-length, -tvhrsdaychild, -comphrsdaychild, -testosterone, 
                -marijuana, -regularmarij, -sexorientation) %>%
  #Outliers
   mutate(
     weight = ifelse(abs(weight - median(weight, na.rm=TRUE)) > 
                       6 * mad(weight, na.rm = TRUE), NA, weight),
     height = ifelse(abs(height - median(height, na.rm=TRUE)) > 
                       6 * mad(height, na.rm = TRUE), NA, height),
     pulse = ifelse(abs(pulse - median(pulse, na.rm=TRUE)) > 
                      6 * mad(pulse, na.rm = TRUE), NA, pulse),
     directchol= ifelse(abs(directchol - median(directchol, na.rm=TRUE)) > 
                          6 * mad(directchol, na.rm = TRUE), NA, directchol),
     totchol= ifelse(abs(totchol - mean(totchol, na.rm=TRUE)) > 
                       1.5 * sd(totchol, na.rm = TRUE), NA, totchol),
     urinevol1 = ifelse(abs(urinevol1 - median(urinevol1, na.rm=TRUE)) > 
                          6 * mad(urinevol1, na.rm = TRUE), NA, urinevol1),
     urineflow1 = ifelse(abs(urineflow1 - median(urineflow1, na.rm=TRUE)) > 
                           6 * mad(urineflow1, na.rm = TRUE), NA, urineflow1),
     alcoholyear = ifelse(abs(alcoholyear - median(alcoholyear, na.rm=TRUE)) > 
                           6 * mad(alcoholyear, na.rm = TRUE), NA, alcoholyear),
     bpsys = ifelse(abs(bpsys - mean(bpsys, na.rm=TRUE)) > 
                      1.5 * sd(bpsys, na.rm = TRUE), NA, bpsys),
     bpdia = ifelse(abs(bpdia - median(bpdia, na.rm=TRUE)) > 
                      6 * mad(bpdia, na.rm = TRUE), NA, bpdia))
 

nhanesold %>% skim
#prop.table(table(nhanesold$marij))

#----6.3.Imputacion-----
#Variable numericas discretas (para imputar las pasamos a factor y luego
#deshacemos cambios)
nhanesold <- nhanesold %>% mutate(homerooms=as.factor(homerooms),
                                  pulse=as.factor(pulse),
                                  sleephrsnight=as.factor(sleephrsnight),
                                  physactivedays=as.factor(physactivedays),
                                  alcoholyear=as.factor(alcoholyear),
                                  #categoricas
                                  across(where(is.character),as.factor))

#Imputamos mediante ?rboles
imputacion <- mice(data=nhanesold, m=1, method="cart")
nhanesolddep <- complete(imputacion)

nhanesolddep <- nhanesolddep %>% mutate(homerooms=as.numeric(homerooms),
                                        pulse=as.numeric(pulse),
                                        sleephrsnight=as.numeric(sleephrsnight), 
                                        physactivedays=as.numeric(physactivedays),
                                        alcoholyear=as.numeric(alcoholyear))
nhanesolddep %>% skim
class(nhanesolddep)

#----6.4.Creacion dummies----
#Creamos dummies
nhanesolddep_dummy <- dummy.data.frame(nhanesolddep,
                                      c("surveyyr", "gender", "race1", "education",
                                        "maritalstatus","homeown", "work", "healthgen",
                                        "daysphyshlthbad", "daysmenthlthbad",
                                        "littleinterest", "depressed", "npregnancies",
                                        "nbabies", "age1stbaby", "sleeptrouble",
                                        "tvhrsday", "comphrsday", "alcohol12plusyr",
                                        "alcoholday", "smokenow", "smoke100", "smokeage",
                                        "agefirstmarij", "ageregmarij", "harddrugs",
                                        "sexever", "sexage", "sexnumpartnlife",
                                        "sexnumpartyear", "samesex", "marij"),
                                        sep = ".")

#Antes de ver todas las correlaciones, vamos a ver que variables tras haber 
#hecho dummies no son necesarias debido a que ya sabemos que van a tener 
#correlaci?n=1 con otras variables. Nos estamos refiriendo a aquellas categorias
#creadas en distintas variables donde se agrupan participantes que no han sido 
#preguntados por ellas. Por ejemplo, sabemos que existe una correlaci?n de 1 
#entre "surveyyr.2009_10" y "tvhrsday.NotAsked" ? "surveyyr.2009_10" y
#"comphrsday.NotAsked" debido a que estas variables, si recordamos, 
#no hab?an sido recogidas en la encuesta de ese a?o. Tambi?n hay correlaciones 
#entre variables que se corresponden a una variable con 2 ?nicas categor?as, por
#lo que eliminaremos una de las 2.

df_correl <-  nhanesolddep_dummy %>% dplyr::select(-diabetes)
matriz_cor <- round(df_correl %>% cor(),3)

for (i in 1:nrow(matriz_cor)) {
  k=i+1
  if (k!=nrow(matriz_cor)){
    for (j in k:nrow(matriz_cor)) {
      if (abs(matriz_cor[i,j])==1) {
        print(c((matriz_cor[i,j]), 
                names(matriz_cor[1,])[i], 
                names(matriz_cor[1,])[j]))
      }
    }
  }
  else break
}

#Sabemos que las siguientes correlaciones entre variables van a ser 1:

#surveyyr.2009_10, tvhrsday.NotAsked y comphrsday.NotAsked

#gender.male, npregnancies.NotApply, nbabies.NotApply y age1stbaby.NotApply

#education.NotAsked y smoke100.NotAsked 

#smoke100.No y smokeage.NotApply

#marij.Never y agefirstmarij.NotApply 

#agefirstmarij.NotAsked, ageregmarij.NotAsked y marij.NotAsked

#marij.NA y ageregmarij.NA

#harddrugs.NotAsked, sexever.NotAsked, samesex.NotAsked, 
#sexnumpartnlife.NotAsked y sexnumpartyear.NotAsked 

#Dejamos ?nicamente una de las variables de cada uno de los anteriores grupos,
#es decir, se eliminan las variables IGUALES

nhanesold_dummy <- nhanesolddep_dummy %>% dplyr::select(
  -tvhrsday.NotAsked, -comphrsday.NotAsked,
  -npregnancies.NotApply, -nbabies.NotApply, -age1stbaby.NotApply,
  -smoke100.NotAsked,
  -smokeage.NotApply,
  -agefirstmarij.NotApply,
  -agefirstmarij.NotAsked,-ageregmarij.NotAsked,
  -ageregmarij.NA,
  -sexever.NotAsked,-samesex.NotAsked,-sexnumpartnlife.NotAsked,-sexnumpartyear.NotAsked,
  #variables binarias
  -surveyyr.2011_12, -gender.female, -homeown.NotOwn, -work.NotWorking,
  -sleeptrouble.No,
  #correlacion negativa entre distintas variables
  -smokenow.NotAsked)

nhanesold_dummy <- nhanesold_dummy %>% dplyr::select(
  #Ahora elimino DUMMIES K dejando asi k- dummies para cada variable categ?rica
  -race1.Other,-education.NotAsked,-maritalstatus.Widowed,
  -healthgen.NA,
  -daysphyshlthbad.Several,
  -daysmenthlthbad.Several,
  -littleinterest.Most,
  -depressed.Most,
  -alcohol12plusyr.NA,
  -alcoholday.MoreThan3,
  -harddrugs.Yes,
  -sexage.18orMore,
  -marij.NA)

#-----7.Preparamos conjuntos train y test y balanceamos-----
#Ahora realizaremos la partici?n en train-test para que todos los algoritmos
#trabajen con el mismo conjunto de datos para su entrenamiento

#vemos si la variable objetivo esta desbalanceada
nhanesold_dummy %>% count(diabetes) %>% mutate(porc=n/sum(n))
#vemos que si lo esta

#se crean conjuntos train y test
set.seed(1234)
nhs_split <- initial_split(nhanesold_dummy, strata = diabetes, prop = 0.8)
train_nhs <- training(nhs_split)
test_nhs <- testing(nhs_split)
train_nhs %>% count(diabetes) %>% mutate(porc=n/sum(n))
test_nhs %>% count(diabetes) %>% mutate(porc=n/sum(n))

#balanceamos la variable objetivo
p=0.4 #p es el porcentaje de observaciones al que queremos aumentar la clase minoritaria

#a continuaci?n, aumentamos las observaciones de la clase minoritaria
nhs_0 <- train_nhs[sample(which(train_nhs$diabetes=="Yes"), 
                          (sum(with(train_nhs,diabetes == "No"))*p)/(1-p), replace=TRUE),]
nhs_1 <- train_nhs[train_nhs$diabetes=="No", ]

#a continuaci?n, disminuimos las observaciones de la clase mayoritaria
# nhs_1 <- train_nhs[sample(which(train_nhs$diabetes=="No"), 
#                           (sum(with(train_nhs,diabetes == "Yes"))*(1-p))/p, replace=FALSE),]
# nhs_0 <- train_nhs[train_nhs$diabetes=="Yes", ]


#nuestro conjunto de entrenamiento para los modelos sera ***nhs***
nhs <- rbind(nhs_0,nhs_1)
nhs %>% count(diabetes) %>% mutate(porc=n/sum(n))
#guardamos el conjunto nhs que es el conjunto de entrenamiento de nuestros 
#modelos ya balanceado

#test_nhs sera el conjunto test para arboles y random forest
#para el resto de modelos, hará falta estandarizar en base a la media y 
#desviación típica del conjunto train
setwd("C:/Users/MediaService/Desktop/MASTER/TFM/Código Febrero/rds")
#saveRDS(nhs, file = "nhs.rds") #dataset balanceado
#saveRDS(test_nhs, file = "test_nhs.rds") #conjunto test

#-----8. Mas cosas-----
# df_correl <-  nhanesold_dummy %>% dplyr::select(-diabetes)
# matriz_cor <- round(df_correl %>% cor(),3)
# 
# for (i in 1:nrow(matriz_cor)) {
#   k=i+1
#   if (k!=nrow(matriz_cor)){
#     for (j in k:nrow(matriz_cor)) {
#       if (abs(matriz_cor[i,j])==1) {
#         print(c((matriz_cor[i,j]), 
#                 names(matriz_cor[1,])[i], 
#                 names(matriz_cor[1,])[j]))
#       }
#     }
#   }
#   else break
#}

#Existen muchas categor?as de distintas variables con correlaciones altas debido
#a haber categorizado los NA. Optamos por eliminar aquellas con correlaciones
#mayores a 0.9 (en valor absoluto)

#healthgen.NA, daysphyshlthbad.NA, daysmenthlthbad.NA, littleinterest.NA,
#depressed.NA, alcohol12plusyr.NA

#npregnancies.NA, nbabies.NA

#smoke100.Yes, smoke100.No 

#marij.NA, agefirstmarij.NA, harddrugs.NA, sexever.NA, sexnumpartnlife.NA, 
#samesex.NA y sexorientation.NA 

#sexever.No, sexnumpartnlife.None

#sexever.Yes, sexage.NA

# nhanesold_dummy_svm <- nhanesold_dummy_trees %>% dplyr::select(
#   -daysphyshlthbad.NA, -daysmenthlthbad.NA, -littleinterest.NA,
#   -depressed.NA, -alcohol12plusyr.NA, -nbabies.NA, -smoke100.No,
#   -agefirstmarij.NA, -harddrugs.NA, -sexever.NA, -sexnumpartnlife.NA, 
#   -samesex.NA, -sexorientation.NA, -sexnumpartnlife.None, -sexage.NA)
# 
# df_correl2 <- nhanesold_dummy_svm %>% dplyr::select(-diabetes)
# 
# matriz_cor2 <- round(df_correl2 %>% cor(),3)
# 
# for (i in 1:nrow(matriz_cor2)) {
#   k=i+1
#   if (k!=nrow(matriz_cor2)){
#     for (j in k:nrow(matriz_cor2)) {
#       if (abs(matriz_cor2[i,j])>0.85) {
#         print(c((matriz_cor2[i,j]), 
#               names(matriz_cor2[1,])[i], 
#               names(matriz_cor2[1,])[j]))
#      }
#     }
#   }
#   else break
# }

