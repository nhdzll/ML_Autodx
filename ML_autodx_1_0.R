# sketchy::make_compendium(name = "ML_Autodx", path = "C:/Users/inp.mx/Documents/Tavarich/2023")
# sketchy::print_skeleton()
#.
#│   
#├── data/  
#  │   ├── processed/  
#  │   └── raw/  
#  ├── manuscript/  
#  ├── output/  
#  └── scripts/ 

setwd("C:/Users/inp.mx/Documents/Tavarich/2023/ML_Autodx")
library(tidyverse)
library(caret)
library(tictoc)
library(doParallel)

# Cargamos los datos de las herramientas de autodiagnóstico
tic()
source("./scripts/clean_ansi.R")
source("./scripts/clean_depre.R")
source("./scripts/clean_alcohol.R")
source("./scripts/clean_tabaco.R")
toc()
# Criterio de exclusión: usuarios de la plataforma mayores de 18 años
tic()
ansi <- subset(ansi, EDAD > 17)
depre <- subset(depre, EDAD > 17)
tabaco <- subset(tabaco, EDAD > 17)
alcohol <- subset(alcohol, EDAD > 17)
toc()
###############################################################################
#Estadísticos descriptivos de la muestra 
#Alcohol
min(alcohol$date) #Fecha debe ser = 2021-02-17
max(alcohol$date) #Fecha debe ser < 2022-06-20

table(alcohol$sexo)
mean(alcohol$EDAD, na.rm = TRUE)
sd(alcohol$EDAD, na.rm = TRUE)
table(alcohol$LGBTI)
table(alcohol$INDIGENA)
table(alcohol$ESTADO)
table(alcohol$solici_atend)

table(alcohol$ESTADO)
table(alcohol$year)
table(alcohol$month)
table(alcohol$hour)
#FALSE  TRUE 
#8298   407 4.26%

#Tabaco
min(tabaco$date) #Fecha debe ser = 2021-02-17
max(tabaco$date) #Fecha debe ser < 2022-06-20

table(tabaco$sexo)
mean(tabaco$EDAD, na.rm = TRUE)
sd(tabaco$EDAD, na.rm = TRUE)
table(tabaco$LGBTI)
table(tabaco$INDIGENA)
table(tabaco$solici_atend)

table(tabaco$ESTADO)
table(tabaco$year)
table(tabaco$month)
table(tabaco$hour)
#FALSE  TRUE 
#10762   3423 24.1%

#Ansiedad
min(ansi$date) #Fecha debe ser = 2021-02-17
max(ansi$date) #Fecha debe ser < 2022-06-20

table(ansi$sexo)
mean(ansi$EDAD, na.rm = TRUE)
sd(ansi$EDAD, na.rm = TRUE)
table(ansi$LGBTI)
table(ansi$INDIGENA)
table(ansi$solici_atend)

table(ansi$ESTADO)
table(ansi$year)
table(ansi$month)
table(ansi$hour)
#FALSE  TRUE 
#10262   664 6.08%

#Depresión
min(depre$date) #Fecha debe ser = 2021-02-17
max(depre$date) #Fecha debe ser <= 2022-06-20

table(depre$sexo)
mean(depre$EDAD, na.rm = TRUE)
sd(depre$EDAD, na.rm = TRUE)
table(depre$LGBTI)
table(depre$INDIGENA)
table(depre$solici_atend)
#FALSE  TRUE 
#58844  1664 2.8%
table(depre$ESTADO)
table(depre$year)
table(depre$month)
table(depre$hour)

alcohol$date <- NULL
tabaco$date <- NULL
depre$date <- NULL
ansi$date <- NULL
##############################################################################
#Análisis de los reactivos

alc_react <- alcohol[ ,c("R1","R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10")]
ltm::cronbach.alpha(alc_react)
apply(alc_react,2, mean)
apply(alc_react,2, sd)
apply(alc_react,2, min)
apply(alc_react,2, max)
apply(alc_react,2, function(x) sum(is.na(x)))

tab_react <- tabaco[ ,c("R1","R2", "R3", "R4", "R5", "R6")]
ltm::cronbach.alpha(tab_react)
apply(tab_react,2, mean)
apply(tab_react,2, sd)
apply(tab_react,2, min)
apply(tab_react,2, max)
apply(tab_react,2, function(x) sum(is.na(x)))

ansi_react <- ansi[ ,c("R1","R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
                       "R11","R12", "R13", "R14", "R15", "R16", "R17", "R18", 
                       "R19", "R20", "R21")]
ltm::cronbach.alpha(ansi_react)
apply(ansi_react,2, mean)
apply(ansi_react,2, sd)
apply(ansi_react,2, min)
apply(ansi_react,2, max)
apply(ansi_react,2, function(x) sum(is.na(x)))

dep_react <- depre[ ,c("R1","R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
                       "R11","R12", "R13", "R14", "R15", "R16", "R17", "R18", 
                       "R19", "R20", "R21")]
ltm::cronbach.alpha(dep_react)

apply(dep_react,2, mean)
apply(dep_react,2, sd)
apply(dep_react,2, min)
apply(dep_react,2, max)
apply(dep_react,2, function(x) sum(is.na(x)))

write.csv(alcohol, "./data/processed/alcohol.csv")
write.csv(tabaco, "./data/processed/tabaco.csv")
write.csv(ansi, "./data/processed/ansiedad.csv")
write.csv(depre, "./data/processed/depresion.csv")