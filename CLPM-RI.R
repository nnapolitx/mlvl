setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")
base<- read.csv("CLPM-MI.csv", header = T, sep = ";")
names(base)

library(lavaan)
library(psych)
library(MVN)
library(dplyr)

describe(base)

mod0 <- '
  ####################
  # Modelo de medida #
  ####################
  
  ansiedad1=~ AS1_t1 + AS2_t1 + AS3_t1
  ansiedad2=~ AS1_t2 + AS2_t2 + AS3_t2
  ansiedad3=~ AS1_t3 + AS2_t3 + AS3_t3
  ansiedad4=~ AS1_t4 + AS2_t4 + AS3_t4
  estima1=~ AE1_t1 + AE2_t1 + AE3_t1
  estima2=~ AE1_t2 + AE2_t2 + AE3_t2
  estima3=~ AE1_t3 + AE2_t3 + AE3_t3
  estima4=~ AE1_t4 + AE2_t4 + AE3_t4
  
  ################
  # BETWEEN PART #
  ################
  
  # Creación del factor RI
  RIX =~ 1*ansiedad1 + 1*ansiedad2 + 1*ansiedad3 + 1*ansiedad4
  RIY =~ 1*estima1 + 1*estima2 + 1*estima3 + 1*estima4 
 
  ###############
  # WITHIN PART #
  ###############
  
  WFX1 =~ 1*ansiedad1 
  WFX2 =~ 1*ansiedad2
  WFX3 =~ 1*ansiedad3 
  WFX4 =~ 1*ansiedad4 
  
  WFY1 =~ 1*estima1
  WFY2 =~ 1*estima2
  WFY3 =~ 1*estima3
  WFY4 =~ 1*estima4
  
  #########################
  # WITHIN PART: dinámico #
  #########################
  
  # Especificación de los efectos autorregresivos y cruzados
  WFX2 + WFY2 ~ WFX1 + WFY1
  WFX3 + WFY3 ~ WFX2 + WFY2
  WFX4 + WFY4 ~ WFX3 + WFY3
  
  # Correlación entre olas
  WFX1 ~~ WFY1
  WFX2 ~~ WFY2
  WFX3 ~~ WFY3
  WFX4 ~~ WFY4
  
  #############################
  # RESTRICCIONES ADICIONALES #
  #############################
  
  # COVARIANZA ENTRE RI y VALIABLE EXÓGENA FIJA A 0
  RIX  + RIY ~~ 0*WFX1 + 0*WFY1
'

fit0 <- cfa(mod0, 
            data = base, 
            estimator = "MLR",
            std.lv= T,
            meanstructure = TRUE
)

summary(fit0, standardized = T, fit.measures = T)


