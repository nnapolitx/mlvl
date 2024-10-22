setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/clases/data")
library(psych)
library(MVN)
library(lavaan)
library(dplyr)

base<- read.csv("rezagos.csv", header = T, sep = ";", na= 99)
names(base)
str(base)
describe(base)

###### Modelo autorregresivo clásico sin restricciones
mod1<- '# Regresiones para restricción verbal PADRE (Dad)
        T2DVC ~ T1MVC  # Efecto cruzado
        T2DVC ~ T1DVC  # Efecto autorregresivo
        T3DVC ~ T2MVC  # Efecto cruzado
        T3DVC ~ T2DVC  # Efecto autorregresivo
        
        # Regresiones para restricción verbal MADRE (Mom)
        T2MVC ~ T1MVC  # Efecto autorregresivo
        T2MVC ~ T1DVC  # Efecto cruzado
        T3MVC ~ T2MVC  # Efecto autorregresivo
        T3MVC ~ T2DVC  # Efecto cruzado
        
        # Correlaciones entre residuos y varianzas
        T1MVC ~~ T1DVC;
        T2MVC ~~ T2DVC;
        T3MVC ~~ T3DVC;
        '
CLPM <- sem(mod1, data = base, mimic = "mplus")
summary(CLPM, fit.measures = TRUE, standardized = T)


###### Modelo autorregresivo clásico con restricciones

mod2<- '# Regresiones para restricción verbal PADRE
        T2DVC ~ c*T1MVC  # Efectos cruzados fijos a iguales
        T2DVC ~ a*T1DVC  # Efectos autorregresivos fijos a iguales
        T3DVC ~ c*T2MVC
        T3DVC ~ a*T2DVC
        
        # Regresiones para restricción verbal MADRE
        T2MVC ~ b*T1MVC # Efectos autorregresivos fijos a iguales
        T2MVC ~ d*T1DVC # Efectos cruzados fijos a iguales
        T3MVC ~ b*T2MVC
        T3MVC ~ d*T2DVC
        
        # Correlaciones entre residuos y varianzas
        T1MVC ~~ T1DVC;
        T2MVC ~~ e*T2DVC;  # Correlación entre residuos fijos a iguales
        T3MVC ~~ e*T3DVC;
        '
CLPM_R <- sem(mod2, data = base, mimic = "mplus")
summary(CLPM_R, fit.measures = TRUE, standardized = T)

anova(CLPM,CLPM_R)
# new model is more parsimonious. look at diff in chi2 as well as pvalue of diff.

####################################

###### Modelo autorregresivo con intercepto aleatorio

mod3<- '# Componente entre-sujetos (between, RI)
	    RI_VCM =~ 1*T1MVC + 1*T2MVC + 1*T3MVC
    	RI_VCD =~ 1*T1DVC + 1*T2DVC + 1*T3DVC
	        # Componentes intra-sujetos (within) 
	    cT1MVC =~ 1*T1MVC 
	    cT2MVC =~ 1*T2MVC 
    	cT3MVC =~ 1*T3MVC 
    	cT1DVC =~ 1*T1DVC 
    	cT2DVC =~ 1*T2DVC
	    cT3DVC =~ 1*T3DVC
          # Residuos restringidos a cero
	    T1MVC ~~ 0*T1MVC
	    T2MVC ~~ 0*T2MVC
    	T3MVC ~~ 0*T3MVC
    	T1DVC ~~ 0*T1DVC
    	T2DVC ~~ 0*T2DVC
    	T3DVC ~~ 0*T3DVC
	       # Efectos autorregresivos y cruzados 
	    cT2DVC ~ c*cT1MVC 
    	cT2DVC ~ a*cT1DVC  
    	cT3DVC ~ c*cT2MVC 
    	cT3DVC ~ a*cT2DVC   
    	cT2MVC ~ b*cT1MVC  
	    cT2MVC ~ d*cT1DVC  
	    cT3MVC ~ b*cT2MVC 
	    cT3MVC ~ d*cT2DVC  
	       # Covarianza entre los efectos aleatorios
	    RI_VCM ~~ RI_VCD
	       # Covarianza entre componentes within T1 
	    cT1MVC ~~ cT1DVC
	       # Estimación de las covarianzas entre los residuos
    	cT2MVC ~~ cov*cT2DVC 
    	cT3MVC ~~ cov*cT3DVC 
        # COV fijas en 0 
	    RI_VCM ~~ 0*cT1MVC + 0*cT1DVC
    	RI_VCD ~~ 0*cT1MVC + 0*cT1DVC'

RI_CLPM <- sem(mod3, data = base, mimic = "mplus")
summary(RI_CLPM, fit.measures = TRUE, standardized = T)
