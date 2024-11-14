library(lavaan)
library(MVN)
library(dplyr)
library(psych)
library(ggplot2)
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

base <- read.csv("Taller_clase6.csv", sep = ';')

names(base)
str(base)

describe(base, skew = F)

mvn(base[10:16], mvnTest = "mardia", multivariatePlot = "qq",
    multivariateOutlierMethod = "adj", desc =F )
mvn(base[17:23], mvnTest = "mardia", multivariatePlot = "qq",
    multivariateOutlierMethod = "adj", desc =F)

# Especificación del modelo para monitoreo materno
modelo0<- "# Intercepto aleatorio
 i=~ 1*monit12 + 1*monit13+ 1*monit14 + 1*monit15
 # Interceptos fijos en 0
 monit12 ~ 0*1
 monit13 ~ 0*1
 monit14 ~ 0*1
 monit15 ~ 0*1
 "

# Estimación del modelo nulo
# Usaremos el comando growth() de lavaan
# "fiml" para tratamiento de datos perdidos
fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="ml")
summary(fit0, fit.measures= T, standardized=T)


# seleccionamos 50 observaciones
data50 = base[sample(nrow(base),50),]
dim(data50)
## [1] 50 23
# cambiamos el formato de la base de datos de wide a long
# comando reshape()
data50L = reshape(data=data50, # subset de datos
                  idvar='id', # variable de identificación
                  varying=c('monit12','monit13','monit14','monit15'),
                  v.names = 'monitoreo', # nombre que agruará las medidas
                  times = c(12,13,14,15), # eje x
                  direction='long')
dim(data50L) ## revisamos que se haya creado el subset

plot_obs = ggplot(data=data50L,
                  aes(x=time, y=monitoreo, group=id)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Edad") +
  scale_y_continuous(name = "Monitoreo") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color="black")
    ,axis.line.y = element_line(color="black")
    ,axis.text=element_text(size=20)
    ,axis.title=element_text(size=26)
  )
plot_obs ## Para obtener el gráfico

# lineal
modelo1<- "
 i=~ 1*monit12 + 1*monit13+ 1*monit14 + 1*monit15
 s=~ 0*monit12 + 1*monit13+ 2*monit14 + 3*monit15
 monit12 ~ 0*1
 monit13 ~ 0*1
 monit14 ~ 0*1
 monit15 ~ 0*1
"

fit1<- growth(modelo1, data = base,
              missing="fiml", se="robust", estimator="ml")
summary(fit1, fit.measures= T, standardized=T)

# cuadratico
modelo2<- "i=~ 1*monit12 + 1*monit13+ 1*monit14 + 1*monit15
 s=~ 0*monit12 + 1*monit13+ 2*monit14 + 3*monit15
 q=~ 0*monit12 + 1*monit13+ 4*monit14 + 9*monit15
 monit12 ~ 0*1
 monit13 ~ 0*1
 monit14 ~ 0*1
 monit15 ~ 0*1"
fit2<- growth(modelo2, data = base,
              missing="fiml", se="robust", estimator="ml")
summary(fit2, fit.measures= T, standardized=T)

anova(fit0, fit1, fit2)

# Modelo para conductas de riesgo

# Modelo nulo
modelo0<- "i=~ 1*riesgo12 + 1*riesgo13+ 1*riesgo14 + 1*riesgo15
 riesgo12 ~ 0*1
 riesgo13 ~ 0*1
 riesgo14 ~ 0*1
 riesgo15 ~ 0*1"
fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="mlr")
summary(fit0, fit.measures= T, standardized=T)

# lineal
modelo1<- "i=~ 1*riesgo12 + 1*riesgo13+ 1*riesgo14 + 1*riesgo15
 s=~ 0*riesgo12 + 1*riesgo13+ 2*riesgo14 + 3*riesgo15
 riesgo12 ~ 0*1
 riesgo13 ~ 0*1
 riesgo14 ~ 0*1
 riesgo15 ~ 0*1"
fit1<- growth(modelo1, data = base,
              missing="fiml", se="robust", estimator="mlr")
summary(fit1, fit.measures= T, standardized=T)


# cuadratico
modelo2<- "i=~ 1*riesgo12 + 1*riesgo13+ 1*riesgo14 + 1*riesgo15
 s=~ 0*riesgo12 + 1*riesgo13+ 2*riesgo14 + 3*riesgo15
 q=~ 0*riesgo12 + 1*riesgo13+ 4*riesgo14 + 9*riesgo15
 riesgo12 ~ 0*1
 riesgo13 ~ 0*1
 riesgo14 ~ 0*1
 riesgo15 ~ 0*1"
fit2<- growth(modelo2, data = base,
              missing="fiml", se="robust", estimator="mlr")
summary(fit2, fit.measures= T, standardized=T)

anova(fit0, fit1, fit2)

# ---- Mod Autoreg ----

mod2 <- '
RI_monit =~ 1*monit12 + 1*monit13+ 1*monit14 + 1*monit15
RI_riesg =~ 
# resid fixed to 0

# efectos autoregres y cruz

# covar entre efects aleatorios


'


