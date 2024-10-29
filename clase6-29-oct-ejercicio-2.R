library(lavaan)
library(MVN)
library(dplyr)
library(psych)
library(ggplot2)
setwd("C:/Users/nnapo/Documents/PhD Classes/optativo MLVL/exercises/data")

base <- read.csv("Taller_clase6.csv", sep = ';')

names(base)

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

# Estimación del modelo
# Usaremos el comando growth() de lavaan
# "fiml" para tratamiento de datos perdidos
fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="ml")
summary(fit0, fit.measures= T, standardized=T)

## Estimación del modelo
# Usaremos el comando growth() de lavaan
# "fiml" para tratamiento de datos perdidos
fit0<- growth(modelo0, data = base,
              missing="fiml", se="robust", estimator="ml")
summary(fit0, fit.measures= T, standardized=T)

# seleccionamos 50 observaciones
data50 = base[sample(nrow(base),50),]
dim(data50)

# cambiamos el formato de la base de datos de wide a long
# comando reshape()
data50L = reshape(data=data50, # subset de datos
                  idvar='id', # variable de identificación
                  varying=c('monit12','monit13','monit14','monit15'),
                  v.names = 'monitoreo', # nombre que agruará las medidas
                  times = c(12,13,14,15), # eje x
                  direction='long')
dim(data50L) ## revisamos que se haya creado el subset

# Plot con comando ggplot()
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
plot_obs







