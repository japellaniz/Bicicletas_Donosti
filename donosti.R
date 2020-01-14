# ###############################################################################################
# Proyecto bicicletas Donosti ##################################################################
#################################################Jose Luis Apellániz############################
################################################################################################

####################################################################################
# 1.- Importación de datos: parsing y formatos de variables (continuas, categóricas, 
# fechas, etc.)
###################################################################################

# Datos capturados de la API del Ayuntamiento de Donosti.
# http://www.donostia.eus/info/ciudadano/Videowall.nsf/movimientos.xsp?
  
# Tomamos los datos del 1er semestre del 2019

library(jsonlite)
library(leaflet)
library(dplyr)
fechas = seq(from = as.POSIXct("2019/01/01"),to = as.POSIXct("2019/06/30"), by = 24*60*60)
url = "http://www.donostia.eus/info/ciudadano/Videowall.nsf/movimientos.xsp?"
datos = data.frame()
for (i in 1:length(fechas)){
  fecha = fechas[i]
  jsonurl = paste0(url,"fecha_inicial=",
                   format(fecha, "%Y-%m-%d"),
                   "%2000:00&fecha_final=",
                   format(fecha, "%Y-%m-%d"),
                   "%2023:59")
  parte = fromJSON(txt=jsonurl, flatten = TRUE, simplifyDataFrame=TRUE)
  datos = rbind(datos,parte)
  print(jsonurl)
}
write.csv(datos, "dat/historico_movimientos.csv", row.names = F)

# Leemos de nuevo el fichero para adaptar mejor los tipos
df = read.csv(file = "dat/historico_movimientos.csv", header = TRUE)

summary(df)
str(df)

# Campos tipo fecha.
df$fecha_desenganche = as.POSIXct(df$fecha_desenganche, format="%Y-%m-%d %H:%M:%S")
str(df$fecha_desenganche)
df$fecha_enganche = as.POSIXct(df$fecha_enganche, format="%Y-%m-%d %H:%M:%S")
str(df$fecha_enganche)

# Convertir campos de tipo factor.
df[,c(1,3)]=lapply(df[,c(1,3)],function(x) as.factor(x))

table(df$estacion_desenganche)
table(df$estacion_enganche)

# Aunque son pocos (5), corrregimos los valores "1000" en estación de enganche por el valor que aparece como estación de 
# desenganche inmediatamente posterior en el tiempo. He comprobado que en la mayoría de los casos, como es natural, la bici
# se desengancha de la estación a la que se enganchó por última vez.
dfTemp = subset(df,df$estacion_enganche == "1000")
for (i in 1:nrow(dfTemp)) {
  bici = dfTemp$idbici[i]
  dfTemp2 = subset(df,df$idbici == bici)
  dfTemp2 = arrange(dfTemp2, dfTemp2$fecha_desenganche)
  ind = as.integer(row.names(dfTemp2[dfTemp2$estacion_enganche == "1000",]))
  dfTemp2$estacion_enganche[ind] = dfTemp2$estacion_desenganche[ind+1]
  dfTemp$estacion_enganche[i] = dfTemp2$estacion_desenganche[ind+1]
}

df$estacion_enganche[as.integer(row.names(dfTemp))] = 
  dfTemp$estacion_enganche

table(df$estacion_enganche) 
c = levels(df$estacion_desenganche)
df$estacion_enganche = factor(df$estacion_enganche, levels = c)

str(df)
table(df$idbici)
plot(df$idbici)
# Se ve que no hay un uso homogéneo de las bicis. Este dato se podría utilizar para
# planificar los mantenimientos de las bicis.
par(mfrow=c(2,1))
plot(df$estacion_desenganche)
plot(df$estacion_enganche)
# Las estaciones tampoco tienen un uso homogéneo. Las que más se usan son la 1, la 12 
# y la 14 y las que menos la 7, la 9 y la 13.

################################################################################
# 2.- Estadísticos básicos para identificación de valores faltantes.
################################################################################

# Utilizamos función de R Bloggers para calcular el PORCENTAJE de valores
# perdidos de cada variable:
pMissed = function(x) {sum(is.na(x))/length(x)*100}
apply(X = df, MARGIN = 2, FUN = pMissed)

# Tenemos NAs en edad y codigo_postal
summary(df$codigo_postal)

# El Código Postal sólo pueden ser números de 01000 en adelante, luego filtramos los que no cumplen dicha condición.
dfCPMal = subset(df,df$codigo_postal<1000)
# Y los cambiamos por NAs
df$codigo_postal[as.integer(row.names(dfCPMal))] = NA
summary(df$codigo_postal)

# Campo edad.
hist(df$edad,
     main = "Distribución de la edad de los usuarios",
     xlab = "Edad", ylab = "Frecuencia",
     col= "blue")
# No se aprecia bien la distribución debido a los outliers
#########################################################################################
# 3.- Identificación de valores extremos 
#########################################################################################

library(ggplot2)
plot(prop.table(table(df$edad)))
g = ggplot2::ggplot(df,aes(df$edad))
g+geom_density()

outliers_age = boxplot.stats(df$edad)$out
boxplot(df$edad,boxwex=0.1)

x = df$edad
qnt = quantile(x, probs = c(.25, .75), na.rm = T)
H = 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] = NA
x[x > (qnt[2] + H)] = NA

plot(prop.table(table(x)))
g = ggplot2::ggplot(df,aes(x))
g+geom_density()

df$edad = x

summary(df)

##########################################################################################
# 4.- Imputación de valores faltantes.
##########################################################################################

library(VIM)
par(mfrow=c(1,1))
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=FALSE, 
                  labels=names(df), cex.axis=.7, gap=3,
                  ylab=c("Histograma de valores perdidos","Patron"))
dfNA = subset(df,is.na(edad)==TRUE & is.na(codigo_postal)==TRUE)
# Por medio de regresión múltiple no es posible realizar una predicción mínimamente
# decente de las edades faltantes, pero en 2590 observaciones coincide falta de 
# edad y de codigo_postal por lo que optamos por retirar esas filas.
summary(df)
df11 = subset(df, !is.na(codigo_postal))
summary(df11)
df = df11
rm(df11)

# Para los 308 casos de NAs de edad que nos quedan vamos a aplicar:

# IMPUTACIÓN MULTIPLE (con paquete "mice") ###########################
# Vamos a utilizar el método de Predictive mean matching (PMM)
# 10 iteraciones y 5 imputaciones múltiples
# Solo pueden ser varibles numéricas.
str(df)
df$estacion_desenganche = as.integer(df$estacion_desenganche)
df$estacion_enganche = as.integer(df$estacion_enganche)

library(mice)
temp = mice(df[,-c(2,4,5,6,7)],
            m = 5, maxit = 10,
            method = "pmm", seed = 1234)
summary(temp) # Resumen del proceso

# Eligimos uno de los "m" (5) conjuntos generados:
df41 = complete(temp, 1) # Por ejemplo, el 1. O bien 2, 3, 4 o 5 (a elegir)

# Comparar el data frame original con el elegido imputado:
summary(df)
summary(df41) # Ya no hay NAs, los hemos imputado

par(mfrow = c(2,1))
hist(df$edad,
     main = "Edad (dataset con NA)",
     col = "red"); grid()
hist(df41$edad,
     main = "Edad (dataset sin NA)",
     col = "green"); grid()

# Acotamos el rango de códigos postales para ver los casos más frecuentes
dfTemp = subset(df,df$codigo_postal>=20000 & df$codigo_postal<20020)
dfTemp41 = subset(df41,df41$codigo_postal>=20000 & df41$codigo_postal<20020)
hist(dfTemp$codigo_postal,
     main = "CP (dataset con NA)",
     col = "red"); grid()
hist(dfTemp41$codigo_postal,
     main = "CP (dataset sin NA)",
     col = "green"); grid()

df$edad = df41$edad
df$codigo_postal = df41$codigo_postal
