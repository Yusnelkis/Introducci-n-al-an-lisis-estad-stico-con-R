Análisis Exploratorio con R - curso Doctorado BIBUPO


#Cargar paquetes 

# Instala el paquete ggplot y algunas extensiones útiles
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("ggthemes")
install.packages("tidyverse")
install.packages("ggstatsplot")
install.packages("rstantools")
install.packages("Hmisc")
install.packages("nortest")

# Cargamos librerías

options(scipen = 999)
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(haven)
library(nortest)

#(Recordar que si no ha instalado estos paquetes debe correr primero el comando: install.packages("nombre del paquete"))

#Importar archivos

#Vamos a utilizar los archivos:

##Antes de comenzar, hay que cambiar el directorio de trabajo y seleccionar el folder en donde tenemos nuestros archivos. 
##Esto se hace con el comando setwd().

###Ahora podemos importar los archivos de varias formas. Podemos hacerlo desde el menú de arriba, usando: File -> Import Dataset y seleccionando el tipo de archivo que queremos importar.

## También podemos hacerlo escribiendo el código. Es siempre recomendable asignar el archivo que importamos a un objeto con el símbolo <-. Inicialmente vamos a importar unos archivos .xlsx, por lo que usamos el paquete readxl, que ya instalamos.



#Importar archivos 

EMPLEADOS <- read_sav("C:/Users/Usuario/Downloads/EMPLEADOS.sav")
View(EMPLEADOS)

EMPLEADOS <- as.data.frame(EMPLEADOS)
class(EMPLEADOS)


Europa <- read_sav("C:/Users/Usuario/Downloads/Europa.sav")
View(Europa)

#Comenzamos con la base EMPLEADOS para ver análisis con datos categóricos


#Explorar los datos 

glimpse(EMPLEADOS)

str(EMPLEADOS)
head(EMPLEADOS)
names(EMPLEADOS)
dim(EMPLEADOS)

class(EMPLEADOS$sexo)

EMPLEADOS$sexo = as.factor(EMPLEADOS$sexo)

#Para convertir varias variables a factor

#Las guardamos en un vector
cols <- c("sexo","educ", "catlab")

#Le aplicamos una misma función a todas las columnas con lapply  
EMPLEADOS[cols] <- lapply(EMPLEADOS[cols], factor)

## Análisis univariante 

#vARIABLES CATEGÓRICAS 

#vamos a calcular tablas de frecuencia y esto lo hacemos con el comando table()

#Frecuencias simples

t_sexo = table(EMPLEADOS$sexo)

#Datos porcentuales, frecuencia relativa 

prop.table(t_sexo)


#Visualizar variables cualitativas 

# Gráfico simple de barras de una variable categórica


ggplot(EMPLEADOS)+
  geom_bar(aes(x=sexo))



#vALORES NUMÉRICOS

summary(EMPLEADOS)

##Si queremos calcular los estadísticos para una sola variable ponemos el nombre de la base de datos seguido del signo de $. Por ejemplo: summary(encuesta$ingreso).

###Además podemos hacer uso de funciones para calcular individualmente los estadísticos descriptivos: mean(), median(), min(), max(), IQR() (rango intercuartílico), sd() (desviación estándar).

mean(EMPLEADOS$salario)

#SI QUEREMPOS UN PANORAMA MÁS COMPLETO DE LAS VARIABLES

describe(EMPLEADOS)


plot(EMPLEADOS$salario)

boxplot(EMPLEADOS$salario)


#histograma 

hist(EMPLEADOS$salario)

ggplot(EMPLEADOS)+
  geom_histogram(aes(x=salario)) 

#agregamos densidad con color

ggplot(EMPLEADOS, aes(x=salario)) + geom_density(col="blue") +
  geom_histogram(aes(y=..density..), colour="black", fill=NA)

#eXTRAER EL PARÁMETRO DE UNA VARIABLE CUANTIATIVA EN FUNCIÓN DE UNA CUALITATIVA

sapply(split(x=EMPLEADOS$salario, f=EMPLEADOS$sexo), mean)


#AGREGAR DATOS 

#Si queremos agregar datos por variables categóricas utilizamos paquete dplyr (Muy usado )


EMPLEADOS <- EMPLEADOS %>%
  group_by(catlab) %>%
  mutate(salariopromedio=mean(salario))

View(EMPLEADOS)

# siqueremos agrupar por más de una variable:
##También podemos agrupar por más de una variable, lo único que debemos hacer es separar las variables por coma. 

#R4esumir valores con summarise()

EMPLEADOS %>%
  group_by(catlab) %>%
  summarise(salariopromedio=mean(salario),
            expprevia=mean(expprev))

#si queremos agregar más de un estadístico, solo es separarlos por coma


EMPLEADOS %>%
  group_by(catlab) %>%
  summarise(salariopromedio=mean(salario),
            n(),
            min(salario),
            max(salario))

#Pruebas de normalidad 

##En la literatura estadística se reportan varias pruebas, algunas de ellas se listan a continuación.

## Prueba Shapiro-Wilk con la función shapiro.test. (Con n < 50)
## Prueba Anderson-Darling con la función ad.test del paquete nortest.
## Prueba Cramer-von Mises con la función cvm.test del paquete nortest.
## Prueba Lilliefors (Kolmogorov-Smirnov) con la función lillie.test del paquete nortest.(con n> 50)
## Prueba Pearson chi-square con la función pearson.test del paquete nortest.

shapiro.test(EMPLEADOS$salario) #si la n < 50

lillie.test(EMPLEADOS$salario) #si la n > 50



# PRUEBA DE NORMALIDAD PARA ESTRATOS 

# Creo el subconjunto de datos 

salarios_sexo <- split(EMPLEADOS$salario, EMPLEADOS$sexo)

salarios_sexo

class(salarios_sexo)

lapply(salarios_sexo, shapiro.test)



............................................................

### Análisis bivariantes 


#Tablas de contingencia (Tabulamos dos variables)

table(EMPLEADOS$sexo, EMPLEADOS$catlab)

#Sacamos proporciones (%)

prop.table(table(EMPLEADOS$sexo, EMPLEADOS$catlab))

#Si queremos ver los %  por filas 

prop.table(table(EMPLEADOS$sexo, EMPLEADOS$catlab), 1)

#Si queremos ver los %  por columnas

prop.table(table(EMPLEADOS$sexo, EMPLEADOS$catlab), 2)

#si queremos hacerlo más rápido para sacar todo al mismo tiempo, podemos usar Crosstable() del paquete gmodels()

CrossTable(EMPLEADOS$sexo, EMPLEADOS$catlab)


#SACAR BIEN EL CHI-CUADRADO 

#opcion A

cuadrado = table(EMPLEADOS$sexo, EMPLEADOS$catlab)

cuadrado 

chisq.test(cuadrado)

#opcion B

fisher.test(cuadrado,simulate.p.value=TRUE)


#Visualización exploratoria de variables cualitativas 




..................................................................




#CORRELACIONES 

#Instalar paquetes 

install.packages("GGally")
install.packages("corrplot")
install.packages("PerformanceAnalytics")


#Cargar paquetes
library(GGally)
library(corrplot)
library(PerformanceAnalytics)


#AhorA avamos a traBajar con el fichero EUROPA 

head(Europa)
names(Europa)

cor(Europa$esperanz, Europa$renta)

#extraemos el p valor 

cor.test(Europa$esperanz, Europa$renta)


#Matriz de correlaciones 

round(cor(base),2) #debe ser numericos todos los valores, sino convertir con as.numeric()

#Calcular la matriz con el p valor 

rcorr(as.matrix(base))

#visualizar l amatriz en forma gráfica 

correlacion<-round(cor(Europa), 1)

corrplot(correlacion, method="number", type="upper")

chart.Correlation(base, histogram = F, pch = 19)




