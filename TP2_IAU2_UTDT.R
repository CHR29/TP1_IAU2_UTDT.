#TP CLASE 1

#A continuacion descargare las librerias necesarias para poder analizar los datos. 

library(tidyverse)
library(tidyr) 
library(readr)
library(vroom)


#A continuacion procedere a importar el Dataset de diferentes maneras para posteriormente organizar y analizar los datos.

Precio.departamentos.venta<- read.csv("Data/precio-venta-deptos.csv", stringsAsFactors = TRUE)

Precio.departamentos.venta<- read_csv("Data/precio-venta-deptos.csv")

Precio.departamentos.venta<- vroom("Data/precio-venta-deptos.csv", delim = ",")

Precio.departamentos.venta

View(Precio.departamentos.venta)


#A continuacion procedere a investigar el Dataset acerca del valor de los inmuebles usados y a estrenar por barrio en CABA.

#Cantidad de observaciones y variables
dim(Precio.departamentos.venta)
#Es posible observar que hay 7 variables y 7296 observaciones.

#Encabezado del Dataset
names(Precio.departamentos.venta)
#Las 7 variables se encuentran divididas por barrio, año, trimestre, precio promedio, ambientes, estado y comuna. 

#Estructura del Dataset. 
str(Precio.departamentos.venta)
#Las variables "barrio", "ambientes" y "estado" son caracteres, mientras que "año", "trimestre", "precio promedio" y "comuna" son numericos. 

#Tipo de datos
class(Precio.departamentos.venta)

#Informacion de las observaciones
head(Precio.departamentos.venta)
#Dentro de las primeras obervaciones es posible observar que dentro de la variable "precio_prom" existen observaciones sin informacion.

#Sintesis del Dataset
summary(Precio.departamentos.venta)
#De esta manera, es posible observar el la minima, media y maxima de cada variable, cuando su estructura es numerica. 

#En este sentido, procederemos a investigar la variable "precio_prom"
Precio.departamentos.venta$precio_prom
#Como mencione previamente, dentro de esta variable es posible encontrar varias observaciones sin datos. 


#A continuacion comenzare a ordenar y tranformar los datos. 

#Comenzare por eliminar la columna "trimestre" dado que no resulta relevante para analizar el precio promedio por barrio segun el año. 
Precio_departamentos_venta <- select(Precio.departamentos.venta, - "trimestre")
#Es posible observar como al haber eliminado la variable trimestre, ahora contamos con 6 variables.

#A continuacion, separare la columna ambientes en dos. 
Precio.departamentos.venta.ambientes <- Precio_departamentos_venta %>% 
  separate(ambientes, c("ambientes", "Nombre.ambientes"), sep= " ")
#Es posible observar como en una columna quedo el numero de ambientes y en otra ambientes. 

#En este sentido, procedere a eliminar la variable "Nombre.ambientes" dado que es irrelevante para dicho analisis.
Precio_departamentos_venta_ambientes<- select(Precio.departamentos.venta.ambientes, - "Nombre.ambientes")
#Es posible observar, como la variable "ambientes" paso a tener unicamente un numero. 

#A continuacion comenzare a filtrar las observaciones que no tienen datos en la variable "precio_prom". 
Precio.departamentos.venta.NA <- Precio_departamentos_venta_ambientes  %>%
  filter(!is.na(precio_prom))
#Es posible observar como se elimino las observaciones con NA.

#A continuacion filtrare las observaciones segun la variable año para tener informacion mas actualizada. 
Precio.departamentos.venta.2017.2019 <- filter (Precio.departamentos.venta.NA, año %in% c("2017", "2018", "2019"))
#Es posible observar la oferta de departamentos de 2017 a 2019.

#A continuacion filtrare las observaciones segun la variable barrio para tener informacion de los barrios aledaños al rio.
Precio.departamentos.venta.barrio <- filter (Precio.departamentos.venta.2017.2019, barrio %in% c("RECOLETA", "PALERMO", "BELGRANO"))
#Es posible observar la oferta de departamentos de en los barrios de Belgrano, Palermo y Recoleta.

#Filtrare el Dataset para obtener los departamentos a estrenar. 
Precio.departamentos.venta.1 <- filter (Precio.departamentos.venta.barrio, estado=="A estrer")
#Es posible observar la oferta de departamentos a estrenar en los barrios de Belgrano, Palermo y Recoleta.

#Por ultimo agruparemos por barrio para saber la oferta de departamentos que hubo en estos años en cada barrio.
Precio.departamentos.venta.2 <- Precio.departamentos.venta.1 %>%
  group_by(barrio) %>% 
  summarise(cantidad=n())
#Es posible observar que entre 2017 y 2019 hubo 20 departamentos a la venta en cada barrio (Belgrano, Palermo y Recolta)

#Conclusion
summary(Precio.departamentos.venta.1) 
#Es posible observar que entre 2017 y 2019 el precio promedio rondo en los 4000 USD el m2, con una minima de 3335 y una maxima de 4785 en los barrios aledaños al rio (Belgrano, Palermo y Recoleta)