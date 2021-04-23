# origen fichero datos: 
# https://cnecovid.isciii.es/covid19/resources/metadata_diag_ccaa_decl_prov_edad_sexo.pdf
# https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=03002.px&L=0

##Analizamos la evolución de la pandemia por quincena, provincia, sexo y grupo de edad 
##en las provincias de CyL en términos de casos, hospitalizados, ucis y defunciones
##Se compara la evolución de los datos totales versus los datos teniendo en cuenta la tasa de población

load("https://github.com/soniacgm/masterbigdatauned/blob/eae7776570fa78236b64e5e8fd8c40da1f23a80d/datos/D_Covid.Rda?raw=true")


#importamos ficheros con los que vamos a trabajar
Data_Covid <- read.csv2("https://github.com/soniacgm/masterbigdatauned/blob/main/datos/casos_hosp_uci_def_sexo_edad_provres.csv?raw=true", sep=",", header=T, )
PoblacionINE <- read.csv2("https://github.com/soniacgm/masterbigdatauned/raw/main/datos/PoblacionINE.csv", sep=";", header=T, )
ProvinciasISO <- read.csv2("https://github.com/soniacgm/masterbigdatauned/raw/main/datos/ISO_Provincias%20Numero.csv", sep=";", header=T, )
names (PoblacionINE)

#cargamos las librerías que vamos a utilizar
library(data.table)
library(dplyr)
library (lubridate)
library (tidyverse)
library(data.table)
library(ggplot2)
library(shiny)

# ---- AJUSTAMOS EL FICHERO DEL INE----------

# 1.- Quitar columnas
PoblacionINE$Año <- NULL
PoblacionINE$Españoles.Extranjeros <- NULL

# 2.- Cambiar Número Provincias a ISO
PoblacionINE$num_provincia <- as.numeric(substr(PoblacionINE$Provincias,0,2))

PoblacionINE = merge(x = PoblacionINE, y = ProvinciasISO, by = "num_provincia", all = TRUE)
PoblacionINE$num_provincia <- NULL
PoblacionINE$Provincias <- NULL

# 3.- Unir quinquenal en decenal
# 3.1.- Obtener grupo de edad
PoblacionINE$edad_inicio = str_split(PoblacionINE$Edad..grupos.quinquenales., "-", n=2)
PoblacionINE$edad_inicio = substring(PoblacionINE$Edad..grupos.quinquenales., 1, regexpr("-", PoblacionINE$Edad..grupos.quinquenales.)-1)
PoblacionINE$edad_inicio[PoblacionINE$edad_inicio == ""] = 100 # Para los de +100

# Dividimos entre 10 para obtener el grupo de edad
PoblacionINE$edad_grupo = trunc(as.numeric(PoblacionINE$edad_inicio)/10)
# Si es 8 o superior lo agrupamos en uno sólo
PoblacionINE$edad_grupo[PoblacionINE$edad_grupo>8] = 8
# Ahora lo convertimos al formato de Data_Covid que es 0-9, 10-19... 80+
PoblacionINE$edad_grupo = paste(as.character(PoblacionINE$edad_grupo*10),"-",as.character(PoblacionINE$edad_grupo*10+9), sep="")
PoblacionINE$edad_grupo[PoblacionINE$edad_grupo == "80-89"] = "80+"

# Hay que pasar el Total a numérico, pero quitando los puntos, que los entiende como decimales
PoblacionINE$Total = as.numeric(sub(".", "", PoblacionINE$Total, fixed = TRUE))

# 3.2.- Agrupar por grupo de edad
PoblacionINE_DT <- as.data.table(PoblacionINE)

PoblacionINE_DT <- PoblacionINE_DT %>% group_by(Provincia, ISO, edad_grupo, Sexo) %>% 
  summarise(Total =sum(as.numeric(Total))) 

#save(PoblacionINE_DT,file="./datos/PoblacionINE.Rda")


#-------- AJUSTAMOS EL FICHERO DEL ISCII ----------


# 1. Creamos un subconjunto con los datos de Castilla y León

Data_Covid_CyL <- Data_Covid[Data_Covid$provincia_iso=="AV" | 
                               Data_Covid$provincia_iso=="BU" | 
                               Data_Covid$provincia_iso=="LE" | 
                               Data_Covid$provincia_iso=="P" | 
                               Data_Covid$provincia_iso=="SA" |
                               Data_Covid$provincia_iso=="SG" | 
                               Data_Covid$provincia_iso=="SO" | 
                               Data_Covid$provincia_iso=="ZA" | 
                               Data_Covid$provincia_iso=="VA"  
                             ,]

summary(Data_Covid_CyL)

# 2.Generamos variables nuevas en el fichero actual: mes, quincena y año. 
Data_Covid_CyL <- Data_Covid_CyL[,] %>%
  mutate (año = year(fecha),
          mes = month(fecha),
          mes_año = paste (año, "-", month(fecha)),
          cday = day(fecha),
          quincena = trunc(cday/16) + 1,
          quincena_año= paste(mes_año, "-", quincena)
  )

# 3. Incluimos una columna con el número de quincena para todo el periodo (2020 y 2021)

Data_Covid_CyL$c_quincena = (Data_Covid_CyL$mes-1)*2+Data_Covid_CyL$quincena+trunc(Data_Covid_CyL$año/2021)*24



# 5. Eliminamos de los dos ficheros los que tienen sexo "NC" y los que no tienen asignada provincia, quincena o edad

Data_Covid_CyL<- Data_Covid_CyL[Data_Covid_CyL$sexo!="NC",]

Data_Covid_CyL <- Data_Covid_CyL[!is.na(Data_Covid_CyL$provincia_iso),]

Data_Covid_CyL <- Data_Covid_CyL[!is.na(Data_Covid_CyL$c_quincena),]

Data_Covid_CyL<- Data_Covid_CyL[!is.na(Data_Covid_CyL$grupo_edad),]

summary(Data_Covid_CyL)

#  Pasamos a numeric la quincena 

Data_Covid_CyL$c_quincena <- as.numeric(Data_Covid_CyL$c_quincena) 


# 4. Creamos el fichero de datos acumulados por provincia, sexo, grupo de edad y quincena
Data_Covid_CyL_A <- Data_Covid_CyL %>%
group_by(provincia_iso,sexo, grupo_edad, c_quincena) %>%
  summarise(total_contagiados = sum(num_casos),
            total_hospitalizados = sum(num_hosp),
            total_uci = sum(num_uci),
            total_fallecidos = sum(num_def))

summary(Data_Covid_CyL_A)

# 6. Pasamos a factor las variables: provincia, sexo, grupo de edad y quincena en cada fichero
Data_Covid_CyL_A$provincia_iso <- as.factor(Data_Covid_CyL_A$provincia_iso)
Data_Covid_CyL_A$sexo <- as.factor(Data_Covid_CyL_A$sexo)
Data_Covid_CyL_A$grupo_edad <- as.factor(Data_Covid_CyL_A$grupo_edad)
Data_Covid_CyL_A$c_quincena <- as.factor(Data_Covid_CyL_A$c_quincena)


# 7. Salvamos el fichero

#save(Data_Covid_CyL_A, file="./datos/Data_Covid_CyL_A.Rda")

##-------------UNIMOS LOS FICHEROS Y NOMBRAMOS COMO FICHERO DE TRABAJO "DATA"


Data_Covid_CyL_A[Data_Covid_CyL_A$grupo_edad == "NC",]
Data_Covid_CyL_A <- Data_Covid_CyL_A[!Data_Covid_CyL_A$grupo_edad == "NC",]
Data_Covid_CyL_A <- Data_Covid_CyL_A[!Data_Covid_CyL_A$sexo == "NC",]

PoblacionINE_DT$Sexo[PoblacionINE_DT$Sexo == "Hombres"] <- "H"
PoblacionINE_DT$Sexo[PoblacionINE_DT$Sexo == "Mujeres"] <- "M"

Data_Covid_CyL_A_Pob <- left_join(Data_Covid_CyL_A, PoblacionINE_DT, by = c("provincia_iso" = "ISO", "sexo" = "Sexo", "grupo_edad" = "edad_grupo"))

#save(Data_Covid_CyL_A_Pob, file="./datos/Data_Covid_CyL_A_Pob.Rda")

D_Covid<- Data_Covid_CyL_A_Pob

# ---------- CALCULAMOS LAS TASAS por provincia, grupo de edad y sexo

D_Covid$tasa_contagiados <- D_Covid$total_contagiados/D_Covid$Total
D_Covid$tasa_hospitalizados <- D_Covid$total_hospitalizados/D_Covid$Total
D_Covid$tasa_uci <- D_Covid$total_uci/D_Covid$Total
D_Covid$tasa_fallecidos <- D_Covid$total_fallecidos/D_Covid$Total

names(D_Covid)[10] = "poblacion_total"

save(D_Covid, file="./datos/D_Covid.Rda")


