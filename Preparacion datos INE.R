library(data.table)
library(dplyr)

setwd("c:/Users/sonia/Documents/R/Covid/")

PoblacionINE <- read.csv2("./datos/PoblacionINE.csv", sep=";", header=T, )
ProvinciasISO <- read.csv2("./datos/ISO_Provincias Numero.csv", sep=";", header=T, )
names (PoblacionINE)

# 1.- Quitar columnas
PoblacionINE$AÒo <- NULL
PoblacionINE$EspaÒoles.Extranjeros <- NULL

# 2.- Cambiar N√∫mero Provincias a ISO
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
# Si es 8 o superior lo agrupamos en uno s√≥lo
PoblacionINE$edad_grupo[PoblacionINE$edad_grupo>8] = 8
# Ahora lo convertimos al formato de Data_Covid que es 0-9, 10-19... 80+
PoblacionINE$edad_grupo = paste(as.character(PoblacionINE$edad_grupo*10),"-",as.character(PoblacionINE$edad_grupo*10+9), sep="")
PoblacionINE$edad_grupo[PoblacionINE$edad_grupo == "80-89"] = "80+"

# Hay que pasar el Total a num√©rico, pero quitando los puntos, que los entiende como decimales
PoblacionINE$Total = as.numeric(sub(".", "", PoblacionINE$Total, fixed = TRUE))

# 3.2.- Agrupar por grupo de edad
PoblacionINE_DT <- as.data.table(PoblacionINE)

PoblacionINE_DT <- PoblacionINE_DT %>% group_by(Provincia, ISO, edad_grupo, Sexo) %>% 
  summarise(Total =sum(as.numeric(Total))) 

save(PoblacionINE_DT,file="./datos/PoblacionINE.Rda")

