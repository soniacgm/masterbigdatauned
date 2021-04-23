
# Módulo visualización avanzada

#library(shiny)
#library(ggplot2)

# Cargamos los datos desde el repositorio github del módulox 

con <- url("https://github.com/soniacgm/masterbigdatauned/blob/eae7776570fa78236b64e5e8fd8c40da1f23a80d/datos/D_Covid.Rda?raw=true")
load(con)

dataset <- D_Covid

# distinguimos variables "a nivel de intervalo" ("continuas" para ggplot)
nums <- sapply(dataset, is.numeric)
continuas <- names(dataset)[nums]

# y variables "categóricas" ("discretas" para ggplot)
cats <- sapply(dataset, is.character)
categoricas <- names(dataset)[cats]

shinyUI(
    navbarPage("Incidencia COVID-19 en CyL",
               tabPanel("Descripción del análisis",
                        mainPanel(
                            h5("Autora: Sonia Castro García Muñoz    -  Fecha: 23 de abril de 2021"),
                            h3("Presentación del análisis", align = "center"),
                            p("La visualización permite analizar la incidencia acumulada por quicena de los casos, las hospitalizados, 
                              los ingresados en uci y los fallecidos en Castilla y León"),
                            p("Los datos corresponden al periodo que se extiende desde el 1 de enero de 2020 al 14 de abril de 2021,
                              lo que supone un total de 30 quincenas"),
                            p("El usuario podrá visualizar los valores absolutos de incidencia por provincia, grupo de edad y sexo."),
                            p("Asi mismo, podrá visualizar los datos relativos teniendo en cuenta el tamaño de la población por provincia, sexo y grupo de edad."),
                            p(""),
                            p(""),
                            hr(),
                            h3("Resultados del análisis", align = "center"),
                            h4("Análisis de la evolución de la pandemia en el tiempo"),
                            p("* En el gráfico que aparece por defecto, total de contagiados por quincena, 
                            se aprecian claramente las tres olas que han tenido lugar durante el periodo analizado."),
                            p("* Respecto al número de contagiados, se observa como la tercera ola es la que refleja una 
                              incidencia más elevada, en buena medida por una detección mayor de casos." ),
                            p("* En cuanto al número de hospitalizados y ucis, las tres olas presentan un número de casos 
                              acumulados por quincena similar."),
                            p("* Por el contrario, al número de fallecidos, disminuye de la primera a la segunda ola, y aún más
                              cuando se alcanza la tercera. Esta caída sin duda es debida a un mayor conocimiento de cómo atajar la enfermedad."),
                            p("* El patrón comportamiento de los cuatro parámetros analizados (contagiados, hospitalizados, ucis y fallecidos)
                              es bastante similar en hombres y mujeres, en los distintos tramos de edad y en cada una de las nueve provincias que conforman la región."),
                            hr(),
                            h4("Análisis del comportamiento de la pandemia por provincias"),
                            p("* El análisis de los datos teniendo en cuenta los valores absolutos de incidencia en cada provincia 
                              lleva a confusión. Lo realmente apropiado es analizar la incidencia teniendo en cuenta el tamaño de población 
                              por provincia, sexo y grupo de edad."),
                            p("* A la vista de los totales de contagiados, ucis, hospitalizados y defunciones en cada provincia, 
                              se prodría concluir que la incidencia ha sido mayor en las provincias de Valladolid, Salamanca y León. Cuando se ponderan esos datos
                              en base tamaño de la población por provincia, sexo y grupo de edad, se observa que la provincia más afectada
                              ha sido Segovia en primer lugar y Soria en segundo puesto."),
                            p(" La incidencia de mortalidad relativa mayor en Segovia y Soria se ve también claramente reflejada cuando se compara
                              la tasa de hospitalizados frente a la tasa de fallecidos por provincias, y a ello se superpone una línea de regresión. 
                              Es en estas provincia donde se observa una relación lineal más fuerte.")
                 )),
               tabPanel("Incidencia COVID",
                        sidebarPanel(
                            
                            selectInput('x', 'Elige variable para eje X', continuas, continuas[[1]]),
                            selectInput('y', 'Elige variable para eje Y', continuas, continuas[[2]]),
                            selectInput('color', 'Color', c('None', 'sexo')),
                            
                            checkboxInput('lm', 'Línea de Regresión'),
                            checkboxInput('smooth', 'Suavizado LOESS'),
                            
                            selectInput('facet_row', 'Elige variable para facetas por filas', c(None='.', categoricas))
                        ),
                        
                        mainPanel(
                            plotOutput('plot',
                                       height=500)
                            
                        )
               )
              
    ))

