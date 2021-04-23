library(shiny)
library(ggplot2)

# Cargamos los datos desde el repositorio github del módulo 

con <- url("https://github.com/soniacgm/masterbigdatauned/blob/eae7776570fa78236b64e5e8fd8c40da1f23a80d/datos/D_Covid.Rda?raw=true")

load(con)
dataset <- D_Covid

shinyServer(function(input, output) {
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset, 
                    aes_string(x=input$x, y=input$y)) + geom_point() 
        
        
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        
        facets <- paste(input$facet_row, "~ .")
        if (facets != '. ~ .')
            p <- p + facet_grid(facets)
        
        if (input$lm)
            p <- p + geom_smooth(method='lm',formula=y~x, na.rm = T)
        if (input$smooth)
            p <- p + geom_smooth(method='loess',formula=y~x, na.rm = T)
        
        print(p)
        
    })
    
})
