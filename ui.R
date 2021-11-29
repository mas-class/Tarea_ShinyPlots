
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


shinyUI(fluidPage(
    
    titlePanel("Interacciones del usuario con graficas"),
    tabsetPanel(
        tabPanel('Graficas Shiny',
                 h1('graficas en shiny'),
                 plotOutput('graficas_base_r'),
                 plotOutput('grafica_ggplot')
        ),
        
        
        #Mostrar Tabla
        tabPanel('Mostrar datos en Tabla',
                 plotOutput("plot_tabla",
                            click = "clk",
                            brush = "mbrush"),
                 verbatimTextOutput('plot_click'),
                 tableOutput('tabla')
        ),
        
        
        tabPanel('Graficas Dinamico',
                 plotOutput("plot_click_option",
                            click = "clk",
                            dblclick = "dclk",
                            brush = "mbrush",
                            hover = "mhover"),
                 verbatimTextOutput('click_data'),
                 tableOutput('mtcars_tbl')
        ),

        
        #Prueba
        tabPanel('Graficas-Tabla Dinamico',
                 plotOutput("plot2_tabla",
                            click = "clk",
                            dblclick = "dclk",
                            brush = "mbrush",
                            hover = "mhover"),
                 verbatimTextOutput('plot2_data'),
                 tableOutput('tabl02')
        )
        
    )
))


