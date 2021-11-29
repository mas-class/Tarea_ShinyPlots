
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


shinyServer(function(input, output) {
    
    #Definir Garfic01
    #---------------------------------------------------------------------------
    output$graficas_base_r <- renderPlot({
        plot(mtcars$wt,
             mtcars$mpg,
             xlab = 'wt',
             ylab = 'mpg')
    })
    
    output$grafica_ggplot <- renderPlot({
        diamonds %>%
            ggplot(aes(x=carat,
                       y=price,
                       color=color))+
            geom_point()+
            xlab('price')+
            ylab('kilates')+
            ggtitle('precio de diamante por kilates')
    })
    
    
    selected <- reactiveVal(rep(0, nrow(mtcars)))
    selected2 <- reactiveVal(rep(0, nrow(mtcars)))
    abc <- reactiveVal(rep(0, nrow(mtcars)))
    df_clk <- reactiveVal(data.frame(0,0))
    
    updater <- function(l1, l2) {
        changes <- ifelse(l1 == 0 | l2 != 2, F, T)
        l1[changes] <- l2[changes]
        changes <- ifelse(l1 == 0 | l2 != 0, T, F)
        l1[changes] <- l2[changes]
        return(l1)
    }
    
    
    #Evento: Graficos dinamicos, cambio de color
    #---------------------------------------------------------------------------

    observeEvent(
        input$mbrush, {
            brushed <- brushedPoints(mtcars, input$mbrush, allRows = TRUE
            )$selected_
            brushed2 <- brushedPoints(mtcars, input$mbrush)
            selected(replace((brushed | selected()),
                             (brushed | selected()) == TRUE, 1))
            abc(updater(selected2(), selected()))
            output$mtcars_tbl <- renderDT(brushed2)
        }
    )
    
    observeEvent(
        input$clk, {
            cliked <- nearPoints(mtcars, input$clk, allRows = TRUE
            )$selected_
            cliked2 <- nearPoints(mtcars, input$clk)
            selected((cliked | selected()) * 1)
            abc(updater(selected2(), selected()))
            df_clk(data.frame(input$clk$x,input$clk$y))
            output$mtcars_tbl <- renderDT(cliked2)
        }
    )
    
    observeEvent(
        input$mhover, {
            hovered <- nearPoints(mtcars, input$mhover, allRows = TRUE
            )$selected_
            selected2(replace((hovered | selected2()),
                              (hovered | selected2()) == TRUE, 2))
            abc(updater(selected(), selected2()))
        }
    )
    
    observeEvent(
        input$dclk, {
            selected(rep(0, nrow(mtcars)))
            selected2(rep(0, nrow(mtcars)))
            abc(updater(selected2(), selected()))
            df_clk(data.frame(0,0))
            output$mtcars_tbl <- renderDT(NULL)
        }
    )
    
    
    output$plot_click_option <- renderPlot({
        mtcars$sel <- abc()
        ggplot(mtcars, aes(wt, mpg)) +
            geom_point(aes(color = as.factor(sel))) +
            scale_color_manual(
                values = c("0" = "blue", "1" = "green", "2" = "gray"))
    }, res = 96)

    
    output$click_data <- renderPrint({
        list(click_xy = c(input$clk$x, input$clk$y),
            doble_click_xy = c(input$dclk$x, input$dclk$y),
            hover_xy = c(input$mhover$x, input$mhover$y),
            brush_xy = c(input$mbrush$xmin, input$mbrush$ymin,
                         input$mbrush$xmax, input$mbrush$ymax)
            )
    })
    
    
    output$mtcars_tbl <- renderTable({
        df <- nearPoints(mtcars, input$clk, xvar = "wt", yvar = "mpg",
                         threshold = 10, maxpoints = 1)
        df
    })
    
    
    #print(a)
    #a <- c(1, 0, 0, 1, 0)
    #b <- c(0, 0, 2, 2, 2)
    #updater(a, b)
    

    #Evento: hacer click o brush y mostrar la informacion del punto en la tabla
    #---------------------------------------------------------------------------
    output$plot2_tabla <- renderPlot({
        plot(mtcars$wt, mtcars$mpg, 
             main = "Hacer Click o Brush y Mostrar Informacion")
    }, res = 96)
    
    output$tabl02 <- renderTable({
        nearPoints(mtcars, input$clk, xvar = "wt", yvar = "mpg")
    })
    

    output$plot_tabla <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) + 
            geom_point()+
            ggtitle('Hacer Click o Brush y Mostrar Informacion')
        }, res = 96)
    
    output$tabla <- renderTable({
        brushedPoints(mtcars, input$mbrush)
    })
    
    
    #Evento: cambio de color cuando se selecciona y detalla la informacion
    #---------------------------------------------------------------------------
    #selected <- reactiveVal(rep(FALSE, nrow(mtcars)))

    #observeEvent(input$mbrush, {
    #    brushed <- brushedPoints(mtcars, input$mbrush, allRows = TRUE)$selected_
    #    selected(brushed | selected())
    #})
    
    #observeEvent(input$dclk, {
    #    selected(rep(FALSE, nrow(mtcars)))
    #})
    
    #output$plot <- renderPlot({
    #    mtcars$sel <- selected()
    #    ggplot(mtcars, aes(wt, mpg)) + 
    #        geom_point(aes(colour = sel)) +
    #        scale_colour_discrete(limits = c("TRUE", "FALSE"))
    #}, res = 96)
    

})






    