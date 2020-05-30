#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(datasets)
library(shinydashboard)

data(iris)
summary(iris)


ui <- dashboardPage(
    dashboardHeader(title = "Assignment 7"),
    dashboardSidebar(),
    dashboardBody(fluidPage(
        fluidRow(
            titlePanel("Assignment 7"),
            column(width = 4, class = "well",
                   h4("Brush and double-click to zoom"),
                   plotOutput("plot1", height = 300,
                              dblclick = "plot1_dblclick",
                              brush = brushOpts(
                                  id = "plot1_brush",
                                  resetOnNew = TRUE
                              )
                   )
            ),
            column(width = 8, class = "well",
                   h4("Left plot controls right plot"),
                   fluidRow(
                       column(width = 6,
                              plotOutput("plot2", height = 300,
                                         brush = brushOpts(
                                             id = "plot2_brush",
                                             resetOnNew = TRUE
                                         )
                              )
                       ),
                       column(width = 6,
                              plotOutput("plot3", height = 300)
                       )
                   )
            )
            
        )
    ))
)


server <- function(input, output) {
    
    # -------------------------------------------------------------------
    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- renderPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
            geom_point(color = "blue") +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) 
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # -------------------------------------------------------------------
    # Linked plots (middle and right)
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    output$plot2 <- renderPlot({
        ggplot(iris, aes(Petal.Length, Petal.Width)) +
            geom_point(color = "blue")
    })
    
    output$plot3 <- renderPlot({
        ggplot(iris, aes(Petal.Length, Petal.Width)) +
            geom_point(color = "blue") +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observe({
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
}

shinyApp(ui, server)
