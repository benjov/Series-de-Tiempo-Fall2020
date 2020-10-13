#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Dibuja tu suerte"),
    
    # Sidebar with a slider input for number of bins
    
    
    sidebarLayout(
        sidebarPanel(
            
                sliderInput("Time",
                            "Numero de volados:",
                            min = 100,
                            max = 5000,
                            value = 1000),
            
            sliderInput("N",
                        "Numero de caminos:",
                        min = 1,
                        max = 50,
                        value = 25)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        Soporte <- input$Time
        Moneda <- c(1,-1)
        Caminos <- input$N
        
        # draw the histogram with the specified number of bins
        
        for(i in 1:Caminos){
            TT <- data.matrix(data.frame(Caminata(Moneda, Soporte)[1]))
            #
            G_t <- data.matrix(data.frame(Caminata(Moneda, Soporte)[2]))
            #
        plot(TT, G_t, col = "blue", type = "l", ylab = "Ganancias", xlab = "Tiempo", 
             ylim = c(-150,150), main="Simulacion caminos aleatorios")
            #
            par(new = TRUE)
            #
            i <- i +1
        }
        
        
        #
        
        
    })
    
})


# Run the application 
shinyApp(ui = ui, server = server)
