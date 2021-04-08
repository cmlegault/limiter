#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(dplyr)
library(ggplot2)
#library(cowplot)
library(gridExtra)

avgb <- as.data.frame(read.csv("avgb.csv"))
pointests <- read.csv("pointests.csv")

# Define UI for application 
ui <- navbarPage("TRAC GBYT Limiter",

    # Application title
    tabPanel("Recent",
      sidebarLayout(
        sidebarPanel(
            sliderInput("Limits",
                        "Limits for Average Biomass:",
                        min = 0,
                        max = 15000,
                        step = 100,
                        value = c(600, 5000)),
            
            sliderInput("Year1",
                        "First Year to Show in Plot:",
                        min = 2010,
                        max = 2019,
                        step = 1,
                        value = 2014,
                        sep=""),
            
            sliderInput("BPC",
                        "Blue Percent Line in Lower Plot:",
                        min = 0,
                        max = 100,
                        step = 1,
                        value = 80),
            
            sliderInput("Quota",
                        "Constant Quota (mt):",
                        min = 50,
                        max = 500,
                        step = 10,
                        value = 200)
        ),

        # Show plots and table
        mainPanel(
           plotOutput("myPlot"),
           tableOutput("myTable")
        )
      )
    )
    
) # close navbarPage parens

# Define server logic 
server <- function(input, output) {

    output$myPlot <- renderPlot({
        myavgb <- filter(avgb, Year >= input$Year1)
        p1 <- ggplot(myavgb, aes(x=factor(Year), y=avgb)) +
            geom_violin() +
            geom_hline(yintercept = input$Limits, 
                       linetype = "dashed", color = "red", lwd = 1.5) +
            geom_point(data=filter(pointests, Year >= input$Year1), 
                       aes(x=factor(Year), y=AverageB)) +
            xlab("Year") +
            ylab("Average Survey Biomass (mt)") +
            theme_bw()

        myprop <- avgb %>%
            filter(Year >= input$Year1,
                   avgb >= input$Limits[1],
                   avgb <= input$Limits[2]) %>%
            group_by(Year) %>%
            summarize(PercentIn = n() / 10, .groups="keep")
        p2 <- ggplot(myprop, aes(x=Year, y=PercentIn)) +
            geom_bar(stat = "identity") +
            geom_hline(yintercept = input$BPC,
                       linetype = "dashed", color = "blue", lwd = 1.5) +
            ylab("Percent Within Limits") +
            expand_limits(x = input$Year1) +
            theme_bw()
        
        # plot_grid from cowplot library preferred to allow vertical alignment of axes, but not supported on Shiny1, so gridExtra::grid.arrange used
        #cowplot::plot_grid(p1, p2, ncol = 1, align = "v")
        gridExtra::grid.arrange(p1, p2, ncol = 1)
    })    

    output$myTable <- renderTable({
        data.frame(Limits = input$Limits,
                   Catch6 = round(0.06 * input$Limits, 0),
                   Quota = input$Quota,
                   ExplRate = round(100 * input$Quota / input$Limits, 1)
                   )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
