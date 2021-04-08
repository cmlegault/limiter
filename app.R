#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(cowplot)
library(gridExtra)

avgb <- as.data.frame(read.csv("avgb.csv"))
pointests <- read.csv("pointests.csv") 

df1 <- pivot_longer(pointests, !Year, names_to = "series", values_to = "mt")

df2 <- pointests %>%
  mutate(ERcatch = Catch / AverageB,
         ERquota = Quota / AverageB) %>%
  select(Year, ERcatch, ERquota) %>%
  pivot_longer(!Year, names_to = "series", values_to = "ER")

# Define UI for application 
ui <- navbarPage("TRAC GBYT Limiter",

    # Welcome Panel
    tabPanel("Welcome",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12,
                   h2("Welcome"),
                   br(),
                   p("blah blah blah"),
                   br(),
                   p("Have fun!")
            )
          )
        ),
        
      mainPanel(
        h3("Recent"),
        p("This tab is the original Limiter page. It shows..."),
        h3("Historical"),
        p("This is where you... "),
        h3("Examples"),
        p("This is the fun part???"),
        h3("Technical Details"),
        p("Under the hood secrets..."),
        p("Use p() to set off paragraphs here"),
        p("Code for this R Shiny app is available at ",
          a("https://github.com/cmlegault/limiter", href="https://github.com/cmlegault/limiter", target="_blank")),
        p("Special thanks to TRAC, TMGC, the NEFMC's SSC, and Diane Rielinger for providing feedback on earlier versions of this app.")
      )
      )
    ),


    # Original Panel
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
                        sep = ""),
            
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
    ),
    
    # Historical Panel
    tabPanel("Historical",
      sidebarLayout(
        sidebarPanel(
          sliderInput("hLimits",
                      "Year Limits for MeanVal Calculations:",
                      min = 1935,
                      max = 2020,
                      step = 1,
                      value = c(1973, 1990),
                      sep = "")
          ),
               
        # Show plots and table
        mainPanel(
          plotOutput("hPlot"),
          tableOutput("hTable")
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
    
    output$hPlot <- renderPlot({
      myavgb <- filter(avgb, Year >= input$Year1)
      p1 <- ggplot(filter(df1, series %in% c("Catch", "Quota")), 
                   aes(x=Year, y=mt, color = series)) +
        geom_point(na.rm = TRUE) +
        geom_line(na.rm = TRUE) +
        geom_vline(xintercept = input$hLimits, 
                   linetype = "dashed", color = "purple", lwd = 1.5) +
        xlab("Year") +
        ylab("Metric Tons") +
        theme_bw() +
        theme(legend.position = c(0.1, 0.7))
      
      p2 <- ggplot(filter(df1, series %in% 
                    c("DFO", "NEFSC_Spring", "NEFSC_Fall_lagged", "AverageB")), 
                   aes(x=Year, y=mt, color = series)) +
        geom_point(na.rm = TRUE) +
        geom_line(data = filter(df1, series %in% c("AverageB")), na.rm = TRUE) +
        geom_vline(xintercept = input$hLimits, 
                   linetype = "dashed", color = "purple", lwd = 1.5) +
        xlab("Year") +
        ylab("Metric Tons") +
        theme_bw() +
        theme(legend.position = c(0.1, 0.7))
      
      p3 <- ggplot(df2, aes(x=Year, y=ER, color=series)) +
        geom_line(na.rm = TRUE) +
        geom_point(na.rm = TRUE) +
        geom_vline(xintercept = input$hLimits, 
                   linetype = "dashed", color = "purple", lwd = 1.5) +
        xlab("Year") +
        ylab("Exploitation Rate") +
        theme_bw() +
        theme(legend.position = c(0.1, 0.7))

      # plot_grid from cowplot library preferred to allow vertical alignment of axes, but not supported on Shiny1, so gridExtra::grid.arrange used
      #cowplot::plot_grid(p1, p2, ncol = 1, align = "v")
      gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
    })    

    output$hTable <- renderTable({
      df3 <- filter(df1, Year %in% seq(input$hLimits[1], input$hLimits[2])) %>%
        group_by(series) %>%
        summarize(MeanVal = mean(mt, na.rm = TRUE), .groups = "drop_last")
      df4 <- filter(df2, Year %in% seq(input$hLimits[1], input$hLimits[2])) %>%
        group_by(series) %>%
        summarize(MeanVal = mean(ER, na.rm = TRUE), .groups = "drop_last")
      rbind(df3, df4) %>%
        filter(series %in% c("Catch", "Quota", "AverageB", "ERcatch", "ERquota")) 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
