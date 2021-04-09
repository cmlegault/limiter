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
                   p("This app provides survey, catch, and quota data for Georges Bank yellowtail flounder in a way that allows exploration of different management approaches. It does not conduct any simulations or forecasts of what will happen in the future. Instead, it allows the user to examine how the stock has behaved in the past. This tab provides a short description of what can be found on the other tabs along with some technical details."),
                   br(),
                   p("Have fun!")
            )
          )
        ),
        
      mainPanel(
        h3("Recent"),
        p("This tab is the original TRAC GBYT Limiter. Conceptually, the idea is to find a set of limits for the average survey biomass that would allow a constant quota to be applied. The current Empirical Approach changes the quota each year in response to changes in the average survey biomass, but the catch has been well below the quota in recent years and the stock is at low abundance. This part of the app was developed during the 2020 TRAC meeting and recommended for future management use."),
        p("The user moves sliders to change 1) the limits for the average survey biomass, 2) how many years to show in the plots, 3) a visual guide for the percent within plot, and 4) the constant quota. These values are reflected in the plots and table. The top panel shows the distributions of the average survey biomass by year with the limits as horizontal red lines. The next panel shows the percentage of each annual distribution that falls within the limits selected. The horizontal blue line is just a visual aid to ease comparison of percentages across years. The bottom table shows for the two limits the catch associated with the current 6% exploitation rate associated with the Empirical Approach (Catch6) as well as the exploitation rate (ExplRate) associated with the quota being fully caught if the average survey biomass was exactly at either of the limits. These exploitation rates can be used as guides to see if the limits are too narrow or too wide."),
        h3("Historical"),
        p("This tab shows the history of this stock and fishery. The user moves the sliders to select a range of years within which (including the end points) the mean value of the catch, quota, average survey biomass, and exploitation rates associated with the catch and quota are computed. The top panel shows the catch and TMGC quota. The middle panel shows the three surveys and the average of the available surveys in that year (note the surveys begin in 1987, 1968, and 1964 for the DFO, NEFSC Spring and NEFSC Fall lagged, respectively). The table at the bottom shows the means with any missing information not included in the mean calculation. In this table, NA stands for Not Available, meaning there was no information for any of the years selected for that variable."),
        h3("Examples"),
        p("The following examples are for demonstration purposes only. They are included to show how this app could be used to help decide on limits and quota, and potentially on reference points as well."),
        h3("Technical Details"),
        p("The Historical tab uses a non-standard approach to create the time series for the two NEFSC surveys. Specifically, it converts the Albatross catch/tow to Bigelow catch/tow and then expands this amount as if the Bigelow had been used in these years. This is not standard practice due to the potential bias introduced due to Albatross tows catching zero fish when the Bigelow would have caught fish (a zero catch of yellowtail by the Albatross gets converted to a zero catch by the Bigelow.) In years of high abundance, there is probably not much bias introduced, because many of the Albatross tows caught yellowtail, but the bias could be noticable in years of low abundance. The magnitude of this bias cannot be easily estimated and all the Albatross years (2008 and prior) are assumed equivalent to the Bigelow years in these plots."),
        p("The violin plots in the Recent tab are based on 1,000 values coming from each of the surveys in a given year. The survey CV values are used to create lognormal standard deviations using the formula sd = sqrt(log(1 + CV*CV)). These survey and year specific sd values are then used to generate random values from a lognormal distribution with mean one. The random deviates then multiply the survey kg/tow to generate uncertainty in the survey observations. This approach assumes independence of the random deviations among the surveys within a year and that the only source of uncertainty in the calculation of the average survey biomass comes from the stratified mean estimates of the surveys. Additional sources of uncertainty, such as variability in the survey q, area swept by a tow, and survey area are not included in this variability."),
        p("There are a number of caveats associated with this approach. Past performance does not ensure future benefits. The relationships that have been observed in the past may not adequately predict what happens in the future, especially since there is no mechanistic model underlying these values. For example, large changes in fishery selectivity could create changes in the relationship between survey catch and fishery catch. This approach, along with the Empirical Approach, assumes that at least one survey is available, but is expected to perform best when all three surveys are available. Similarly, changes in how the survey observations of catch/tow are expanded to survey biomass would require recalculations for both this approach and the Empirical Approach."),
        p("Code for this R Shiny app is available at ",
          a("https://github.com/cmlegault/limiter", href="https://github.com/cmlegault/limiter", target="_blank")),
        p("Special thanks to TRAC, TMGC, and the NEFMC's SSC for providing feedback on earlier versions of this app.")
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
    ),

    # Examples Panel
    tabPanel("Examples",
      sidebarLayout(
        sidebarPanel(
          radioButtons("example",
                       "Scenario",
                       choices = c("Empirical", "Initial", "Limits First", "Catch First", "Ref Points"),
                       selected = "Initial")
               ),
               
               # Show plots and table
               mainPanel(
                 plotOutput("exPlot"),
                 textOutput("exText")
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

    output$exPlot <- renderPlot({
      exavgb <- seq(1, 15000, 10)
      
      if (input$example == "Empirical"){
        exlimits <- NA
        exdf <- data.frame(exavgb = exavgb, 
                           exsetquota = exavgb * 0.06) %>%
          mutate(exer = exsetquota / exavgb)
      }
      
      if (input$example == "Initial"){
        exlimits <- c(600, 5000)
        exquota <- 200
        exdf <- data.frame(exavgb = exavgb) %>%
          mutate(exsetquota = ifelse((exavgb < exlimits[1]) |
                                       (exavgb > exlimits[2]), 
                                     exavgb * 0.06, exquota)) %>%
          mutate(exer = exsetquota / exavgb)
      }

      p1 <- ggplot(exdf, aes(x=exavgb, y=exsetquota)) +
        geom_line(lwd = 1.5) +
        {if (!all(is.na(exlimits))) geom_vline(xintercept = exlimits, linetype = "dashed", color = "red", lwd = 1.5)} +
        xlab("Average Survey Biomass (mt)") +
        ylab("Quota (mt)") +
        theme_bw()
      
      p2 <- ggplot(exdf, aes(x=exavgb, y=exer)) +
        geom_line(lwd = 1.5) +
        {if (!all(is.na(exlimits))) geom_vline(xintercept = exlimits, linetype = "dashed", color = "red", lwd = 1.5)} +
        xlab("Average Survey Biomass (mt)") +
        ylab("Exploitation Rate") +
        theme_bw()
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    })    
    
    output$exText <- renderText({
      if (input$example == "Empirical"){
        "The Empirical Approach does not contain any limits (vertical red lines). In this example, the plots use the most recent value of 6% for the exploitation rate regardless of the average survey biomass. This results in a linear relationship between the quota and the average survey biomass (with slope 0.06). In every recent assessment, time has been spent discussing whether 6% is the appropriate exploitation rate to use, and these discussions would be expected to occur in future assessments. There is also no pre-set value that would indicate the stock has increased or decreased sufficiently to justify a different exploitation rate. So every assessment will have this discussion, with the outcome depending to some extent on who is in the room."
      }
      
      else if (input$example == "Initial"){
        "Initial text here"
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
