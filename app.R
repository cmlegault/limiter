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
                   p("This app provides survey, catch, and quota data for Georges Bank yellowtail flounder in a way that allows exploration of different management approaches. It does not conduct any simulations or forecasts of what will happen in the future. Instead, it allows the user to examine how the stock has behaved in the past. This tab provides a short description of what can be found on the other tabs along with some technical details. The other tabs can be selected by clicking on their names at the top of the screen. A slider will appear at the right if there is more information than can be shown on the screen."),
                   br(),
                   p("Have fun!")
            )
          )
        ),
        
      mainPanel(
        h3("Recent"),
        p("This tab is the original TRAC GBYT Limiter. Conceptually, the idea is to find a set of limits for the average survey biomass that would allow a constant quota to be applied. The current Empirical Approach changes the quota each year in response to changes in the average survey biomass, but the catch has been well below the quota in recent years and the stock is at low abundance. This part of the app was developed during the 2020 TRAC meeting and recommended for future management use."),
        p("The user moves sliders to change 1) the limits for the average survey biomass, 2) how many years to show in the plots, 3) a visual guide for the percent of average survey biomass that is between the two selected limits, and 4) the constant quota. These values are reflected in the plots and table. The top panel shows the distributions of the average survey biomass by year with the limits as horizontal red lines. The next panel shows the percentage of each annual average survey biomass distribution that falls within the limits selected. The horizontal blue line is just a visual aid to ease comparison of percentages across years. The bottom table shows for the two average survey biomass limits the catch associated with the current 6% exploitation rate associated with the Empirical Approach (Catch6) as well as the exploitation rate (ExplRate) associated with the quota being fully caught if the average survey biomass was exactly at either of the limits. These exploitation rates can be used as guides to see if the average survey biomass limits are too narrow or too wide for the selected quota."),
        h3("Historical"),
        p("This tab shows the history of this stock and fishery. The user moves the sliders to select a range of years within which (including the end points) the mean value of the catch, quota, average survey biomass, and exploitation rates associated with the catch and quota are computed. The top panel shows the catch and TMGC quota. The middle panel shows the three surveys and the average of the available surveys in that year (note the surveys begin in 1987, 1968, and 1964 for the DFO, NEFSC Spring and NEFSC Fall lagged, respectively). The table at the bottom shows the means with any missing information not included in the mean calculation. In this table, NA stands for Not Available, meaning there was no information for any of the years selected for that variable."),
        h3("Examples"),
        p("The examples shown are for demonstration purposes only. They are included to show how this app could be used to help decide on average survey biomass limits and quota, and potentially on reference points as well. There are numerous combinations of approaches and other approaches that could be used to set average survey biomass limits, quota, and potentially reference points."),
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
                        "Limits for Average Survey Biomass:",
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
                       choices = c("Empirical", "Initial", "Limits First", "Quota First", "Ref Points", "Recent Tab Values"),
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
            ylab("Percent of Average Survey Biomass \nBetween Selected Limits") +
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

      if (input$example == "Limits First"){
        exlimits <- c(900, 8500)
        exquota <- 300
        exdf <- data.frame(exavgb = exavgb) %>%
          mutate(exsetquota = ifelse((exavgb < exlimits[1]) |
                                       (exavgb > exlimits[2]), 
                                     exavgb * 0.06, exquota)) %>%
          mutate(exer = exsetquota / exavgb)
      }
      
      if (input$example == "Quota First"){
        exlimits <- c(800, 2600)
        exquota <- 160
        exdf <- data.frame(exavgb = exavgb) %>%
          mutate(exsetquota = ifelse((exavgb < exlimits[1]) |
                                       (exavgb > exlimits[2]), 
                                     exavgb * 0.06, exquota)) %>%
          mutate(exer = exsetquota / exavgb)
      }

      if (input$example == "Ref Points"){
        exlimits <- NA
        exquota <- NA
        exdf <- data.frame(exavgb = exavgb) %>%
          mutate(exer = ifelse((exavgb > 2600),
                               (0.06 + (exavgb-2600) * (0.275-0.06) / (63900-2600)), 0.06)) %>%
          mutate(exsetquota = exer * exavgb)
      }
      
      if (input$example == "Recent Tab Values"){
        exlimits <- input$Limits
        exquota <- input$Quota
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
        expand_limits(y = 0) +
        theme_bw()
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    })    
    
    output$exText <- renderText({
      if (input$example == "Empirical"){
        "The Empirical Approach does not contain any average survey biomass limits (vertical red lines). In this example, the plots use the most recent value of 6% for the exploitation rate regardless of the average survey biomass. This results in a linear relationship between the quota and the average survey biomass (with slope 0.06). In every recent assessment, time has been spent discussing whether 6% is the appropriate exploitation rate to use, and these discussions would be expected to occur in future assessments. There is also no pre-set value that would indicate the stock has increased or decreased sufficiently to justify a different exploitation rate. So every assessment will have this discussion, with the outcome depending to some extent on who is in the room."
      }
      
      else if (input$example == "Initial"){
        "These plots reflect the initial settings when the Shiny app is opened. The average survey biomass limits are set at 600 and 5,000 mt (red vertical lines) and the constant quota is 200 mt. In this example, the Empirical Approach exploitation rate of 6% is assumed to apply outside the average survey biomass limits. This combination of average survey biomass limits and quota results in a declining exploitation rate as the average survey biomass increases between the average survey biomass limits. At the lower limit of 600 mt, the exploiation rate is 33.3%, which may be considered too high by some. At the upper limit of 5,000 mt, the exploitation rate is 4%, which is below the current Empirical Approach rate. There are discontinuities in both catch and exploitation rate as the average survey biomass crosses both average survey biomass limits."
      }
      
      else if(input$example == "Limits First"){
        "One way to use this tool is to focus on the average survey biomass limits first. For example, if at least 80% of the distributions of average survey biomass are desired to be within the average survey biomass limits each year since 2014, then the average survey biomass limits could be set at 900 and 8,500 mt. The next step could be to select a quota that balances the exploitation rate within the average survey biomass limits, for example 300 mt (as shown above). This results in exploitation rates of 33.3% and 3.5% at the average survey biomass limits. Alternatively, a maximum exploiation rate could be used based on the lower limit, or a minimum exploitation rate based on the upper limit. This example again shows the Empirical Approach exploitation rate of 6% outside the average survey biomass limits."
      }
      
      else if(input$example == "Quota First"){
        "Another way to use this tool is to set the quota first. In this example, a quota of 150 mt was selected based on recent quotas and the needs of the fishery. Once the quota is set, the lower limit was found by not allowing the exploitation rate to exceed 20%, resulting in the lower limit of 800 mt. The upper limit was set to remove the discontinuities in the catch and exploitation plots when the Empirical Approach was applied outside the average survey biomass limits. This resulted in the calculated value of 2,666.67 mt, which was rounded down to 2,600 mt."
      }
      
      else if(input$example == "Ref Points"){
        "This tool can also be used in conjunction with setting reference points. The current fishing mortality reference level (Fref) is 0.25, based on F0.1 and F40% from a VPA which assumed an M of 0.2. This reference point cannot be used directly anymore, but could be converted to an exploitation rate using the equation ER = F(1-exp(-Z))/Z, which results in 0.20. Alternatively, the historical tab can be used to look for a period when catch and biomass were thought to be in a level near MSY or Bmsy. For example, if the years 1967-1976 were thought to be appropriate, the averages during this period could be used as reference points, producing Bref = 63,900 mt, Catchref = 17,600 mt, and ERref = 27.5%. The ER reference points could then be used to limit the exploitation rate in any of the other approaches. The Bref could be used to determine when to change from a reduced exploitation rate to ERref, perhaps gradually as the average survey biomass increased. The example plotted has the exploitation rate increase linearly from 6% at 2,600 mt to 27.5% at 63,900 mt. Note the large change in the y-axis scale for the quota plot."
      }
      
      else if(input$example == "Recent Tab Values"){
        paste0("These plots show the values currently selected in the Recent tab: average survey biomass limits of ", input$Limits[1], " and ", input$Limits[2], " mt and quota of ", input$Quota, " mt. The current Empirical Approach exploitation rate of 6% is assumed to apply when the average survey biomass is outside the selected limits in these plots, but different decisions could be made in these situations. Changes to the Limits for Average Survey Biomass or Constant Quota (mt) sliders in the Recent tab are reflected in these plots. Changes to the other two sliders in the Recent tab (First Year to Show in Plot and Blue Percent Line in Lower Plot) have no effect on the plots shown here.")
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
