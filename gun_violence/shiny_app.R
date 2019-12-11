
# Reading in libraries

library(shiny)
library(gganimate)
library(gapminder)
library(ggplot2)
library(shinythemes)
library(shinythemes)
library(coefplot)
library(gt)
library(plotly)
library(scales)
library(tidyverse)
library(htmltools)
library(vembedr)

# Reading in rds file with my final data

final_gun_violence_data <- read_rds("final data copy/final_data.rds")

state_policy <- read_rds("final data copy/state_policy.rds")

policy_and_checks <- read_rds("final data copy/policy_and_checks.rds")

# After I had already read in my rds file with my final data, I decided that I
# didn't want to include the data for 2018 because I only have data for the
# first 3 months of 2018. Therefore, I filtered to exclude 2018 from my 
# "new_data" dataset. I also tried to use fct_relevel to get 2013 first
# in my drop down selection but was not successful. 

new_data <- final_gun_violence_data %>%
  filter(year != 2018) %>%
  mutate(year = fct_relevel(year, "2013", "2014", "2015", "2016", "2017"))

# I also filtered by final data for 2013 and 2014 for my state policy graphs 
# as the state policy dataset only includes which states required gun 
# registration in 2013 and 2014. 

new_data2 <- final_gun_violence_data %>%
  filter(year %in% c(2013, 2014))

# Here, I'm defining my ui. First, I use the fluidPage function to define my
# shinytheme as "flatly." I also add the navbarPage function to add a title
# to my shiny app.



ui <- fluidPage(theme = shinytheme("flatly"),
          
                navbarPage("More Permits More Problems? Tracing Factors Correlated to Gun Violence",
                        
                           # I'm adding my tabPanel called "2013-2017 Gun Violence in March" and calling the 
                           # selectInput I defined in the server for the "chosenyear" variable. I ask the 
                           # user to select the year and use the unique function to get a year from my dataset. 
                           
                           tabPanel("2013-2017 Gun Violence in March",
                                    
                                    selectInput("chosenyear",
                                                "Select Year:", unique(new_data$year)),
                                    
                                    # I added br() to put in a page break. 
                                    # h3 and h4 define the font size for my text in quotes. Remember that as the number
                                    # gets smaller, the text gets larger. 
                                    
                                    br(),
                                    h3("How do the number of gun permits vary by month each year 2013-2017?"),
                                    h4("Do the number of gun permits sold increase in the summer months?"),
                                    br(),
                                    
                                    # I then use plotOuput to print my graph called "permiteighteen" as well as 
                                    # my plot called "permitpermontheighteen."
                                    
                                    plotOutput("permitpermontheighteen"),
                                    
                                    # Adding more page breaks and text to emphasize important information.
                                    
                                    br(),
                                    h3("The highest number of permits granted in 2014, 2016, and 2017 were in the month of March."),
                                    h4("In March of each year 2013-2017, which 10 states granted the most permits?"),
                                    br(),
                                    plotOutput("permiteighteen")),
                           
                           
                           # Next, I define my next tabPanel as "More Permits = More Gun Violence?"
                           # I wanted to emphasize certain important information points with page breaks and
                           # text in different sizes. 
                           
                           tabPanel("Which states granted the most permits?",
                                    br(),
                                    h3("Which month granted the most permits per year 2013-2017?"),
                                    h4("In the month that granted the most permits, what were the ten states that granted the most permits."),
                                    br(),
                                    
                                    # My slider input defines the values I want on my slider, in this case values 
                                    # 2013-2017 to represent the years I have data for. I set animate equal to TRUE 
                                    # in order to get a button where I can press play and Shiny will automatically 
                                    # run through the years 2013-2017 and the corresponding graphs. 
                                    
                                    sliderInput("currentyear",
                                                "Year", min = 2013, max = 2017, value = 500, animate = TRUE),
                                    
                                    # I use the imageOutput function in order to call the graphs that I define in 
                                    # the server as graph and graph 2.
                                    # I once again provided descriptive information about my graphs.
                                    
                                    imageOutput("graph"),
                                    imageOutput("graph2"),
                                    h5("In 2014, 2016, and 2017, the highest number of permits were granted in March. Therefore, the number of guns would increase going into the summer months which therefore leads to increased violence according to the data.")),
                           
                           # Here, I create a new tab called "Additional Data Visualizations for 
                           # 2013" and print out the gt graph.
                           
                           tabPanel("More Permits = More Gun Violence?",
                                    h3("The states that granted a high number of permits in 2013 also saw a high number of gun violence incidents."),
                                    h4("Notice that 4 of the 9 highlighted states are in the south."),
                                    imageOutput("graph3")),
                           
                           # Next, I'm making my Regression tab.
                           
                           tabPanel("Visualizing Linear Regressions",
                                    
                                    # I used plotly on my regressiongraph so I had to call plotlyOutput rather
                                    # than just plotOutput as for my regressiongraph.
                                    # However, my regressionsgraph2 is just a plot so I called out plotOutput
                                    # rather than plotlyOutput. 
                                    # Between my plotOutputs and plotlyOutputs I also added text. 
                                    
                                    h3("The Impact of Permits Granted on the Number of Gun Violence Incidents"),
                                    plotOutput("regressiongraph2"),
                                    h5("All 227,885 data points are shown in the graph above."),
                                    h5("As the number of permits granted increases, the number of gun violence incidents increase"),
                                    h3("Number of Permits Granted and Gun Violence Incidents By Region"),
                                    plotlyOutput("regressionsssgraph"),
                                    h5("The x-axis is scaled by log in order to better see the points."),
                                    h5("Notice that for a fixed number of permits, the South has more gun violence incidents than average as most of the data points for the South are above the line of best fit."),
                                    h5("Please double click on the region at right to only see the observations from that region."),
                                    h3("Number of Permits Granted and Gun Violence Incidents By State"),
                                    h4("An Analysis of the 50 US States"),
                                    plotlyOutput("regressiongraph"),
                                    h5("The confidence intervals are small because there are over 227,000 gun violence incidents in my dataset."),
                                    h5("Please double click on the state at right to only see the observations from that state.")),
                           
                           # My Regression Coefficient Plot tab calls the regressiondata output defined
                           # in my server. 
                           
                           tabPanel("Regression Coefficient Plot",
                                    h3("Coefficient plot with the intercept"),
                                    h4("The following plots estimate the number of gun violence incidents dependent on the number of permits granted and the population."),
                                    plotOutput("originalcoef"),
                                    h5("In the above plot, the intercept coefficient is much larger than the other coefficients in the regression. 
                           Therefore, the other coefficients appear to be almost zero. In order to more accurately see the variables in my 
                           regression, please see the fixed effects model."),
                                    h5("This coefficient plot shows the estimates for permit and population. The horizontal lines show the 95% confidence
                           intervals. If we were to get different samples using the same methods, it would be expected that the values of 
                           permit and population would be within the lines of the confidence interval 95% of the time."),
                                    h5("The intercept is also known as the constant value and is thus the expected regression estimate."),
                                    h3("Fixed effects model (omitting the intercept)"),
                                    plotOutput("regressiondata"),
                                    h5("In the plots, a positive coefficient indicates a positive relationship with the dependent variable while a negative
                           coefficient indicates a negative relationship with the dependent variable."),
                                    h5("Therefore, a higher population indicates more gun violence incidents while more permits leads to a slight decrease
                           in the number of gun violence incidents.")),
                           
                           # Here, I make my state policy correlation tab and include descriptive text.
                           # Remember that br() causes a page break.
                           
                           tabPanel("State Policy Correlation",
                                    h3("Do state-wide gun registration requirements lead to less gun violence incidents?"),
                                    br(),
                                    
                                    # SelectInput creates a dropdown bar where you can select the year.
                                    # Depending on the year, U print out two graphs (statepolicy1 and
                                    # statepolicy2). 
                                    
                                    selectInput("year",
                                                "Select Year:", unique(new_data2$year)),  
                                    h4("States that required gun registration:"),
                                    plotOutput("statepolicy1"),
                                    h4("States that did not require gun registration:"),
                                    plotOutput("statepolicy2"),
                                    h5("States that required gun registration in 2013 and 2014 did not see a significant decrease in the number of 
                           gun violence incidents."),
                                    br(),
                                    h4("States that have a waiting period law:"),
                                    plotOutput("statepolicy3"),
                                    h4("States that do not have a waiting period law:"),
                                    plotOutput("statepolicy4"),
                                    h5("According to Giffords Law Center, waiting periods mandate that an individual can only gain access to a firearm a certain number of days after buying the firearm. Furthermore, waiting periods are beleived to limit impulsive gun violence actions."),
                                    h4("States that have a ban on assault weapons:"),
                                    plotOutput("statepolicy5"),
                                    h4("States that do not have a ban on assault weapons:"),
                                    h5("Some states prohibit ownership of assault weapons and often specify what qualifies as an assault weapon."),
                                    plotOutput("statepolicy6"),
                                    h4("States that have an open carry policy:"),
                                    plotOutput("statepolicy7"),
                                    h4("States that do not have an open carry policy:"),
                                    plotOutput("statepolicy8"),
                                    h5("An open carry policy allows people to legally carry firearms openly in public."),
                                    h4("States that have a stand your ground policy:"),
                                    plotOutput("statepolicy9"),
                                    h4("States that do not have a stand your ground policy:"),
                                    plotOutput("statepolicy10"),
                                    h5("Stand your ground policies allow people to use firearms to defend themselves without first trying to negotiate or retreat.")),
                           
                           tabPanel("Video",
                           h3("Video Walkthrough of Gun Violence Project Shiny App"),
                           h5("A brief summary of the highlights of my project."),
                           embed_youtube("n1KaiZ679OA", width = 500, height = 280, allowfullscreen = TRUE)),
                           
                           
                           # Here, I format my purpose and conclusions tab.
                           # Remember that h3 is bigger than h5 (smaller numbers means bigger text).
                           
                           tabPanel("Purpose and Conclusions",
                                    mainPanel(
                                      h3("Why is analyzing data related to gun violence important?"),
                                      h5("Every day, 96 people die from gun violence and 222 people are shot and survive ", a("(source)", href="https://www.teamenough.org/gun-violence-statistics"), "Gun violence impacts people of all ages and mass shootings are increasingly common. What are the leading causes of gun violence? What factors lead to an increase in gun violence and what policies fail to make an impact on gun violence?"),
                                      h5("It is widely accepted that gun violence increases in summer months. An ", a("article", href="https://www.nytimes.com/2018/09/21/upshot/a-rise-in-murder-lets-talk-about-the-weather.html"), "called “A Rise in Murder? Let’s Talk About the Weather,” published in the New York Times in September 2018, suggests that murders increase in summer months. Additionally ", a("Giffords Law Center to Prevent Gun Violence", href="https://lawcenter.giffords.org/resources/publications/shootings-spike-in-summer-months/"), "also argues that the number of murders increase as the temperature increases in summer months. However, just as the effects of gun violence are wide-reaching, impacting individuals as well as community sentiment, I argue that there is no single cause of gun violence in the United States."), 
                                      h3("Why is it important to specifically analyze the number of gun permits granted?"),
                                      h5("On November 25, 2019, USA Today published an article that detailed the sudden increase in 
                           background checks as a result of protests for stricter gun laws following gun violence incidents."),
                                      h5("Therefore, an increase in background checks is often spurred by a fear of more restrictive gun laws."),
                                      h6("To read more about the increase in gun violence permits, please see the ", a("USA Today Article.", href="https://www.usatoday.com/story/news/politics/2019/11/25/fbi-background-checks-rise-amid-mass-shootings-calls-gun-control/4228725002/")),
                                      h3("Conclusions"),
                                      h5("In 2013, the number of permits sold increased between January and June, the month in which the most permits were sold, before decreasing. In 2014, the most permits were sold in March and decreased July and November. In 2015, March was the month in which the second most number of permits were sold. Permits sold generally decreased in the summer months of 2015. In 2016, the most number of permits were sold in March and permits sold increased between May and July. Finally in 2017, the number of permits sold decreased after reaching a high point in March. Therefore, March is most commonly the month where the most permits were sold. This is significant because more guns in March would therefore lead to more gun violence incidents in the summer months. Furthermore, Kentucky was most often the state that granted the most number of permits over much larger states. This is especially significant given that the population of Kentucky is about 4 million people while the population of California is about 40 million ", a("(source)", href="https://www.census.gov/search-results.html?searchType=web&cssp=SERP&q=state%20population"), "."),
                                      h5("An increase in permits granted correlates to higher numbers of gun violence incidents. For a fixed number of 
                            permits, southern states have more gun violence incidents than the midwest, northeast, and west. Western states 
                            had the most spread out gun violence incidents when compared to the average number of permits granted per month. 
                            Although gun violence activists often call for stricter gun violence policies, such as gun registration, the 
                            data does not show a significant change in the number of incidents with different gun registration policies. 
                            In other words, states that required gun registration had very similar rates of gun violence incidents when 
                            compared to states that did not require gun registrations."))),
                           
                           # Finally, I created my About tab and used the textOutput function to define
                           # the text that I want on my About page. 
                           
                           tabPanel("About",
                                    mainPanel(
                                      h3("Data Sources"),
                                      h5("The plots are created using data from ", a("The National Instant Criminal Background Check System (NICS)", href="https://www.fbi.gov/services/cjis/nics"), ", provided by the Federal Bureau of Investigation. Background checks are strong indicators of the number of firearms sold."),
                                      h5("Population data was also gathered from ", a("The United States Census Bureau", href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"), ", which gathers population data from the 2010s."),
                                      h5("In order to find the number of gun violence incidents, I used a ", a("Gun Violence", href="https://www.kaggle.com/jameslko/gun-violence-data"), " data set"),
                                      h5("The Gun Violence dataset uses data from  which compiled data from ", a("the Gun Violence Archive", href="https://www.gunviolencearchive.org"), " an organization dedicated to providing real-time gun violence data"),
                                      h5("Finally, I also used a dataset created by the Institute for Public Policy and Social Research called ", a("Correlates of State Policy", href="http://ippsr.msu.edu/public-policy/correlates-state-policy"), ", which includes state policy data related to gun violence, such as states that require gun registration."),
                                      h5("Citation for State Policy Dataset"),
                                      h6("Jordan, Marty P. and Matt Grossmann. 2017. The Correlates of State Policy Project v.2.1. East Lansing, MI: Institute for Public Policy and Social Research (IPPSR)."),
                                      h3("About Me"),
                                      h5("Hi! My name is Emily Axelsen and I am first-year student at Harvard College studying History and passionate about data science and R."),
                                      h5("This project was created for a course called Gov 1005: Data Science fall semester 2019."),
                                      h5("Contact me at emilyaxelsen@college.harvard.edu or connect with me on ", a("LinkedIn", href="https://www.linkedin.com/in/emily-axelsen/"),"."),
                                      h5("The code for my Shiny App can be found at my ", a("GitHub", href="https://github.com/EmilyAxelsen/gun_violence_final"),"."),
                                      h3("Site Navigation"),
                                      h5("In the drop down graphics tab, the graphs show the top ten states that granted the most gun permits as well 
                             as the total number of gun permits granted per month. The slider graphics tab shows the number of gun permits 
                             granted per month then graphs the top ten states that granted the most permits for that month where the most 
                             number of permits were granted. For the 2013 tab, I also created a gt table which shows the number of incidents 
                             per state in the month that granted the most number of permits in 2013. Next, the regression tab shows a linear 
                             regression model of the number of permits granted per month in relation to the number of gun violence incidents. 
                             The x axis of my first graph is a log of the x axis of my second graph in order to see where the data is most 
                             concentrated. Users may also hover over each point to see more information about the point. The regression 
                             coefficient plot is a visual representation of my linear regression.")),
                                    
                
                                    
                           )))

# This is the start of my server section, rather than the ui section.    

server <- function(input, output) {
  
  # First, I specified the images I wanted to render for my graph.
  
  output$graph <- renderImage({
    
    # Here, I specify that if the slider is at 2013 then shiny should
    # display the "2013graph.png" image. I repeat this process for my
    # 2014, 2015, 2016, and 2017 graphs. 
    
    if(input$currentyear==2013) year <-"2013graphuse.png"
    if(input$currentyear==2014) year <-"2014graphuse.png"
    if(input$currentyear==2015) year <-"2015graphuse.png"
    if(input$currentyear==2016) year <-"2016graphuse.png"
    if(input$currentyear==2017) year <-"2017graphuse.png"
    
    # Here, I specify the size of my images because when I first put
    # the images in my Shiny they were very large. I played around
    # with the width and height until I settled upon 600 by 400. 
    
    list(src=year,
         width = 600,
         height = 400)
  },
  
  # Initially, after Shiny would read the if statements above, it
  # would then delete the png file from my shiny folder. I set 
  # deleteFile equal to FALSE to prevent this issue and make sure 
  # that I could continually run my Shiny app. 
  
  deleteFile = FALSE )
  
  # I repeated the process described above for graph 2. I had to
  # specify another renderImage because I wanted two graphs to
  # appear when I specified the year on my slider. I saved my
  # graphs from my R Markdown file as png and specified the path
  # as equal to my shiny folder. 
  
  output$graph2 <- renderImage({
    if(input$currentyear==2013) year <-"2013graph2use.png"
    if(input$currentyear==2014) year <-"2014graph2use.png"
    if(input$currentyear==2015) year <-"2015graph2use.png"
    if(input$currentyear==2016) year <-"2016graph2use.png"
    if(input$currentyear==2017) year <-"2017graph2use.png"
    
    # Once again, I want to specify my width and height so the user 
    # can easily read my graphs.
    
    list(src=year,
         width = 600,
         height = 400)
  },
  
  # I also do not want to delete the graph2 files. 
  
  deleteFile = FALSE )
  
  # Here, I read in my gt file which is for 2013 data. Since the gt
  # is only for 2013 data, I only call it if the input is 2013. 
  
  output$graph3 <- renderImage({
    year <-"gt.png"
    
    # Here I format to make sure the user can read the gt by specifying
    # the size of the gt image. 
    
    list(src=year,
         width = 800,
         height = 1000,
         align = "center")
  },
  
  # Also, I don't want to delete my gt. 
  
  deleteFile = FALSE )
  
  # Here, I used ggplotly to create a dynamic graph that the user can 
  # interact with. In order to make the ggplotly, I simply wrapped my 
  # ggplot with the ggplotly function. 
  
  
  # Graph with 95% confidence intervals instead of each data point itself.
  
  output$regressiongraph <- renderPlotly({
    ggplotly(
      permit1 %>%
        ggplot(aes(x = permit, y = n_incidents, color = state)) +
        geom_jitter(show.legend = FALSE) +
        geom_smooth(method = 'lm',col = 'black')+
        scale_x_continuous(limits = c(0, 20000))+
        scale_y_continuous(limits = c(0, 2000)) +
        labs(x = "Average Permits Granted Per Month", 
             y = "Number of Gun Violence Incidents") +
        
        
        # I scaled the x and y axes by log10 in order to see the data easier. 
        # I chose to scale the x and y axis in order to maintain the validity 
        # of the data and just scaled down the x and y axis values.            
        
        scale_y_log10(labels = comma) +
        scale_x_log10(labels = comma) ) %>%
      style(hoverinfo = "text",
            hovertext = paste("State:", permit1$state))
  })
  
  
  output$regressionsssgraph <- renderPlotly({
    permit2 <- final_gun_violence_data %>%
      group_by(year, state, permit, population) %>% 
      summarise(n_incidents = n()) %>%
      
      # Within mutate, I specify the region with a series of ifelse statements.
      # If the state is within the list of states I specify, then it will be 
      # assigned that region. Until the state matches a state in the list, it
      # continues to go through the series of ifelse statements until it reaches 
      # the end when it is specified as "District of Columbia." 
      
      mutate(region = ifelse(state %in% c("Wisconsin", "Michigan", "Ohio", "Indiana", "Illinois", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Midwest",
                             ifelse(state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"), "South",
                                    ifelse(state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennslyvania"), "Northeast",
                                           ifelse(state %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington"), "West", "District of Columbia")))))
    ggplotly(
      permit2 %>%
        
        # I wanted the colors of each point to be defined by region to be able to 
        # quickly determine which regions have increased gun violence compared to
        # other regions. 
        
        ggplot(aes(x = permit, y = n_incidents, color = region)) +
        geom_jitter(show.legend = FALSE) +
        
        # Calling the geom_smooth function allowed me to add a regression line.
        
        geom_smooth(method = 'lm', col = 'black') + 
        labs(x = "Average Permits Granted Per Month", 
             y = "Number of Gun Violence Incidents") +
        scale_y_log10(labels = comma) +
        scale_x_log10(labels = comma) ) %>%
      
      # The style function allowed me to define what information I wanted
      # to show up in the box that appears when I hover over a specific point
      # in my plotly. I also called region from within my permit2. 
      
      style(hoverinfo = "text",
            hovertext = paste("Region:", permit2$region))
    
  })
  
  # Here, in order to apply this code to multiple years, I filter 
  # for the year that the user specifies with input$year. 
  # Next, I make a graph with the states that require gun registration. 
  # I also find the number of incidents by summarising n. 
  
  output$statepolicy1 <- renderPlot({
    incidents_regis_requir <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(w_guncontrol_registration_requir == "TRUE") %>%
      group_by(year, state, permit, population, w_guncontrol_registration_requir) %>% 
      summarise(n_incidents = n()) %>%
      arrange(desc(n_incidents))
    
    # Remember that setting fill equal to the values on the x or y axis results
    # in different color bar plots. 
    
    ggplot(incidents_regis_requir, aes(x = reorder(state, n_incidents), y = n_incidents, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
  })
  
  # Here, I make my state policy graph remembering to set the year equal to 
  # the year the user specifies with input$year. 
  
  output$statepolicy2 <- renderPlot({
    incidents_regis_requir2 <- policy_and_checks %>%
      filter(year == input$year) %>%
      
      # This graph shows the states where gun registration is not required which
      # is therefore FALSE in the dataset. 
      
      filter(w_guncontrol_registration_requir == "FALSE") %>%
      group_by(year, state, permit, population, w_guncontrol_registration_requir) %>% 
      summarise(n_incidents1 = n()) 
    
    # Since the states have longer titles, I wanted to put them on the y-axis.           
    
    ggplot(incidents_regis_requir2, aes(x = reorder(state, n_incidents1), y = n_incidents1, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # This graph plots states that do have a waiting period
  # requirement. 
  
  output$statepolicy3 <- renderPlot({
    incidents_waiting1 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(w_guncontrol_waitingperiod == "TRUE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents2 = n())           
    
    ggplot(incidents_waiting1, aes(x = reorder(state, n_incidents2), y = n_incidents2, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # This graph plots the states that do not have a waiting 
  # period requirement. 
  
  output$statepolicy4 <- renderPlot({
    incidents_waiting2 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(w_guncontrol_waitingperiod == "FALSE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents3 = n())         
    
    ggplot(incidents_waiting2, aes(x = reorder(state, n_incidents3), y = n_incidents3, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # Graphing states that do have a ban on assault weapons.
  
  output$statepolicy5 <- renderPlot({
    incidents_assault1 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_assaultweapon_ban == "TRUE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents4 = n())           
    
    ggplot(incidents_assault1, aes(x = reorder(state, n_incidents4), y = n_incidents4, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # Graphing states that do not have a ban on assault weapons.
  
  output$statepolicy6 <- renderPlot({
    incidents_assault2 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_assaultweapon_ban == "FALSE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents5 = n())           
    
    ggplot(incidents_assault2, aes(x = reorder(state, n_incidents5), y = n_incidents5, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # This graph is for states that do have an open carry policy.
  
  output$statepolicy7 <- renderPlot({
    incidents_open1 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_opencarry == "TRUE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents6 = n())           
    
    ggplot(incidents_open1, aes(x = reorder(state, n_incidents6), y = n_incidents6, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # This graph plots the gun violence incidents in states that
  # do not have an open carry policy.
  
  output$statepolicy8 <- renderPlot({
    incidents_open2 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_opencarry == "FALSE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents7 = n())           
    
    ggplot(incidents_open2, aes(x = reorder(state, n_incidents7), y = n_incidents7, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # Here, I'm making a graph for states that have stand_your_ground
  # policies. 
  
  output$statepolicy9 <- renderPlot({
    incidents_stand1 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_stand_your_ground == "TRUE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents8 = n())           
    
    ggplot(incidents_stand1, aes(x = reorder(state, n_incidents8), y = n_incidents8, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # Making a graph for states that do not have a stand your ground policy.
  # Other than replacing the variable with guncontrol_stand_your_ground, the 
  # code for this graph is the same as the code for my other previous graphs.
  
  output$statepolicy10 <- renderPlot({
    incidents_stand2 <- policy_and_checks %>%
      filter(year == input$year) %>%
      filter(guncontrol_stand_your_ground == "FALSE") %>%
      group_by(year, state, permit, population, w_guncontrol_waitingperiod) %>% 
      summarise(n_incidents9 = n())           
    
    ggplot(incidents_stand2, aes(x = reorder(state, n_incidents9), y = n_incidents9, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "State", y = "Number of Incidents")
    
  })
  
  # This plot is a graph of my regression. I made sure to call renderPlot
  # rather than renderPlotly here because my graph is a plot rather than a 
  # plotly. 
  
  output$regressiongraph2 <- renderPlot({
    permit1 <- final_gun_violence_data %>%
      group_by(year, state, permit, population) %>% 
      summarise(n_incidents = n()) 
    
    # Remember that geom_jitter AND geom_smooth are useful when creating
    # regression graphs. Within geom_smooth, I specified the model as 'lm' and 
    # set the color equal to 'black.           
    
    permit1 %>%
      ggplot(aes(x = permit, y = n_incidents, color = state)) +
      geom_jitter(show.legend = FALSE) +
      geom_smooth(method = 'lm', se = F, col = 'black') +
      
      # The labs function is always used to specify the titles, subtitles, captions
      # (for the source of the data) as well as the x and y axis labels.               
      
      labs(x = "Average Permits Granted Per Month", 
           y = "Number of Gun Violence Incidents") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = comma)
  })
  
  # Here, I create another plot using my final_gun_violence_data dataset. 
  # Instead of hard coding what year I wanted the graph to show, I instead
  # used input$chosenyear which corresponds to the year that the user 
  # chooses in the drop down. 
  
  output$permitpermontheighteen <- renderPlot({
    final_gun_violence_data %>%
      filter(year == input$chosenyear) %>%
      
      # Next, I create a regular bar plot and remember to set show.legend = 
      # FALSE since I don't want to include a legend in my plot. 
      
      ggplot(aes(x = month, y = permit, fill = month, color = month)) + 
      geom_bar(stat = 'identity', show.legend = FALSE) +
      labs(x = "Month", 
           y = "Number of Permits Granted") +
      
      # I specified scale_y_continuous with labels as commas in order to 
      # appropriately format the numbers on the y axis. 
      
      scale_y_continuous(labels = comma)
  })
  
  # I created my regression within a renderPlot and called the result
  # regressiondata so I can then call regression data within my ui and
  # have the result in my Shiny app.  
  
  output$originalcoef <- renderPlot({
    permit1 <- final_gun_violence_data %>%
      group_by(year, state, permit, population) %>% 
      summarise(n_incidents = n()) 
    
    ggplot(
      m1 <- lm(n_incidents ~ permit + population, data = permit1))
    
    A <- coefplot(m1)
    A + theme_bw() + 
      scale_y_discrete(labels=c("Constant", "Permit", "Population")) +
      scale_x_continuous(name="Regression Estimate") +
      labs(title = "")
    
  })
  
  output$regressiondata <- renderPlot({
    
    ggplot(
      
      m2 <- lm(n_incidents ~ permit + population + as.factor(state) + as.factor(year), data = permit1))
    
    summary(m2)
    
    # Remember that coefplot is a way to visualize the regression and 
    # is from the library coefplot.            
    
    B <- coefplot(m2, coefficients = c("permit", "population"))
    B + theme_bw() + 
      scale_y_discrete(labels=c("Permit", "Population")) +
      scale_x_continuous(name="Regression Estimate") +
      labs(title = "")
    
  })
  
  
  # Remember that you must specify the output in order to call the
  # output in your ui. 
  
  output$permiteighteen <- renderPlot({
    
    # In order to get my permit_2013 filtered data, I first piped my
    # final_gun_violence_data set into select to select for only the
    # variables I want. Next, I filtered for the 3rd month (March) 
    # since I know that's the month that the most permits were granted 
    # in the majority of the years 2013-2017. 
    
    permit_2013 <- final_gun_violence_data %>%
      select(state, permit, year, month) %>%
      filter(month == "03") %>%
      
      # In order to be able to apply this to multiple years, I specified
      # the year to be input$chosenyear which sets the year equal to the 
      # year chosen by the user.
      
      filter(year == input$chosenyear) %>%
      unique() %>%
      arrange(desc(permit))  %>%
      slice(1:10) 
    
    # I call ggplot in order to create a graph with my permit_2013 data I created in
    # the previous step. I reordered the x and y axises in order to create a graph
    # that is easier to read. I also chose to fill the bars by state in order to get
    # a more aesthetically appealing graph. Remember to use labs to add titles and 
    # captions for your graphs. I also called the coord_flip() function in order to 
    # have my bars in my bar graph as horizontal rather than vertical. The show.legend
    # equal to false is also useful in order to remove the legend (just have to 
    # remember) to put it in geom_col. 
    
    ggplot(permit_2013, aes(x = reorder(state, permit), y = permit, fill = state)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      
      # I specified my axis titles with labs. 
      
      labs(x = "State", 
           y = "Number of Permits Granted") +
      
      # Here, I added commas to my y-axis numbers.
      
      scale_y_continuous(labels = comma)
  }
  )
  
  # Here, I define my output$Text as renderText and list the text I want in
  # quotes. Remember that I use output$Text in order to be able to call the 
  # output in my ui. 
  
  output$Text_drop_down <- renderText({
    "Source: The National Instant Criminal Background Check System, Gun Violence Data courtesy of gunviolencearchive.org, Population data from Census.gov"
  })
}


# Here, I call shinyApp and define my ui as the ui I defined in my code
# and the server as the server that I also defined in my code. 

shinyApp(ui = ui, server = server)


