
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
                
    navbarPage("More Permits More Problems?: Tracing Factors Correlated to Gun Violence",
               
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
                         

# Next, I define my next tabPanel as "Slider Graphics."

               tabPanel("More Permits = More Gun Violence?",
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
# the server as graph, graph2, and graph 3.
                        
                        imageOutput("graph"),
                        imageOutput("graph2"),
                        h5("In 2014, 2016, and 2017, the highest number of permits were granted in March. Therefore, the number of guns would increase going into the summer months which therefore leads to increased violence according to the data.")),
          
      
              tabPanel("Additional Data Visualizations for 2013",
                        imageOutput("graph3")),

# Next, I'm making my Regression tab.

               tabPanel("Visualizing Linear Regressions",
                        
# I used plotly on my regressiongraph so I had to call plotlyOutput rather
# than just plotOutput as for my regressiongraph2. 
                        
                        h4("The impact of gun permits granted on the number of gun violence incidents:"),
                        plotOutput("regressiongraph2"),
                        h5("All data points are shown in the graph above."),
                        plotlyOutput("regressionsssgraph"),
                        h5("The x-axis is scaled by log in order to better see the points."),
                        h5("Notice that for a fixed number of permits, the South has more gun violence incidents than average as most of the data points for the South are above the line of best fit."),
                        plotlyOutput("regressiongraph")),
               
# My Regression Coefficient Plot tab calls the regressiondata output defined
# in my server. 

               tabPanel("Regression Coefficient Plot",
                        plotOutput("regressiondata")),

               tabPanel("State Policy Correlation",
                        h3("Do state-wide gun registration requirements lead to less gun violence incidents?"),
                        br(),
                        
                        selectInput("year",
                                    "Select Year:", unique(new_data2$year)),  
                        h4("States that required gun registration:"),
                        plotOutput("statepolicy1"),
                        h4("States that did not require gun registration:"),
                        plotOutput("statepolicy2"),
                        h5("States that required gun registration in 2013 and 2014 did not see a significant decrease in the number of gun violence incidents.")),

# Finally, I created my About tab and used the textOutput function to define
# the text that I want on my About page. 
               
               tabPanel("About",
                        mainPanel(
                          h2("Data Sources"),
                          h5("The plots are created using data from ", a("The National Instant Criminal Background Check System (NICS)", href="https://www.fbi.gov/services/cjis/nics"), ", provided by the Federal Bureau of Investigation. Background checks are strong indicators of the number of firearms sold."),
                          h5("Population data was also gathered from ", a("The United States Census Bureau", href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"), ", which gathers population data from the 2010s."),
                          h5("In order to find the number of gun violence incidents, I used a ", a("Gun Violence", href="https://www.kaggle.com/jameslko/gun-violence-data"), " data set"),
                          h5("The Gun Violence dataset uses data from  which compiled data from ", a("the Gun Violence Archive", href="https://www.gunviolencearchive.org"), " an organization dedicated to providing real-time gun violence data"),
                          h5("Finally, I also used a dataset created by the Institute for Public Policy and Social Research called ", a("Correlates of State Policy", href="http://ippsr.msu.edu/public-policy/correlates-state-policy"), ", which includes state policy data related to gun violence, such as states that require gun registration."),
                          h5("Citation for State Policy Dataset"),
                          h6("Jordan, Marty P. and Matt Grossmann. 2017. The Correlates of State Policy Project v.2.1. East Lansing, MI: Institute for Public Policy and Social Research (IPPSR)."),
                          h2("About Me"),
                          h5("Hi! My name is Emily Axelsen and I am first-year student at Harvard College studying History and passionate about data science and R."),
                          h5("This project was created for a course called Gov 1005: Data Science fall semester 2019."),
                          h5("Contact me at emilyaxelsen@college.harvard.edu."),
                          h5("The code for my Shiny App can be found at my ", a("GitHub", href="https://github.com/EmilyAxelsen"),"."),
                          h2("Detailed Explanation of Data & Sources"),
                          h5("Just as the effects of gun violence are wide-reaching, impacting individuals as well as community sentiment, there is no single cause of gun violence in the United States. Therefore, I am interested in analyzing how gun violence correlates to economic injustice and the impact of community-based programs."),
                          h5("Does data show that communities with gun violence programs actually experience a decrease in gun violence? By evaluating the statistically significant rates of gun violence compared to the population, I located the cities on which to focus my analysis. The Federal Bureau of Investigation’s National Instant Criminal Background Check System (NICS) provides data on how many background checks were conducted in the United States. This data is converted from PDF to CSV by Buzzfeed News and is significant as firearm background checks often have a close correlation to gun sales and therefore are a good indication of a state’s gun sales. Through my data analysis, I plan to explore whether there is a connection between increased gun sales and more gun violence."),
                          h5("To determine public policies related to guns, I made use of a dataset that compiles information about different state’s public policies. Students and scholars through Michigan State University’s Institute for Public Policy and Social Research worked to organize and make policy data publicly available. Through The Trace, a resource that publishes information related to gun violence, I acquired a data source that provides information on the state, date, number killed, number injured, and age group for over 230,000 incidents involving gun violence. This gun violence datasource is from an organization called Gun Violence Archive which finds data by combing through local and state police and other government sources that report gun violence and crime."),
                          h5("To compare the number of background checks conducted, the number of public policies compared to population size, and gun violence incidents, I located a data source that provides population information for more than 28,000 United States cities and towns. This data was compiled through the use of data from the United States Census Bureau and the United States Geological Survey."),
                          h2("Site Navigation"),
                          h5("In the drop down graphics tab, the graphs show the top ten states that granted the most gun permits as well as the total number of gun permits granted per month. The slider graphics tab shows the number of gun permits granted per month then graphs the top ten states that granted the most permits for that month where the most number of permits were granted. For the 2013 tab, I also created a gt table which shows the number of incidents per state in the month that granted the most number of permits in 2013. Next, the regression tab shows a linear regression model of the number of permits granted per month in relation to the number of gun violence incidents. The x axis of my first graph is a log of the x axis of my second graph in order to see where the data is most concentrated. Users may also hover over each point to see more information about the point. The regression coefficient plot is a visual representation of my linear regression."),
                          h2("Why is it important to analyze the number of gun permits granted?"),
                          h5("On November 25, 2019, USA Today published an article that detailed the sudden increase in 
                           background checks as a result of protests for stricter gun laws following gun violence incidents."),
                          h5("Therefore, an increase in background checks is often spurred by a fear of more restrictive gun laws."),
                          h6("To read more about the increase in gun violence permits, please see the ", a("USA Today Article.", href="https://www.usatoday.com/story/news/politics/2019/11/25/fbi-background-checks-rise-amid-mass-shootings-calls-gun-control/4228725002/ ")
                          ),
                          
                          ))))
  

  
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
        
# Here I format to make sure the user can read the gt. 
        
        list(src=year,
             width = 800,
             height = 1000,
             align = "center")
    },

# Also don't want to delete my gt. 

    deleteFile = FALSE )
    
# Here, I used ggplotly to create a dynamic graph that the user can 
# interact with. In order to make the ggplotly, I simply wrapped my 
# ggplot with the ggplotly function. This ggplot is the same ggplot
# as the one that appears next in my Shiny. However, I scaled the x
# and y axes by log10 in order to see the data easier. 
# I chose to scale the x and y axis in order to maintain the validity
# of the data and just scaled down the x and y axis values. 
    
    
# Graph with 95% confidence intervals instead of each data point itself.
    
        output$regressiongraph <- renderPlotly({
          permit1 <- final_gun_violence_data %>%
            group_by(year, state, permit, population) %>% 
            summarise(n_incidents = n()) 
          hide_legend(ggplotly(
          permit1 %>%
            ggplot(aes(x = permit, y = n_incidents, color = state)) +
            geom_jitter(show.legend = FALSE) +
            geom_smooth(method = 'lm', col = 'black') +
            scale_x_continuous(limits = c(0, 10)) + 
            scale_y_continuous(limits = c(0, 10)) +
            coord_cartesian(xlim=c(0,10), ylim=c(0,10)) + 
            labs(x = "Number of Gun Violence Incidents", 
                 y = "Average Permits Granted Per Month",
                 title = "Impact of Permits Granted on Number of Gun Violence Incidents",
                 subtitle = "An Analysis of the 50 US States",
                 caption = "Source: The National Instant Criminal Background Check System and " +
                scale_y_log10() +
                scale_x_log10() 
            )))
            
        })
        
        
        output$regressionsssgraph <- renderPlotly({
          permit2 <- final_gun_violence_data %>%
            group_by(year, state, permit, population) %>% 
            summarise(n_incidents = n()) %>%
            mutate(region = ifelse(state %in% c("Wisconsin", "Michigan", "Ohio", "Indiana", "Illinois", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "Midwest",
                            ifelse(state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"), "South",
                            ifelse(state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennslyvania"), "Northeast",
                            ifelse(state %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington"), "West", "District of Columbia")))))
         ggplotly(
          permit2 %>%
            ggplot(aes(x = permit, y = n_incidents, color = region)) +
            geom_jitter(show.legend = FALSE) +
            geom_smooth(method = 'lm', col = 'black') + 
            labs(x = "Average Permits Granted Per Month", 
                 y = "Number of Gun Violence Incidents") +
            scale_y_log10() +
            scale_x_log10() ) %>%
           style(hoverinfo = "text",
                 hovertext = paste("Region:", permit2$region))
          
        })
        
        
        output$statepolicy1 <- renderPlot({
          incidents_regis_requir <- policy_and_checks %>%
            filter(year == input$year) %>%
            filter(w_guncontrol_registration_requir == "TRUE") %>%
            group_by(year, state, permit, population, w_guncontrol_registration_requir) %>% 
            summarise(n_incidents = n()) 
        
          ggplot(incidents_regis_requir, aes(x = reorder(state, n_incidents), y = n_incidents, fill = state)) +
            geom_col(show.legend = FALSE) +
            coord_flip() +
            labs(x = "State", y = "Number of Incidents")
                 #title = "Incidents in States With Required Gun Registration",
                 #subtitle = "Do the number of gun violence incidents decrease when gun registration is required?")
        })
        
        output$statepolicy2 <- renderPlot({
          incidents_regis_requir2 <- policy_and_checks %>%
            filter(year == input$year) %>%
            filter(w_guncontrol_registration_requir == "FALSE") %>%
            group_by(year, state, permit, population, w_guncontrol_registration_requir) %>% 
            summarise(n_incidents = n()) 
      
          ggplot(incidents_regis_requir2, aes(x = reorder(state, n_incidents), y = n_incidents, fill = state)) +
            geom_col(show.legend = FALSE) +
            coord_flip() +
            labs(x = "State", y = "Number of Incidents")
                 #title = "Incidents in States Where Gun Registration Is Not Required",
                 #subtitle = "Do the number of gun violence incidents decrease when gun registration is required?")
        })
        
# This plot is a graph of my regression. I made sure to call renderPlot
# rather than renderPlotly here because my graph is a plot rather than a 
# plotly. Remember that geom_jitter AND geom_smooth are useful when creating
# regression graphs. Within geom_smooth, I specified the model as 'lm' and 
# set the color equal to 'black. 
# The labs function is always used to specify the titles, subtitles, captions
# (for the source of the data) as well as the x and y axis labels. 
        
        output$regressiongraph2 <- renderPlot({
          
          permit1 <- final_gun_violence_data %>%
            group_by(year, state, permit, population) %>% 
            summarise(n_incidents = n()) 
          
          
            permit1 %>%
                ggplot(aes(x = permit, y = n_incidents, color = state)) +
                geom_jitter(show.legend = FALSE) +
                geom_smooth(method = 'lm', se = F, col = 'black') +
                labs(x = "Number of Gun Violence Incidents", 
                     y = "Average Permits Granted Per Month") 
            
        })
        
# Here, I create another plot using my final_gun_violence_data dataset. 
# Instead of hard coding what year I wanted the graph to show, I instead
# used input$chosenyear which corresponds to the year that the user 
# chooses in the drop down. 
            
        output$permitpermontheighteen <- renderPlot({
            final_gun_violence_data %>%
                filter(year == input$chosenyear) %>%
                ggplot(aes(x = month, y = permit, fill = month)) + 
                geom_bar(stat = 'identity', show.legend = FALSE) +
                labs(x = "Month", 
                     y = "Number of Permits Granted"
                #      title = "The Number of Gun Permits Granted Per Month",
                #      subtitle = "Do Gun Permits Sold Increase in Summer Months?",
                #      caption = "Source: The National Instant Criminal Background Check System, Gun Violence Data courtesy of gunviolencearchive.org, Population data from Census.gov
                                ) +
            scale_y_continuous(labels = comma)
        })
    
# I created my regression within a renderPlot and called the result
# regressiondata so I can then call regression data within my ui and
# have the result in my Shiny app. Remember that coefplot is a way
# to visualize the regression and is from the library coefplot. 
                
        output$regressiondata <- renderPlot({
          permit1 <- final_gun_violence_data %>%
            group_by(year, state, permit, population) %>% 
            summarise(n_incidents = n()) 
          
          ggplot(
            m1 <- lm(n_incidents ~ permit + population, data = permit1))
            
            coefplot(m1)
            
        })
        
# Remember that you must specify the output in order to call the
# output in your ui. 
        
        output$permiteighteen <- renderPlot({
            
# In order to get my permit_2013 filtered data, I first piped my
# final_gun_violence_data set into select to select for only the
# variables I want. Next, I filtered for the 6th month (June) 
# since I know that's the month that the most permits were granted 
# in the majority of the years 2013-2017. 
            
            permit_2013 <- final_gun_violence_data %>%
                select(state, permit, year, month) %>%
                filter(month == "03") %>%
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
                labs(x = "State", 
                     y = "Number of Permits Granted"
                      #title = "The Number of Gun Permits Granted Per State in March",
                      #subtitle = "The 10 States With That Granted The Highest Number of Permits",
                     #caption = "Source: The National Instant Criminal Background Check System, Gun Violence Data courtesy of gunviolencearchive.org, Population data from Census.gov"
                             ) +
              scale_y_continuous(labels = comma)
        }
        )
        
        
        output$Text_drop_down <- renderText({
          "Source: The National Instant Criminal Background Check System, Gun Violence Data courtesy of gunviolencearchive.org, Population data from Census.gov"
        })

# Here, I define my output$Text as renderText and list the text I want in
# quotes. Remember that I use output$Text in order to be able to call the 
# output in my ui. 
        
        
    }
    

# Here, I call shinyApp and define my ui as the ui I defined in my code
# and the server as the server that I also defined in my code. 

shinyApp(ui = ui, server = server)




