
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
library(tidyverse)

# Reading in rds file with my final data

final_gun_violence_data <- read_rds("final_data.rds")

# After I had already read in my rds file with my final data, I decided that I
# didn't want to include the data for 2018 because I only have data for the
# first 3 months of 2018. Therefore, I filtered to exclude 2018 from my 
# "new_data" dataset. I also tried to use fct_relevel to get 2013 first
# in my drop down selection but was not successful. 

new_data <- final_gun_violence_data %>%
    filter(year != 2018) %>%
    mutate(year = fct_relevel(year, "2013", "2014", "2015", "2016", "2017"))



# Here, I'm defining my ui. First, I use the fluidPage function to define my
# shinytheme as "flatly." I also add the navbarPage function to add a title
# to my shiny app.

ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage("Tracing Factors Correlated to Gun Violence",
               
# I'm adding my tabPanel called "Drop Down Graphics" and calling the selectInput
# I defined in the server for the "chosenyear" variable. I ask the user to select 
# the year and use the unique function to get a year from my dataset. 
               
                tabPanel("Drop Down Graphics",
                         selectInput("chosenyear",
                                     "Select Year:", unique(new_data$year)),
                         
# I then use plotOuput to print my graph called "permiteighteen" as well as 
# my plot called "permitpermontheighteen."

                         plotOutput("permiteighteen"),
                         plotOutput("permitpermontheighteen")),

# Next, I define my next tabPanel as "Slider Graphics."

               tabPanel("Slider Graphics",
                        
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
                        imageOutput("graph3")),

# Next, I'm making my Regression tab.

               tabPanel("Regression",
                        
# I used plotly on my regressiongraph so I had to call plotlyOutput rather
# than just plotOutput as for my regressiongraph2. 
                        
                        plotlyOutput("regressiongraph"),
                        plotOutput("regressiongraph2")),
               
# My Regression Coefficient Plot tab calls the regressiondata output defined
# in my server. 

               tabPanel("Regression Coefficient Plot",
                        plotOutput("regressiondata")),

# Finally, I created my About tab and used the textOutput function to define
# the text that I want on my About page. 
               
               tabPanel("About",
                        textOutput("Text"))))
  

  
# This is the start of my server section, rather than the ui section.    
    
server <- function(input, output) {
    
# First, I specified the images I wanted to render for my graph.
    
    output$graph <- renderImage({
        
# Here, I specify that if the slider is at 2013 then shiny should
# display the "2013graph.png" image. I repeat this process for my
# 2014, 2015, 2016, and 2017 graphs. 
        
        if(input$currentyear==2013) year <-"2013graph.png"
        if(input$currentyear==2014) year <-"2014graph.png"
        if(input$currentyear==2015) year <-"2015graph.png"
        if(input$currentyear==2016) year <-"2016graph.png"
        if(input$currentyear==2017) year <-"2017graph.png"
        
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
        if(input$currentyear==2013) year <-"2013graph2.png"
        if(input$currentyear==2014) year <-"2014graph2.png"
        if(input$currentyear==2015) year <-"2015graph2.png"
        if(input$currentyear==2016) year <-"2016graph2.png"
        if(input$currentyear==2017) year <-"2017graph2.png"
        
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
        if(input$currentyear==2013) year <-"2013gt.png"
        
# Here I format to make sure the user can read the gt. 
        
        list(src=year,
             width = 500,
             height = 800,
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
    
        output$regressiongraph <- renderPlotly({
            ggplotly(
            permit1 %>%
                ggplot(aes(x = permit, y = n_incidents, color = state)) +
                geom_jitter(show.legend = FALSE) +
                geom_smooth(method = 'lm', se = F, col = 'black') +
                labs(x = "Number of Gun Violence Incidents", 
                     y = "Average Permits Granted Per Month",
                     title = "Impact of Permits Granted on Number of Gun Violence Incidents",
                     subtitle = "An Analysis of the 50 US States",
                     caption = "Source: The National Instant Criminal Background Check System and ") +
                scale_y_log10() +
                scale_x_log10() 
            )
            
        })
        
# This plot is a graph of my regression. I made sure to call renderPlot
# rather than renderPlotly here because my graph is a plot rather than a 
# plotly. Remember that geom_jitter AND geom_smooth are useful when creating
# regression graphs. Within geom_smooth, I specified the model as 'lm' and 
# set the color equal to 'black. 
# The labs function is always used to specify the titles, subtitles, captions
# (for the source of the data) as well as the x and y axis labels. 
        
        output$regressiongraph2 <- renderPlot({
            permit1 %>%
                ggplot(aes(x = permit, y = n_incidents, color = state)) +
                geom_jitter(show.legend = FALSE) +
                geom_smooth(method = 'lm', se = F, col = 'black') +
                labs(x = "Number of Gun Violence Incidents", 
                     y = "Average Permits Granted Per Month",
                     title = "Impact of Permits Granted on Number of Gun Violence Incidents",
                     subtitle = "An Analysis of the 50 US States",
                     caption = "Source: The National Instant Criminal Background Check System and ") 
            
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
                     y = "Number of Permits Granted",
                     title = "The Number of Gun Permits Granted Per Month",
                     subtitle = "Do Gun Permits Sold Increase in Summer Months?",
                     caption = "Source: The National Instant Criminal Background Check System")
        })
    
# I created my regression within a renderPlot and called the result
# regressiondata so I can then call regression data within my ui and
# have the result in my Shiny app. Remember that coefplot is a way
# to visualize the regression and is from the library coefplot. 
                
        output$regressiondata <- renderPlot({
            m1 <- lm(n_incidents ~ permit + population, data = permit1)
            
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
                     y = "Number of Permits Granted",
                     title = "The Number of Gun Permits Granted Per State in March",
                     subtitle = "The 10 States With That Granted The Highest Number of Permits",
                     caption = "Source: The National Instant Criminal Background Check System")
        }
        )

# Here, I define my output$Text as renderText and list the text I want in
# quotes. Remember that I use output$Text in order to be able to call the 
# output in my ui. 
        
        output$Text <- renderText({
            "Just as the effects of gun violence are wide-reaching, impacting individuals as well as community sentiment, there is no single cause of gun violence in the United States. Therefore, I am interested in analyzing how gun violence correlates to economic injustice and the impact of community-based programs. Does data show that communities with gun violence programs actually experience a decrease in gun violence? By evaluating the statistically significant rates of gun violence compared to the population, I located the cities on which to focus my analysis.

The Federal Bureau of Investigation’s National Instant Criminal Background Check System (NICS) provides data on how many background checks were conducted in the United States. This data is converted from PDF to CSV by Buzzfeed News and is significant as firearm background checks often have a close correlation to gun sales and therefore are a good indication of a state’s gun sales. Through my data analysis, I plan to explore whether there is a connection between increased gun sales and more gun violence. 

To determine public policies related to guns, I made use of a dataset that compiles information about different state’s public policies. Students and scholars through Michigan State University’s Institute for Public Policy and Social Research worked to organize and make policy data publicly available. 

Through The Trace, a resource that publishes information related to gun violence, I acquired a data source that provides information on the state, date, number killed, number injured, and age group for over 230,000 incidents involving gun violence. This gun violence datasource is from an organization called Gun Violence Archive which finds data by combing through local and state police and other government sources that report gun violence and crime. 

To compare the number of background checks conducted, the number of public policies compared to population size, and gun violence incidents, I located a data source that provides population information for more than 28,000 United States cities and towns. This data was compiled through the use of data from the United States Census Bureau and the United States Geological Survey. 
"
        })
    }
    

# Here, I call shinyApp and define my ui as the ui I defined in my code
# and the server as the server that I also defined in my code. 

shinyApp(ui = ui, server = server)




