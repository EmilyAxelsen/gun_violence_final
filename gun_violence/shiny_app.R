
library(shiny)
library(gganimate)
library(gapminder)
library(ggplot2)
library(shinythemes)
library(shinythemes)
library(coefplot)
library(gt)
library(tidyverse)

final_gun_violence_data <- read_rds("final_data.rds")



ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage("Gun Violence",
                tabPanel("*Work In Progress* Graphics",
                         selectInput("chosenyear",
                                     "Select Year:", unique(final_gun_violence_data$year)),
                         plotOutput("permiteighteen"),
                         plotOutput("permitpermontheighteen")),
               tabPanel("Slider Graphics",
                        sliderInput("currentyear",
                                    "Year", min = 2013, max = 2017, value = 500),
                        imageOutput("graph")),
               tabPanel("Graphics",
                        plotOutput("thirteengraphone"),
                        plotOutput("thirteengraphtwo"),
                        plotOutput("gtgraph"),
                        plotOutput("fourteengraphone")),
               tabPanel("Regression",
                        plotOutput("regressiongraph")),
               tabPanel("Regression Coefficient Plot",
                        plotOutput("regressiondata")),
               tabPanel("About",
                        textOutput("Text"))))
    
    
    
server <- function(input, output) {
    output$fourteengraphone <- renderPlot({
        final_gun_violence_data %>%
            filter(year == 2014) %>%
            ggplot(aes(x = month, y = permit, fill = month)) + 
            geom_bar(stat = 'identity', show.legend = FALSE) +
            labs(x = "Month", 
                 y = "Number of Permits Granted",
                 title = "The Number of Gun Permits Granted Per Month in 2014",
                 subtitle = "Do Gun Permits Sold Increase in Summer Months?",
                 caption = "Source: The National Instant Criminal Background Check System")
    })
    output$graph <- renderImage({
        
        if(input$currentyear==2013) year <-"2013graph.png"
        if(input$currentyear==2014) year <-"2014graph.png"
        if(input$currentyear==2015) year <-"2015graph.png"
        if(input$currentyear==2016) year <-"2016graph.png"
        if(input$currentyear==2017) year <-"2017graph.png"
        list(src=year)
    })
    
    output$thirteengraphone <- renderPlot({
    final_gun_violence_data %>%
        filter(year == 2013) %>%
        ggplot(aes(x = month, y = permit, fill = month)) + 
        geom_bar(stat = 'identity', show.legend = FALSE) +
        labs(x = "Month", 
             y = "Number of Permits Granted",
             title = "The Number of Gun Permits Granted Per Month in 2013",
             subtitle = "Do Gun Permits Sold Increase in Summer Months?",
             caption = "Source: The National Instant Criminal Background Check System")
    }) 
    
    output$thrteengraphtwo <- renderPlot({
    permit_2013 <- final_gun_violence_data %>%
        select(state, permit, year, month) %>%
        filter(month == "06") %>%
        filter(year == 2013) %>%
        unique() %>%
        arrange(desc(permit))  %>%
        slice(1:10)
    
    
    ggplot(permit_2013, aes(x = reorder(state, permit), y = permit, fill = state)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "State", 
             y = "Number of Permits Granted",
             title = "The Number of Gun Permits Granted Per State in June, 2013",
             subtitle = "The 10 States With That Granted The Highest Number of Permits in June, 2013",
             caption = "Source: The National Instant Criminal Background Check System")
    })
    output$gtgraph <- renderPlot({
        n_incidents <- final_gun_violence_data %>%
            filter(year == 2013) %>%
            count(state) %>%
            arrange(desc(n)) %>%
            gt() %>%
            cols_label(
                n = "Number of Incidents",
                state = "State") %>%
            tab_header(title = "Number of Gun Violence Incidents In June 2013", 
                       subtitle = "The top ten states that granted the highest number of permits are formatted in \nbold text and with the same color as the 2013 gun permit bar graph.") %>%
            tab_style(
                style = list(
                    cell_fill(color = "#3ab600"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(3))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#f8766d"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(1))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#ff62bb"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(9))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#01bec4"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(8))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#01afff"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(2))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#02bf7d"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(13))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#e86bf3"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(12))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#a2a500"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(14))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#d89000"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(22))) %>%
            tab_style(
                style = list(
                    cell_fill(color = "#9690ff"),
                    cell_text(weight = "bold")
                ),
                locations = cells_data(
                    columns = vars(state, n),
                    rows = c(14)))
    })
        output$regressiongraph <- renderPlot({
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
        
        output$regressiondata <- renderPlot({
            m1 <- lm(n_incidents ~ permit + population, data = permit1)
            
            coefplot(m1)
            
        })
        
        output$permiteighteen <- renderPlot({
            
            permit_2013 <- final_gun_violence_data %>%
                select(state, permit, year, month) %>%
                filter(month == "06") %>%
                filter(year == input$chosenyear) %>%
                unique() %>%
                arrange(desc(permit))  %>%
                slice(1:10)
            
            
            ggplot(permit_2013, aes(x = reorder(state, permit), y = permit, fill = state)) +
                geom_col(show.legend = FALSE) +
                coord_flip() +
                labs(x = "State", 
                     y = "Number of Permits Granted",
                     title = "The Number of Gun Permits Granted Per State",
                     subtitle = "The 10 States With That Granted The Highest Number of Permits",
                     caption = "Source: The National Instant Criminal Background Check System")
        }
        )
        
        output$Text <- renderText({
            "Just as the effects of gun violence are wide-reaching, impacting individuals as well as community sentiment, there is no single cause of gun violence in the United States. Therefore, I am interested in analyzing how gun violence correlates to economic injustice and the impact of community-based programs. Does data show that communities with gun violence programs actually experience a decrease in gun violence? By evaluating the statistically significant rates of gun violence compared to the population, I located the cities on which to focus my analysis.

The Federal Bureau of Investigation’s National Instant Criminal Background Check System (NICS) provides data on how many background checks were conducted in the United States. This data is converted from PDF to CSV by Buzzfeed News and is significant as firearm background checks often have a close correlation to gun sales and therefore are a good indication of a state’s gun sales. Through my data analysis, I plan to explore whether there is a connection between increased gun sales and more gun violence. 

To determine public policies related to guns, I made use of a dataset that compiles information about different state’s public policies. Students and scholars through Michigan State University’s Institute for Public Policy and Social Research worked to organize and make policy data publicly available. 

Through The Trace, a resource that publishes information related to gun violence, I acquired a data source that provides information on the state, date, number killed, number injured, and age group for over 230,000 incidents involving gun violence. This gun violence datasource is from an organization called Gun Violence Archive which finds data by combing through local and state police and other government sources that report gun violence and crime. 

To compare the number of background checks conducted, the number of public policies compared to population size, and gun violence incidents, I located a data source that provides population information for more than 28,000 United States cities and towns. This data was compiled through the use of data from the United States Census Bureau and the United States Geological Survey. 
"
        })
    }
    



shinyApp(ui = ui, server = server)




