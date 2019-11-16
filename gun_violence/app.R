
library(shiny)
library(gganimate)
library(gapminder)
library(ggplot2)
library(shinythemes)

# final_gun_violence_data <- readRDS("final data/final_data.rds")

library(shinythemes)

shinyApp(
    ui = fluidPage(theme = shinytheme("flatly"),
    navbarPage("Gun Violence",
                 tabPanel("Graphics",
                          imageOutput("plot1")),
                 tabPanel("About",
                          textOutput("Text")))),
    server <- function(input, output) {
        
        output$Text <- renderText({
            "Just as the effects of gun violence are wide-reaching, impacting individuals as well as community sentiment, there is no single cause of gun violence in the United States. Therefore, I am interested in analyzing how gun violence correlates to economic injustice and the impact of community-based programs. Does data show that communities with gun violence programs actually experience a decrease in gun violence? By evaluating the statistically significant rates of gun violence compared to the population, I located the cities on which to focus my analysis.

The Federal Bureau of Investigation’s National Instant Criminal Background Check System (NICS) provides data on how many background checks were conducted in the United States. This data is converted from PDF to CSV by Buzzfeed News and is significant as firearm background checks often have a close correlation to gun sales and therefore are a good indication of a state’s gun sales. Through my data analysis, I plan to explore whether there is a connection between increased gun sales and more gun violence. 

To determine public policies related to guns, I made use of a dataset that compiles information about different state’s public policies. Students and scholars through Michigan State University’s Institute for Public Policy and Social Research worked to organize and make policy data publicly available. 

Through The Trace, a resource that publishes information related to gun violence, I acquired a data source that provides information on the state, date, number killed, number injured, and age group for over 230,000 incidents involving gun violence. This gun violence datasource is from an organization called Gun Violence Archive which finds data by combing through local and state police and other government sources that report gun violence and crime. 

To compare the number of background checks conducted, the number of public policies compared to population size, and gun violence incidents, I located a data source that provides population information for more than 28,000 United States cities and towns. This data was compiled through the use of data from the United States Census Bureau and the United States Geological Survey. 
"
        })
    }
    )







