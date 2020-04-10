#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 
# https://stackoverflow.com/questions/53318993/rendering-an-animated-plot-in-r-s
# hiny-with-ggplot-and-plotly

library(transformr)
library(gifski)
library(gganimate)
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)

# Define UI for application that renders an animated plot
ui <- fluidPage(

    # Application title
    titlePanel("Spread of COVID-19 in Hubei, China"),
    
    # Plot output
    imageOutput("hubei_plot")
)

# Define server logic required to render the plot
server <- function(input, output) {

    output$hubei_plot <- renderImage({
        
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # Read in and prepare the data
        deaths <- read_csv("time_series_19-covid-Deaths.csv") %>%
            rename(
                "province" = "Province/State" ,
                "country" = "Country/Region",
            ) %>% 
            filter(
                country == "China",
                province == "Hubei"
            ) %>% 
            select(-"Lat", -"Long") %>% 
            pivot_longer(names_to = "date",
                         values_to = "deaths",
                         cols = -c(province, country)) %>% 
            rename("date" = "date")
        
        recovered <- read_csv("time_series_19-covid-Recovered.csv") %>% 
            rename(
                "province" = "Province/State" ,
                "country" = "Country/Region",
            ) %>% 
            filter(
                country == "China",
                province == "Hubei"
            ) %>% 
            select(-"Lat", -"Long") %>% 
            pivot_longer(names_to = "date",
                         values_to = "recovered",
                         cols = -c(province, country)) %>% 
            rename("date" = "date")

        confirmed <- read_csv("time_series_19-covid-Confirmed.csv") %>% 
          rename(
            "province" = "Province/State" ,
            "country" = "Country/Region",
          ) %>% 
          filter(
            country == "China",
            province == "Hubei"
          ) %>% 
          select(-"Lat", -"Long") %>% 
          pivot_longer(names_to = "date",
                       values_to = "confirmed",
                       cols = -c(province, country)) %>% 
          rename("date" = "date")

        hubei <- confirmed %>% 
          inner_join(deaths) %>% 
          inner_join(recovered)
        
        # Create the plot with ggplot2
        hubei_plot <- hubei %>% 
          ggplot(aes(x = as.Date(date, "%m/%d/%Y"))) +
            geom_line(aes(y = confirmed, color = "Confirmed")) +
            geom_line(aes(y = deaths, color = "Deaths")) +
            geom_line(aes(y = recovered, color = "Recovered")) +
            scale_color_manual(
              "", 
              breaks = c("Confirmed", "Deaths", "Recovered"),
              values = c("black", "darkred", "steelblue")
            ) +
            labs(
              title = "COVID-19 in Hubei, China (as of March 19, 2020)",
              subtitle = "Data provided by JHU CSSE",
              x = "Time",
              y = "Count"
            ) +
            scale_x_date(
              date_breaks = "1 week", 
              date_labels = "%b %d"
            ) +
            theme_gray() +
            transition_states(as.Date(date, "%m/%d/%Y"))
    
        # Save plot as a gif to integrate into Shiny app.
        # Might have to change the nframes to speed or slow up presentation of plots
        animate(hubei_plot, nframes = 75, renderer = gifski_renderer("outfile.gif"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
