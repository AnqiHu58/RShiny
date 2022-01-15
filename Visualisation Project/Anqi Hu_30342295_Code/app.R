#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(leaflet)
library(rworldmap)
library(dplyr)
library(tidyverse)
library(inspectdf)
library(hrbrthemes)
library(viridisLite)
library(viridis)
library(spData)
library(sf)
library(mapdata)
library(leaflet)
library(countrycode)
mapData <- world[c(2,11)]

data <-
    read_csv("DP_LIVE_10012020131736460 Unemployment Rate OECD.csv")
cons <- data$LOCATION %>% as.factor() %>% levels()
data <-
    data %>% filter(LOCATION != 'EA19' &
                        LOCATION != 'EU28' & LOCATION != 'OECD')
data$LOCATION <-
    countrycode(data$LOCATION, origin = 'iso3c', destination = 'country.name')
countries <- left_join(data, mapData, c("LOCATION" = "name_long"))

Q1 <- read_csv("q1.csv")
Q1.1 <- read_csv("Q1.1.csv")
agg_data <- read_csv("agg_data.csv")
emp <- read_csv("emp.csv")


monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}


ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "COVID-19", titleWidth = 200),
    dashboardSidebar(width = 200, sidebarMenu(
        menuItem(
            "Cases",
            tabName = "dashboard",
            icon = icon("lungs-virus")
        ),
        menuItem("HDI", tabName = "widgets", icon = icon("balance-scale-left")),
        menuItem("Map", tabName = "map", icon = icon("globe-asia"))
    )),
    dashboardBody(tabItems(
        # First tab content
        tabItem(
            tabName = "dashboard",
            fluidRow(
                # A static infoBox
                infoBox(
                    p("Total Cases", style = "color:#FFB90F"),
                    subtitle = '2021-5-30',
                    169643590,
                    icon = icon("lungs-virus"),
                    color = "yellow"
                ),
                infoBox(
                    p("Total Recovered", style = "color:#458B74"),
                    subtitle = '2021-5-30',
                    151475435,
                    icon = icon("head-side-mask"),
                    color = "green"
                ),
                infoBox(
                    p("Total Cases", style = "color:#FF3030"),
                    subtitle = '2021-5-30',
                    3525259,
                    icon = icon("procedures"),
                    color = "red"
                )
            ),
            fluidRow(box(
                height = 120,
                width = 6,
                h4('COVID-19 Affect', style = "color: #FF6347"),
                p(
                    'The world experienced a disaster in 2020, with the emergence of 
                    COVID-19 affecting the entire globe to varying degrees. Nearly 
                    150 countries closed all schools and forced the cancellation of 
                    activities during the outbreak.',
                    style = "color: #FF6347"
                )
            ),
            box(height = 120, width = 6,
                h4('COVID-19 With Economics', style = "color: #458B74"),
                p('Based on the above analysis of the problem, 
                  the higher the per capita GDP of a country, 
                  the more capable it is to deal with an epidemic like COVID-19.',
                  style = "color: #458B74")),
            
            box(height = 160, width = 12,
                h4('What is HDI and STI?', style = "color: #FFA500"),
                p('The Human Development Index is the ultimate criterion for evaluating the development of a country. The higher human 
                development index, the better the chances of citizens to survive the COVID-19 pandemic.',
                style = "color: #FFA500"),
                p('The Stringency Index is a number from 0 to 100 that reflects these indicators. A higher index score indicates a higher 
                level of stringency. The value of STI can show the degree of control of a country, which also directly affects the total 
                death and total case during COVID-19.',
                style = "color: #FFA500"))
            ),
            fluidRow(
                box(
                    title = "Controls",
                    selectInput(
                        "select1",
                        "Select Country:",
                        choices = Q1.1$location %>% as.factor() %>% levels(),
                        selected = Q1.1$location %>% as.factor() %>% levels(),
                        multiple = T
                    ),
                    width = 2,
                    height = 432
                ),
                tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1",
                    height = "432px",
                    tabPanel("Tab1",  plotlyOutput("tolip1", height = 365)),
                    tabPanel("Tab2", plotlyOutput("tolip2", height = 365)),
                    width = 6
                ),
                box(plotlyOutput("tolip3", height = 408), width = 4),
            )
        ),
        
        # Second tab content
        tabItem(
            tabName = "widgets",
            h2("Comparison between a Country's GDP per Capita and HDI", style = "color: #AB82FF"),
            fluidRow(
                box(height = 100, width = 12,
                    h4("Tips:", style = "color: #CD96CD"),
                    p('GDP per Cap represents the per capita GDP (gross domestic product) of a country.
                  It refers to the money a country can use to provide benefits to its citizens. 
                  The higher the GDP per capita of a country, the more capable it is to deal with 
                  an epidemic like COVID-19.', style = "color:#CD96CD")),
                box(plotlyOutput("bub", height = 562), width = 9),
                box(
                    title = "Controls",
                    selectInput(
                        "select2",
                        "Select Country:",
                        choices = agg_data$location %>% as.factor() %>% levels(),
                        multiple = T
                    ),
                    sliderInput(
                        "DatesMerge",
                        "Dates:",
                        min = as.Date("2019-12-31"),
                        max = as.Date("2020-10-19"),
                        value = as.Date("2019-12-31"),
                        timeFormat = "%Y-%m-%d"
                    ),
                    width = 3,
                    height = 582
                ),
               
            )
        ),
        
        # Third tab content
        tabItem(tabName = "map",
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = 'Unemployment Rate in 2020',
                            plotOutput('map_emp'),
                            height = 480,
                            width = 12
                        ),
                        box(height = 100, width = 12,
                            h4("Tips:", style = "color:#FF82AB"),
                            p('The darker the red show in the map, the higher the proportion 
                            of people who are unemployed. Conversely, the lighter the red, the 
                            lower the unemployment rate.', style = "color:#FF82AB")
                            ),
                        box(
                            sliderInput(
                                "slider1",
                                label = "Year Select",
                                min = as.Date.character("2020-01-01"),
                                max = as.Date.character("2020-10-01"),
                                value =  as.Date.character("2020-01-01"),
                                timeFormat = "%Y-%m",
                                step = 60,
                                animate = T
                            ),
                            width = 12
                        ),
                        
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Unemployment Rate from 1953-2018",
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset1",
                            height = "440px",
                            
                            leafletOutput('leaf1', height = 380),
                            width = 12
                        ),
                        #cons
                        box(column(
                            6, radioButtons(
                                "radio",
                                h3("Gender Select"),
                                choices = list(
                                    "MEN" = 'MEN',
                                    "WOMEN" = 'WOMEN',
                                    "TOT" = 'TOT'
                                ),
                                selected = 'TOT'
                            ),
                        ),
                        column(
                            6,
                            sliderInput(
                                "slidermap",
                                label = "Year Select",
                                min = 1953,
                                max = 2018,
                                value =  2015,
                                step = 3,
                                animate = T
                            ),
                        ),
                        width = 12)
                    )
                ))
    ))
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    output$plot1 <- renderPlot({
        hist(data)
    })
    
    output$tolip1 <- renderPlotly({
        if (!is.null(input$select1)) {
            Q1 <- Q1 %>% filter(location %in% input$select1)
        }
        p <- ggplot(Q1, aes(x = location, y = total_cases)) +
            geom_segment(aes(
                x = location,
                xend = location,
                y = 0,
                yend = total_cases
            ),
            color = "grey") +
            geom_point(color = "orange", size = 4) +
            theme_light() +
            theme(
                panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_text(angle = 60,  size = 6),
                axis.title.x = element_text(size = 10),
                axis.title.y = element_text(size = 10),
                plot.title = element_text(size = 10)
            ) +
            xlab("Country") +
            ylab("Total Cases") +
            ggtitle("Top10 Countries Covid-19 Cases")
        ggplotly(p)
    })
    
    output$tolip2 <- renderPlotly({
        if (!is.null(input$select1)) {
            Q1 <- Q1 %>% filter(location %in% input$select1)
        }
        p <- ggplot(Q1, aes(x = location, y =  total_deaths)) +
            geom_segment(aes(
                x = location,
                xend = location,
                y = 0,
                yend = total_deaths
            ),
            color = "grey") +
            geom_point(color = "orange", size = 4) +
            theme_light() +
            theme(
                panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_text(angle = 60,  size = 6),
                axis.title.x = element_text(size = 10),
                axis.title.y = element_text(size = 10),
                plot.title = element_text(size = 10)
            ) +
            xlab("Country") +
            ylab("Total Deaths") +
            ggtitle("Top10 Countries Covid-19 Deaths")
        ggplotly(p)
    })
    
    output$tolip3 <- renderPlotly({
        if (!is.null(input$select1)) {
            Q1.1 <- Q1.1 %>% filter(location %in% input$select1)
        }
        p <- ggplot(Q1.1, aes(x = location, y = sti)) +
            geom_segment(aes(
                x = location,
                xend = location,
                y = 0,
                yend = sti
            ), color = "blue") +
            geom_point(color = "orange", size = 4) +
            theme_light() +
            theme(
                panel.grid.major.x = element_blank(),
                axis.text.x = element_text(angle = 60,  size = 6),
                plot.title = element_text(size = 8),
                panel.border = element_blank(),
                axis.ticks.x = element_blank()
            ) +
            xlab("Country") +
            ylab("Stringency Index") +
            ggtitle("Stringency Index of the 10 hardest-hit countries")
        ggplotly(p)
    })
    
    
    
    output$bub <- renderPlotly({
        #print(agg_data)
        if (!is.null(input$select2)) {
            agg_data <- agg_data %>% filter(location %in% input$select2)
        }
        
        one <- agg_data %>%
            filter(date == input$DatesMerge) %>%
            arrange(desc(pop)) %>%
            ggplot(aes(
                x = gdp,
                y = hdi,
                size = pop,
                fill = location
            )) +
            geom_point(alpha = 0.5,
                       shape = 21,
                       color = "black") +
            scale_size(range = c(.1, 24), name = "Population (M)") +
            scale_fill_viridis(discrete = TRUE,
                               guide = FALSE,
                               option = "A") +
            theme_ipsum() +
            theme(legend.position = "bottom") +
            ylab("HDI") +
            xlab("GDP") +
            theme(legend.position = "none")
        one + theme(
            plot.title = element_text(
                color = "black",
                size = 5,
                face = "bold"
            ),
            axis.title.x = element_text(
                color = "blue",
                size = 10,
                face = "bold"
            ),
            axis.title.y = element_text(
                color = "#993333",
                size = 10,
                face = "bold"
            )
        )
        ggplotly(one)
        
    })
    
    sliderMonth <- reactiveValues()
    observe({
        full.date <- as.POSIXct(input$slider1, tz = "GMT")
        sliderMonth$Month <- as.character(monthStart(full.date))
    })
    
    output$map_emp <- renderPlot({
        emp_con <- emp %>% filter(TIME == sliderMonth$Month)
        emp_con_map <-
            joinCountryData2Map(emp_con,
                                joinCode = 'NAME',
                                nameJoinColumn = 'LOCATION')
        colourPalette <- brewer.pal(7, 'Reds')
        mapCountryData(
            emp_con_map,
            nameColumnToPlot = 'unemployment',
            mapTitle = '',
            catMethod = "fixedWidth",
            colourPalette = colourPalette
        )
    })
    
    output$leaf1 <- renderLeaflet({
        countries <- countries %>%
            filter(TIME == input$slidermap) %>%
            filter(SUBJECT == input$radio)
        
        map <- leaflet() %>%
            addTiles() %>% setView(0, 0, 1)
        
        # Add polygons to map
        map %>% addPolygons(data = countries$geom)
        
        pal <-
            colorNumeric(palette = "YlOrRd", domain = countries$Value)
        
        map_labels <- paste("Unemp Rate of",
                            countries$LOCATION,
                            "is",
                            round(countries$Value, 1))
        
        
        map %>% addPolygons(
            data = countries$geom,
            fillColor = pal(countries$Value),
            fillOpacity = .7,
            color = "grey",
            weight = 1,
            label = map_labels,
            labelOptions = labelOptions(textsize = "12px")
        ) %>%
            
            # Add legend to map
            addLegend(pal = pal,
                      values = countries$Value,
                      position = "bottomleft")
    })
    
    output$plot1 <- renderPlot({
        
    })
    
    output$plot1 <- renderPlot({
        
    })
    
}

shinyApp(ui, server)
