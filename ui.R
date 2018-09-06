#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

    
navbarPage("WineDiscovery", id="nav",
           
           tabPanel("Introduction",
                    titlePanel("Wine Not?"),
                      mainPanel(
                        HTML(
                          paste(
                            h3("Have you ever walked into a wine store and not know what to buy for that dinner party?"),
                            h3("My Objective: To provide the user insights about wine, based on the data from my kaggle dataset - such as points, prices, reviews, vintage year, and wineries."),
                            h3("Questions I would like to provide insights:"),
                            h3("-What matters more, price or rated points, what do the wine experts have to say? Is there a correlation?"),
                            h3("-Does older wine necessarily mean it’s better?"),
                            h3("-Where will you get the best value for your money?"),
                            h3("Let's see what the Data tells us!!!")
                          )
                     )
                )
           ),
           tabPanel("Summary",
              # sidebarPanel(
              #   selectInput('category', "Select Category", c('Country','Province','Variety','Winery')),
              #   selectInput('value',"Select Value",c('Price','Points'))
              # ),          
            # mainPanel(
               #plotlyOutput('bubbleChart')
               
            # )
            plotlyOutput('bubbleChart',height = 700)
           ),
           #sidebarPanel(tags$img(src = "winepic.jpg",height = 50, width = 100)),
           
           tabPanel("Wine Visuals",
                    sidebarPanel(
                      selectInput("histCountry", "Country", c("All countries"="", sort(unique(as.character(cleantable$Country)))), multiple=TRUE),
                      selectInput("histProvince", "Province", c("All provinces"="", sort(unique(as.character(cleantable$Province)))), multiple=TRUE),
                      selectInput("histRegion1", "Region1", c("All region_1"="", sort(unique(as.character(cleantable$Region1)))), multiple=TRUE),
                      selectInput("histRegion2", "Region2", c("All region_2"="", sort(unique(as.character(cleantable$Region2)))), multiple=TRUE),
                      selectInput("histWinery", "Winery", c("All wineries"="", sort(unique(as.character(cleantable$Winery)))), multiple=TRUE),
                      selectInput("histVariety", "Variety", c("All varieties"="", sort(unique(as.character(cleantable$Variety)))), multiple=TRUE)
                    ),
                    mainPanel(
                      plotOutput('priceGraph'),
                      plotOutput('hist')
                      #plotOutput('wordCloud')
                    )
           # ),
           # tabPanel("bChart",
           #          sidebarPanel(
           #            selectInput("histCountry", "Country", c("All countries"="", sort(unique(as.character(cleantable$Country)))), multiple=TRUE),
           #            selectInput("histProvince", "Province", c("All provinces"="", sort(unique(as.character(cleantable$Province)))), multiple=TRUE),
           #            selectInput("histRegion1", "Region1", c("All region_1"="", sort(unique(as.character(cleantable$Region1)))), multiple=TRUE),
           #            selectInput("histRegion2", "Region2", c("All region_2"="", sort(unique(as.character(cleantable$Region2)))), multiple=TRUE),
           #            selectInput("histWinery", "Winery", c("All wineries"="", sort(unique(as.character(cleantable$Winery)))), multiple=TRUE),
           #            selectInput("histVariety", "Variety", c("All varieties"="", sort(unique(as.character(cleantable$Variety)))), multiple=TRUE)
           #          ),
           #          mainPanel(
           #            plotOutput('bubblechart'),
           #          
           #          )
           ),
           tabPanel("Word Cloud",
                  sidebarPanel(
                    selectInput("cloudCountry", "Country", c("All countries"="", sort(unique(as.character(vtable$Country)))), multiple=TRUE),
                    selectInput("cloudProvince", "Province", c("All provinces"="", sort(unique(as.character(vtable$Province)))), multiple=TRUE),
                    selectInput("cloudRegion1", "Region1", c("All region_1"="", sort(unique(as.character(vtable$Region1)))), multiple=TRUE),
                    selectInput("cloudRegion2", "Region2", c("All region_2"="", sort(unique(as.character(vtable$Region2)))), multiple=TRUE),
                    selectInput("cloudWinery", "Winery", c("All wineries"="", sort(unique(as.character(vtable$Winery)))), multiple=TRUE),
                    selectInput("cloudVariety", "Variety", c("All varieties"="", sort(unique(as.character(vtable$Variety)))), multiple=TRUE)
             ),
             mainPanel(
               plotOutput('wordCloud')
             )
           ),
           tabPanel("Data explorer",
                    fluidRow(
                      column(2,
                             selectInput("country", "Country", c("All countries"="", sort(unique(as.character(cleantable$Country)))), multiple=TRUE)
                      ),
                      column(2,
                             selectInput("province", "Province", c("All provinces"="", sort(unique(as.character(cleantable$Province)))), multiple=TRUE)
                      ),
                      column(2,
                             selectInput("region_1", "Region1", c("All Region1"="", sort(unique(as.character(cleantable$Region1)))), multiple=TRUE)
                      ),
                      column(2,
                             selectInput("region_2", "Region2", c("All Region2"="", sort(unique(as.character(cleantable$Region2)))), multiple=TRUE)
                      ),
                      column(2,
                             selectInput("winery", "Winery", c("All wineries"="",sort(unique(as.character(cleantable$Winery)))), multiple=TRUE)
                      ),
                      column(2,
                             selectInput("variety", "Variety", c("All Varieties"="",sort(unique(as.character(cleantable$Variety)))), multiple=TRUE)
                      )
                    ),
                    fluidRow(
                      column(4,
                             #numericInput("minPoints", "Min points", min=0, max=100, value=0)
                             sliderInput("rangePoints", label = "Points Range", min = 80, 
                                         max = 100, value = c(60, 80))
                      ),
                      column(4,
                             #numericInput("minPrice", "Min price", min=0, max=100, value=0)
                             sliderInput("rangePrice", label = "Price Range", min = 0, 
                                         max = 200, value = c(10, 50))
                      )
                      
                    ),
                    hr(),
                    DT::dataTableOutput("winetable")
           )
           
)
