library(shiny)
library(plotly)
library(shinythemes)

setwd("C:/Users/illus/Documents/GitHub/324-Indiv-Project/Cleaned Datasets/")

# read datasets

gdp_income_fee_size <- read.csv("Netflix subscription fee Dec-2021 with GDP and Income Dis 2019.csv")

# change some columns from character to string to properly display graph

gdp_income_fee_size$X..of.Subscribers.Q4.2021..Estimate. <- as.numeric(gsub(",", "", gdp_income_fee_size$X..of.Subscribers.Q4.2021..Estimate.))

gdp_income_fee_size$Q4.2021.Revenue....Estimate. <- as.numeric(gsub(",", "", gdp_income_fee_size$Q4.2021.Revenue....Estimate.))

# remove outliers for utility in shiny app

gdp_income_fee_size_scat_out <- filter(gdp_income_fee_size, Country != "United States")

gdp_income_fee_size_bar <- filter(gdp_income_fee_size, Country != "Switzerland")

gdp_income_fee_size_bar_out <- filter(gdp_income_fee_size_bar, Country != "South Africa")

genre <- read.csv("Popular Genres in Different Countries Netflix.csv")

tree <- read.csv("No Center Sunburst.csv")

countries <- read.csv("Country Tree Map.csv")

country_list <- filter(countries, is.na(parents))

ui <- navbarPage(theme = shinytheme("united"), 
                 
                 # title
                 
                 "Netflix Data",

                 tabPanel("Scatterplot",
                          sidebarLayout(
                            
                            # Sidebar with a slider input
                            sidebarPanel(
                              # y axis choices to compare with GDP
                              
                              selectInput("select", label = h3("Y-Axis Variables"), 
                                          choices = list("Q4 2021 Netflix Revenue Estimate ($)" = "Q4.2021.Revenue....Estimate.", "Q4 2021 Netflix Subscription Estimate" = "X..of.Subscribers.Q4.2021..Estimate.","Total Library Size" = "Total.Library.Size", "Basic Netflix Subscription Price" = "Cost.Per.Month...Basic....", "Standard Netflix Subscription Price"= "Cost.Per.Month...Standard....","Premium Netflix Subscription Price" = "Cost.Per.Month...Premium...."), 
                                          selected = 1
                              ),
                              # show United States?
                              checkboxInput("outlierscatter", "Show outlier", FALSE)
                            ),
                          mainPanel(
                            plotlyOutput("scatPlot")
                          )
                          )
                 ),
                 tabPanel("Income Disparity",
                          # graph title
                          h3("Income Disparity and Differences in Basic, Standard, and Premium Netflix Subscription Prices (Monthly)"),
                          sidebarPanel(
                            # show South Africa?
                            checkboxInput("outlierbar", "Show outlier", FALSE)
                          ),
                          mainPanel(
                            plotlyOutput("barPlot")
                          )
                 ),
                 tabPanel("Popular Genres",
                          # two panels, one for world data, one for country specific
                          tabsetPanel(
                            tabPanel("World", 
                                     h3("Genre Popularity Worldwide"),
                                     h5("Based on Number of Times a Movie/TV-Show of a Certain Genre has been in the Weekly Top 10 of Netflix in a Country (June 2021-March 2022)"),
                                     plotlyOutput("treePlot")
                                     ), 
                            tabPanel("Country", 
                                     sidebarLayout(
                                       # Sidebar with a slider input
                                       sidebarPanel(
                                         # differnet countries to choose from
                                         selectInput("Country", label = h3("Country"), 
                                                     choices = country_list$labels, 
                                                     selected = 1
                                         ),
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         h3("Genre Popularity in Country"),
                                         h5("Based on Number of Times a Movie/TV-Show of a Certain Genre has been in the Weekly Top 10 of Netflix in a Country (June 2021-March 2022)"),
                                         plotlyOutput("countryPlot")
                                       )
                                     )
                                     ), 
                          )
                          ),
                 tabPanel("Netflix Revenue and Subscribers",
                          sidebarLayout(
                            
                            # Sidebar with a slider input
                            sidebarPanel(
                              # choose scale
                              selectInput("select3", label = h3("Scale"), 
                                          choices = list("Q4 2021 Netflix Revenue Estimate ($)" = "Q4.2021.Revenue....Estimate.", "Q4 2021 Netflix Subscription Estimate" = "X..of.Subscribers.Q4.2021..Estimate."),
                                          selected = 1
                              ),
                              # show United States?
                              checkboxInput("outliermap", "Show outlier", FALSE),
                              
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotlyOutput("mapPlot"),
                              # scatterplot showing revenue and subscription relation
                              h3("Netflix Revenue to Netflix Subscription"),
                              plotlyOutput("mapscatPlot")
                            ),
                            
                          )
                 )
)

# Server logic
server <- function(input, output) {
  # y-axis flexible GDP scatter plot
  output$scatPlot <- renderPlotly({
    # show outlier or not
    if (input$outlierscatter){
      dfs <- gdp_income_fee_size
    } else {
      dfs <- gdp_income_fee_size_scat_out
    }
    # scatter plot function
    fig <- plot_ly(data = dfs, x = ~X2019.GDP..World.Bank., y = ~get(input$select), type= "scatter", mode = "markers", text= ~Country)
    fig <- fig %>% layout(yaxis = list(title = 'Y-Variable'), xaxis = list(title = 'GDP (Current US $)'))
    fig
    })
  # income disparity bar graph
  output$barPlot <- renderPlotly({
    # show outlier or not
    if (input$outlierbar){
      dfb <- gdp_income_fee_size_bar
    } else {
      dfb <- gdp_income_fee_size_bar_out
    }
    #bar plot function
    fig <- plot_ly(dfb, x = ~gini_disp, y = ~Cost.Per.Month...Basic...., type = 'bar', name = 'Basic', text = ~Country)
    # adding layers
    fig <- fig %>% add_trace(y = ~basic_standard_diff, name = 'Standard')
    fig <- fig %>% add_trace(y = ~standard_premium_diff, name = 'Premium')
    # layout
    fig <- fig %>% layout(yaxis = list(title = 'Monthly Cost of Basic, Standard, and Premium Membership (Dollars)', titlefont = list(size=10)), xaxis = list(title = 'Income Disparity (GINI)'), barmode = 'stack')
    fig
  })
  
  # if country focused treemap
  output$countryPlot <- renderPlotly({
    # get country relavant information
    country <- filter(countries, parents == input$Country)
    
    country <- rbind(filter(countries, labels == input$Country), country)
    
    # treemap function
    fig <- plot_ly(country, ids = ~id, labels = ~labels, parents = ~parents, values = ~n, type = 'treemap', branchvalues = 'total', pathbar=list(visible= TRUE))
    
    fig
  })
  
  
  # if genre focused treemap (world)
  output$treePlot <- renderPlotly({
    # treemap function
    fig <- plot_ly(tree, ids = ~id, labels = ~label, parents = ~parent, values = ~n, type = 'treemap', branchvalues = 'total', pathbar=list(visible= TRUE))
    
    fig
  })
  
  # choropleth Map
  output$mapPlot <- renderPlotly({
    # show outlier or not
    if (input$outliermap){
      dfm <- gdp_income_fee_size
    } else {
      dfm <- gdp_income_fee_size_scat_out
    }
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    # choropleth map function
    fig <- plot_geo(dfm)
    fig <- fig %>% add_trace(
      z = ~get(input$select3), color = ~get(input$select3), colorscale = 'Purples',
      text = ~Country, locations = ~Alpha.3.code, marker = list(line = l)
    )
    # layout
    fig <- fig %>% colorbar(title = 'Scale')
    fig <- fig %>% layout(
      title = 'Q4 2021 Netflix World Map Data'
    )
    
    fig
  })
  output$mapscatPlot <- renderPlotly({
    # show outlier or not
    if (input$outliermap){
      dfms <- gdp_income_fee_size
    } else {
      dfms <- gdp_income_fee_size_scat_out
    }
    # scatter plot function
    fig <- plot_ly(data = dfms, x = ~Q4.2021.Revenue....Estimate., y = ~X..of.Subscribers.Q4.2021..Estimate., type= "scatter", mode = "markers", text= ~Country)
    # layout
    fig <- fig %>% layout(yaxis = list(title = 'Q4 2021 Netflix Subscription Estimate'), xaxis = list(title = 'Q4 2021 Netflix Revenue Estimate ($)'))
    fig
  })
}

shinyApp(ui, server)
                 