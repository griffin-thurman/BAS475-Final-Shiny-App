library(shiny)
library(fpp3)
library(readr)
library(plotly)
library(ggplot2)
library(dtplyr)
library(seasonal)
library(ggpubr)
library(shinythemes)
library(zoo)


timeseries <- aus_arrivals %>%
  filter(Origin %in% c("Japan", "NZ", "UK", "US"))

decomp <- timeseries %>%
  model(x11 = X_13ARIMA_SEATS(Arrivals ~ x11())) %>%
  components()

ui <- fluidPage(theme = shinytheme("superhero"),
  
  h1("Arrivals to Australia by Country"),
  
  h2("Plots and Interpretations"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               textOutput("intro_direct")),
      
      tabPanel("Time Series Plot",
               checkboxGroupInput(inputId = "select_ts",
                                  label = "Select Country",
                                  choices = c("US", "UK", "NZ", "Japan"),
                                  selected = "US"),
               checkboxInput(inputId = "trendline",
                             label = "Show trend line",
                             value = TRUE),
               plotlyOutput("ts_plot"),
               textOutput("ts_interp")),
      
      tabPanel("Decompostion Plot", 
               plotOutput("decomp_plot"), 
               textOutput("decomp_interp")),
      
      tabPanel("Seasonality Plot", 
               radioButtons(inputId = "select_season",
                            label = "Select Country",
                            choices = c("US", "UK", "NZ", "Japan"),
                            selected = "US"),
               plotlyOutput("seasonal_plot"), 
               textOutput("seasonal_interp")),
      
      tabPanel("AutoCorrelation Plot",
               radioButtons(inputId = "select_ACF",
                            label = "Select Country",
                            choices = c("US", "UK", "NZ", "Japan"),
                            selected = "US"),
               plotOutput("ACF_plot"), 
               textOutput("ACF_interp")),
      
      tabPanel("Subseries Plot",
                 radioButtons(inputId = "select_subseries",
                              label = "Select Country",
                              choices = c("US", "UK", "NZ", "Japan"),
                              selected = "US"),
                 plotlyOutput("subseries_plot"), 
                 textOutput("subseries_interpretation")),
      
      tabPanel("Naive Forecasting Model",
                 radioButtons(inputId = "select_naive",
                              label = "Select Country",
                              choices = c("US", "UK", "NZ", "Japan"),
                              selected = "US"),
                 plotOutput("naive_model")),
        
      tabPanel("Seasonal Naive Forecasting Model",
                 radioButtons(inputId = "select_seasonalnaive",
                              label = "Select Country",
                              choices = c("US", "UK", "NZ", "Japan"),
                              selected = "US"),
                 plotOutput("seasonalnaive_model")),
      
      tabPanel("Mean Forecasting Model",
               radioButtons(inputId = "select_mean",
                            label = "Select Country",
                            choices = c("US", "UK", "NZ", "Japan"),
                            selected = "US"),
               plotOutput("mean_model")),
      
      tabPanel("Drift Forecasting Model",
               radioButtons(inputId = "select_drift",
                            label = "Select Country",
                            choices = c("US", "UK", "NZ", "Japan"),
                            selected = "US"),
               plotOutput("drift_model"))
      
    )
  ),
)

server <- function(input, output){
  
  output$intro_direct <- renderText({
    "This shiny app is made from data found off of the Australian Arrivals timeseries dataset. The countries Australia received imports from are the United States of America, Japan, New Zealand, and United Kingdom. On most tabs you are able to switch between which country's data you would like to view in the graphic. 
    
         TABS OF CONTENTS: 
    Tab 1) Full series plotted
    Tab 2) Decomposition of the timeseries
    Tab 3) Timeseries seasonality plot
    Tab 4) Autocorrelation of the timeseries
    Tab 5) Subseries plots of the timeseries
    Tab 6) Naive forecasting model
    Tab 7) Seasonal naive forecasting model
    Tab 8) Mean forecasting model
    Tab 9) Drift forecasting model"
  })
  
  output$ts_plot <- renderPlotly({
    plot1 <- timeseries %>%
      filter(Origin == input$select_ts) %>%
      ggplot(aes(x = Quarter, y = Arrivals, color = factor(Origin))) +
      geom_line()
    
    if(input$trendline == TRUE) {
      plot1 <- plot1 + geom_smooth(method = lm)
    }
    plot1
  })
  
  output$ts_interp <- renderText({
    "Three of the four countries under observation see a drastic change in arrivals in the first quarter a little before 1990 and after 2000. At around the 1990 and 2000 mark for Japan, United States, and New Zealand they each see a quick spike followed by a slightly less gradual drop off in arrivals before rebounding. There are no obvious outliers in this plot. New Zealand shows the largest amount of arrivals by 2010. Japan and the United Kingdom were trending in a positive directoin until a decline began to occur in the 2000s. The United States showed very slight but consistent growth from 1980 to 2010. "
  })
  
  output$decomp_plot <- renderPlot({
    autoplot(decomp) +
      labs(title = "Arrivals to Australia Decomposition")
  })
  
  output$decomp_interp <- renderText({
    "New Zealand has by far the strongest, most consistent growth in trend-cycles, while the United States also saw consistent growth but not near as significant as New Zealand. Japan showed a large increase in trend-cycles until about 1997 where it started to decline back to where it started in 1980."
  })
  
  output$seasonal_plot <- renderPlotly({
    timeseries %>% 
      filter(Origin == input$select_season) %>%
      gg_season(Arrivals) +
      labs(title = "Arrivals to Australia Seasonality Graph")
  }) 
  
  output$seasonal_interp <- renderText({
    "The United States shows seasonality with their lowest Australian arrivals happening at the beginning of Q2 and their most coming at the beginning of Q4 across all 3 decades.
    Arrivals from the UK show extreme seasonality with the lowest arrivals occurring at the beginning of Q2 and Q3 before seeing a very large increase throughout Q3 going into the beginning of Q4. 
    Arrivals from New Zealand don't show seasonality until a little drop at the beginning of Q4 for all 3 decades.
    Arrivals from Japan show strong seasonality as arrivals peak at the beginning of Q1 and Q4 with the lowest arrivals coming at the beginning of Q2 and Q4. This goes for the 2000s and 2010s."
  })
  
  output$ACF_plot <- renderPlot({
    timeseries %>%
      filter(Origin == input$select_ACF) %>%
      ACF(Arrivals) %>%
      autoplot() +
      labs(title = "Autocorrelation of Arrivals to Australia")
  })
  
  output$ACF_interp <- renderText({
    "None of the changes in quartery arrivals are considered white noise.
    
    For an autocorrelation series to appear to not be white noise, there has to be more than 5% of spikes out of bounds or within the dotted line. In each plot there are 21 lags. This means that if there is more than one lag below the boundary, the plot can be considered to be white noise. 
    The United States, New Zealand, and Japan do not have any lags out of bounds. The United Kingdom has multiple lags very close to being out of bounds, but only one lag that actually is out of bounds. 
    The United Staes, New Zealand, and Japan have scallops in their respective plots indicating seasonality for the arrivals from each country."
  })
  
  output$subseries_plot <- renderPlotly({
    timeseries %>%
      filter(Origin == input$select_subseries) %>%
      gg_subseries() +
      labs(title = "Subseries Plot of Arrivals to Australia")
  })
  
  output$subseries_interpretation <- renderText({
    "The US shows consistent upward trending with a simlar amount of arrivals for each quarter. The UK plot shows significantly more arrivals during the first and fourth quarters than the second and third. Australia has seen a constant growth in the number of arrivals from Japan and New Zealand in each quarter."
  })
  
  output$naive_model <- renderPlot({
    
    naive_data <- timeseries %>%
      filter(Origin == input$select_naive)
    
    train_naive <- naive_data %>%
      filter_index("1992 Q1" ~ "2006 Q4")
    
    naive_fit <- naive_data %>%
      model(NAIVE(Arrivals)) %>%
      forecast(h = 12)
    
    naive_fit %>%
      autoplot(train_naive, level = NULL) + 
      autolayer(
        filter_index(naive_data, "2006 Q1" ~ .),
        colour = "black") +
      labs(title = "Naive Forecasting Model")
  })
  
  output$seasonalnaive_model <- renderPlot({
    
    seasonalnaive_data <- timeseries %>%
      filter(Origin == input$select_seasonalnaive)
    
    train_seasonalnaive <- seasonalnaive_data %>%
      filter_index("1992 Q1" ~ "2006 Q4")
    
    seasonalnaive_fit <- seasonalnaive_data %>%
      model(SNAIVE(Arrivals ~lag("year"))) %>%
      forecast(h = 12)
    
    seasonalnaive_fit %>%
      autoplot(train_seasonalnaive, level = NULL) + 
      autolayer(
        filter_index(seasonalnaive_data, "2006 Q1" ~ .),
        colour = "black") +
      labs(title = "Seasonal Naive Forecasting Model")
  })
  
  output$mean_model <- renderPlot({
    
    mean_data <- timeseries %>%
      filter(Origin == input$select_mean)
    
    train_mean <- mean_data %>%
      filter_index("1992 Q1" ~ "2006 Q4")
    
    mean_fit <- mean_data %>%
      model(MEAN(Arrivals)) %>%
      forecast(h = 12)
    
    mean_fit %>%
      autoplot(train_mean, level = NULL) + 
      autolayer(
        filter_index(mean_data, "2006 Q1" ~ .),
        colour = "black") +
      labs(title = "Mean Forecasting Model")
  })
  
  output$drift_model <- renderPlot({
    
    drift_data <- timeseries %>%
      filter(Origin == input$select_drift)
    
    train_drift <- drift_data %>%
      filter_index("1992 Q1" ~ "2006 Q4")
    
    drift_fit <- drift_data %>%
      model(RW(Arrivals ~ drift())) %>%
      forecast(h = 12)
    
    drift_fit %>%
      autoplot(train_drift, level = NULL) + 
      autolayer(
        filter_index(drift_data, "2006 Q1" ~ .),
        colour = "black") +
      labs(title = "Drift Forecasting Model")
  })
  
}

shinyApp(ui = ui, server = server)
