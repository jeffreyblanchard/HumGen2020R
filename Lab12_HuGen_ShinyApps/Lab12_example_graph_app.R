#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

### Preparing the times series data

time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths")
time_series_recovered_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region") %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Recovered")
# Create Keys 
time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)
time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Deaths)
time_series_recovered_long <- time_series_recovered_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Recovered)
# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))
time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
    select(-Key)
# Reformat the data
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)
# Create Report table with counts
time_series_long_joined_counts <- time_series_long_joined %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date),
                 names_to = "Report_Type", values_to = "Counts")

# rename the data
global_time_series <- time_series_long_joined

# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)

# Define reporting types
Report_Type = c("Confirmed", "Deaths", "Recovered")

# Create list of countries
Countries = global_time_series$Country_Region

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("Example Graphs from COVID-19 Reporting data"),
    p("Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
      tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
    ),
    tags$br(),
    tags$hr(),  # Adds line to page
    
    sidebarLayout(
        sidebarPanel(
            # Select Country
            selectInput("select_country", 
                        label = "Country", 
                        choices = Countries),
            # Select Reporting type
            selectInput("select_type", 
                        label = "Report Type", 
                        choices = Report_Type, 
                        selected = "Confirmed"),
            # Date range
            dateRangeInput("dates", label = "Date range", start = first_date)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plot1")
        )
    )
)

# Define server logic required to make the plot
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({
        # Graph specific code
        pick_country <- global_time_series %>% 
            group_by(Country_Region,Date) %>% 
            summarise_at(c("Confirmed", "Deaths", "Recovered"), sum) %>% 
        # Here is where we select the country
            filter (Country_Region == input$select_country)
        
        # Note that aes_strings was used to accept y input and needed to quote other variables
        ggplot(pick_country, aes_string(x = "Date",  y = input$select_type, color = "Country_Region")) + 
            geom_point() +
            geom_line() +
            # Here is where the dates are used to set limits for x-axis
            xlim(input$dates) +
            ggtitle("JHU COVID-19 data for reporting type:", input$select_type)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
