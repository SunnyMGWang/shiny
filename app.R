
library(dplyr)
library(magrittr)
library(tidyr)
library(maps)
library(ggvis)
library(shiny)
library(RColorBrewer)

pop <- read.csv("data/population.csv", skip = 3, nrow = 4524)
states <- read.csv("data/states.csv", nrow = 51)

# Tidy the data, so that the yearly population that spreads out are collected in year (key) and population (value)
pop_tidy <- pop %>%
  gather(7:34, key = "year", value = "population") %>%
  separate("year", into = c("nature", "year")) %>%
  select(-1:-3, -5) %>%
  filter(State != "US")
pop_tidy$State <- factor(pop_tidy$State)
pop_tidy <- inner_join(states, pop_tidy) 

# Create two data frames for further analysis: pop_state_total and pop_state_age
pop_state_total <- filter(pop_tidy, Age == "Total")
# After filtering, the Age column is still factor with 87 levels, so I turn the column into characters
pop_state_total$Age <- as.character(pop_state_total$Age)
pop_state_age <- filter(pop_tidy, Age != "Total")
pop_state_age$Age <- factor(pop_state_age$Age, levels = c(0:84, "85+"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("US Population Projection"),
   
   # Fluid page with tab panels
   fluidPage(

         tabsetPanel(
           tabPanel("Population Map", 
                    sidebarLayout(
                      sidebarPanel(
                        p("This interactive toolbox explores the US population projection by state and by age from 2004 to 2030 based on 2000 census."),
                        br(),
                        p("It aims to answer the following questions:"),
                        p("* Which state has the largest population?"),
                        p("* What is the largest age group in the US?"),
                        p("* What is the total population in the US in a given year?"),
                        p("* How does the population evolve over time?"),
                        br(),
                        p("Play around with the widgets and hover over the plots to get more information!"),
                        hr(),
                        p(strong("Data Source: "), "The data is from", a(href="https://www.census.gov/population/projections/data/state/projectionsagesex.html", "United States Census Bureau")),
                        width = 3
                      ),
                      mainPanel(ggvisOutput("pop_map"), 
                                p("Note: This population map presents the 2000 census data."))
                    )),
           tabPanel("Age Distribution", 
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year", "Projection year:",
                                    min = 2004, max = 2030, value = 2004, animate = TRUE),
                        hr(),
                        helpText("Note: The bar for Age: 85 represents population aged 85 and 85+."),
                        width = 3
                      ),
                      mainPanel(
                        uiOutput("agebar_ui"),ggvisOutput("agebar")
                        
                      )
                    )),
           tabPanel("Annual Population", 
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("state", "Choose state(s):", choices = c("US", as.character(states$Name))),
                        hr(),
                        helpText("Note: The first bar in red is for 2000 census data."),
                        width = 3
                      ),
                      mainPanel(
                        uiOutput("yearbar_ui"),ggvisOutput("yearbar"))
                    )),
           tabPanel("Population Evolvement", 
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("year", "Projection year:",
                                    min = 2004, max = 2030, value = 2004, animate = TRUE),
                        br(),
                        radioButtons("us_vs", "Show US Total or Compare States:",
                                     c("US Total" = "us", "Compare States" = "vs")),
                        selectizeInput("states", "If you choose to Compare States, please specify the state(s):",
                                       choices = states$Name, multiple = TRUE, 
                                       selected = c("California", "New York")),
                        hr(),
                        helpText("Note: The bar for Age: 85 represents population aged 85 and 85+."),
                        width = 3
                      ),
                      mainPanel(uiOutput("ribbon_ui"), ggvisOutput("ribbon"))
                    ))
         )
  
   )
)

# Define server logic required to draw plots
server <- function(input, output) {
# First Plot -----------------------------------------------------------  
  # Load US states map data with ggplot2's map_data()
  us_map <- map_data("state") %>% select(1:5)
  
  # To merge us_map with pop_state_total, create a Name column for states with first letter capitalized in us_map that can match the Name column in pop_state_total
  us_map$Name <- tools::toTitleCase(us_map$region)
  pop_state_total_2000 <- pop_state_total %>% filter(year == "2000")
  pop_map <- merge(us_map, pop_state_total_2000, all.x = TRUE)
  # Create the value to show when hovered over the map
  # I only want to show state name and population, which is the 1st and 4th item in the list, so I use c(1, 4)
  pop_values <- function(x) {
    paste0(names(x)[c(1, 4)], ": ", format(x, big.mark=",", trim=TRUE)[c(1, 4)], collapse = "<br />")
  } 
  
  # Create the map
  pop_map %>%
    ggvis(~long, ~lat) %>%
    group_by(Name) %>%
    layer_paths(fill = ~population, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    hide_axis("x") %>% 
    hide_axis("y") %>%
    scale_numeric("fill", range = c("white", "#3182bd")) %>%
    add_tooltip(pop_values, "hover") %>%
    set_options(width=800, height=450, keep_aspect=TRUE) %>%
    bind_shiny("pop_map")

# Second Plot -----------------------------------------------------------  
  pop_agebar <- reactive({
    pop_state_age %>% 
      filter(year == input$year)
  })
  pop_agebar %>% 
    filter(year == input$year) %>%
    ggvis(x = ~as.numeric(Age), y = ~population) %>%
    layer_bars(fill :="#81a9f8", opacity := 0.9,
               strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_axis("x", title = "Age", subdivide = 4, 
             values = seq(0, 85, by = 5)) %>%
    add_axis("y", title = "Population", title_offset = 80) %>%
    add_tooltip(function(df) paste(c("Age:", "Population:"), 
                                   c(df$xmin_ - 0.5, format(df$stack_upr_, big.mark=",", trim=TRUE)),
                                   collapse = "<br />")) %>%
    set_options(width=900, height=500, keep_aspect=TRUE)%>%
    bind_shiny("agebar", "agebar_ui")
  
# Third Plot -----------------------------------------------------------
  pop_yearbar <- reactive({
    if(input$state == "US") {
      pop_state_total %>%
        group_by(year, nature) %>%
        summarise(population = sum(population)) 
      
    } else {
      pop_state_total %>%
        filter(Name == input$state)}
  })
  
  pop_yearbar %>%
    ggvis(~year, ~population) %>%
    layer_bars(fill = ~nature, opacity := 0.9, 
               strokeOpacity := 0.5, strokeWidth := 0.25)%>%
    scale_nominal("fill", range = c("#ff7879", "#81a9f8")) %>%
    add_axis("x", subdivide = 1, values = c(2000, seq(2004, 2030, by = 2))) %>%
    add_axis("y", title = "Population", title_offset = 80) %>%
    add_tooltip(function(df) paste("Total population:", format(df$stack_upr_, big.mark=",", trim=TRUE))) %>%
    set_options(width=900, height=500, keep_aspect=TRUE)%>%
    bind_shiny("yearbar", "yearbar_ui")
  
# Fourth Plot -----------------------------------------------------------
  
  pop_ribbon <- reactive({
    if (input$us_vs == 'us') {
    pop_state_age %>%
      filter(year == input$year) %>%
      group_by(Age) %>% 
      arrange(Age) %>%
      mutate(to = cumsum(population), from = c(0, to[-n()]))
    } else {
      pop_state_age %>%
        filter(year == input$year) %>%
        group_by(Age) %>% 
        arrange(Age) %>%
        filter(Name %in% input$states) %>%
        mutate(to = cumsum(population), from = c(0, to[-n()]))
    }
  })
  
  pop_ribbon %>%
    ggvis(x = ~Age) %>%
    group_by(Name) %>%
    layer_ribbons(y = ~from, y2 = ~to, 
                  fill = ~Name, opacity = ~population) %>%
    hide_legend("fill") %>%
    add_tooltip(function(df) df$Name) %>%
    add_axis("x", subdivide = 4, values = c(seq(0, 84, by = 5), "85+"),
             tick_size_major = 10, tick_size_minor = 0) %>%
    add_axis("y", title = "population", title_offset = 80) %>%
    scale_nominal("fill", range = c("#81a9f8", "#ff7879")) %>%
    scale_numeric("opacity", range = c(0.2, 1)) %>%
    set_options(width=900, height=500, keep_aspect=TRUE)%>%
    bind_shiny("ribbon", "ribbon_ui")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

