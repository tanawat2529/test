# ---- Module 10 Part 1 ----
# See slides

# ---- Module 10 Part 2 ----
# Import libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Shiny apps are composed of two pieces:
# an object called ui
# a function called server

# ---- Basic App ----
# Define the user interface (ui)
ui <- fluidPage(
  # Overall, use the "sidebar" layout, which has a side panel and a main panel
  sidebarLayout(
    # On the side panel ...
    sidebarPanel(
      # Let users select a number between 1 and 5
      selectInput(inputId = "mynumber", label = "Choose a Number", choices = 1:5)),
    # On the main panel ...
    # Display a plot that changes based on the number the user chooses
    # Define the details of this figure as an output called "myfigure"
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

# Define the server function
server <- function(input, output) {
  # Given the input called mynumber, render a plot following these directions, and
  # display it in the space where the output is called "myfigure"
  output$myfigure <- renderPlot(plot(x = input$mynumber, y = 1, xlim = c(0,6)))
}

# Run the app
shinyApp(ui, server)

# ---- Income App ----
income <- read.csv("income.csv")

# Define the ui
# You want a side panel with three interactive pieces and a main panel with the resulting figure
ui <- fluidPage(
  # App title: Module 10 - Shiny App Demo
  titlePanel("Module 10 - Shiny App Demo"),
  sidebarLayout(
    # In the sidebar panel ...
    sidebarPanel(
      # Interactive piece 1: Which countries do you want to include in the figure?
      selectInput(inputId = "subset_country", label = "Select Countries", 
                  choices = unique(income$country), multiple = TRUE),
      # Interactive piece 2: Do you want the color of the points to be based on gender or marital status?
      selectInput(inputId = "set_color", label = "Color Points by:", 
                  choices = c(Gender = "gender", MaritalStatus = "grouped_marital")),
      # Interactive piece 3: What are the minimum and maximum ages you want to include in the figure?
      sliderInput(inputId = "subset_age", label = "Age Range:", 
                  min = min(income$age), max = max(income$age), value = c(25, 75))),
    # In the main panel ...
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

# Define server function
server <- function(input, output) {
  # 1. Create the subset that will be used in the figure
  # This will be based on the input from "subset_age" and "subset_country"
  # This subset will be reactive to changes in the input that the users provides
  create_subset <- reactive(income %>%
                                     # Only include capital gain between 1 and 50000
                              filter(between(capital_gain, 1, 50000) &
                                     # Only include the selected countries
                                     country %in% input$subset_country &
                                     # Only include ages between the minimum and maximum provided
                                     between(age, input$subset_age[1], input$subset_age[2])) %>%
                              # Also create the column called grouped_marital
                              mutate(grouped_marital = ifelse(marital_status %in% c("Divorced", "Never married", "Widowed"), "Not married", "Married")))
  

  # 2. Define how the figure is created
  # Use create_subset() to run the reactive code above and generate a
    # subset that the figure will be built from
  # Use aes_string() rather than aes() to allow the use of input$set_color to
    # be interpreted as the name of a column
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  # Plot age and capital gain as a scatterplot
                                  geom_point(aes_string(x = "age", y = "capital_gain",
                                                        col = input$set_color)) +
                                  # Draw a line representing a simple linear model of the data
                                  geom_smooth(aes_string(x = "age", y = "capital_gain",
                                                         col = input$set_color), method = "lm") +
                                  # Set the x limits so you can see how the app is working
                                  xlim(15, 95) + 
                                  # Increase the relative font size to 18 for easy viewing
                                  theme_bw(18) + 
                                  ylab("Capital Gains") +
                                  xlab("Age"))
}

# Run app
shinyApp(ui,server)
