# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(shinythemes)
library(DT)

# Reading the csvs
committee_edge_list <- read_csv(here("Congressional Committee Shiny/committee_edge_list.csv"))
congressperson_affiliation <- read_csv(here("Congressional Committee Shiny/congressperson_affiliation.csv"))

# User interface
ui <- fluidPage(
  titlePanel("Congressional Committees"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chamber", 
                         label = "Select chamber(s) of interest", 
                         choices = list("House" = 1, 
                                        "Senate" = 2),
                         selected = 1),
      radioButtons(inputId = "color",
                   label = "Select variable of interest for Congressfolk",
                   choices = c("Gender", "Leadership",
                               "Political Party"),
                   selected = "Gender"),
      sliderInput(inputId = "threshold", 
                  # I feel like this wording could make more sense? But I am too tired to find it
                  label = "Minimum number of committees shared between Congressfolk to have a tie",
                  min = 1, 
                  max = 5,
                  value = 1),
      submitButton("Submit")
    ),
    mainPanel(
      # Set up my tabs and what output goes where HERE!
    )
  )
)

# Server function
server <- function(input, output){
  # Filter the dataset to only have folks from the chambers of interest
  
  # Then, make the one-mode network!
  
  # Have three if statements for each radio button input, where I create the vector of color I need from 
  # congressperson_affiliation csv file
    # leadership_colors <- congressperson_affiliation[['title_color']]
  
  # Then, plot the network :)
  
  # If time—try and figure out an interactive network. Convert statnet net to the other thing
  
  # Finally, make an output table that corresponds to a hypothesis test I run on centralization 
  # calculations (are differences between the groups statistically significant? Given that each group
  # is a binary variable, I could probably do the same test for them all... cat x, cont y—difference in
  # means!! Woot. Could also make a lil boxplot to emphasize some of the more natural trends, show his-
  # togram.)
}

# Creates app
shinyApp(ui = ui, server = server)