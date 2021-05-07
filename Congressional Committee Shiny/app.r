# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)
library(shinythemes)
library(DT)
library(readr)
library(statnet)
library(ade4)
library(broom)

# Reading the csvs
committee_edge_list <- read_csv(here("Congressional Committee Shiny/committee_edge_list.csv")
                                #skip = 1,
                                #header = FALSE,
                                #directed = FALSE
                                )
congressperson_affiliation <- read_csv(here("Congressional Committee Shiny/congressperson_affiliation.csv"))

# User interface
ui <- fluidPage(
  titlePanel("Congressional Committees"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chamber", 
                         label = "Select chamber(s) of interest", 
                         choices = c("House", "Senate"),
                         selected = "House"),
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
      dataTableOutput(outputId = "t_test_table"),
      plotOutput(outputId = "network")
    )
  )
)

# Server function
server <- function(input, output){
  # Filter the dataset to only have folks from the chambers of interest
  # First filter the affiliation data, then do a left join (the one that makes the other lost what
  # it doesn't have in common)
  congressperson_affiliation_reactive <- reactive({
    congressperson_affiliation %>%
      filter(type %in% input$chamber)
  })
  
  committee_edge_list_reactive <- reactive({
    names_to_keep <- congressperson_affiliation_reactive()[['name']]
    
    committee_edge_list %>%
      filter(name %in% names_to_keep)
  })
  
  # Have three if statements for each radio button input, where I create the vector of color I need from 
  # congressperson_affiliation csv file
    # leadership_colors <- congressperson_affiliation[['title_color']]
  vector_color_reactive <- reactive({
    if(input$color == "Gender") {
      congressperson_affiliation_reactive()[['gender_color']]
    }
    else if(input$color == "Leadership") {
      congressperson_affiliation_reactive()[['title_color']]
    } else {
    # if(input$color == "Political Party") {
      congressperson_affiliation_reactive()[['party_color']]
    }
  })
  
  # Then, make the one-mode network!
  matrix_prod_reactive <- reactive({
    mode1 <- unique(committee_edge_list_reactive()[,1])
    mode2 <- unique(committee_edge_list_reactive()[,2])
    net <- as.network(committee_edge_list_reactive(),
                      bipartite = length(mode2),   # Number of nodes in the second mode (events)
                      directed = FALSE)
    bipartite_matrix <- as.matrix(net)
    matrix_prod <- bipartite_matrix %*% t(bipartite_matrix)
    diag(matrix_prod) <- 0
    matrix_prod
  })
  
  # Then, plot the network :)
  
  output$network <- renderPlot({
    # mode1 <- unique(committee_edge_list_reactive()[,1])
    # mode2 <- unique(committee_edge_list_reactive()[,2])
    # net <- as.network(committee_edge_list_reactive(), 
    #                   bipartite = length(mode2),   # Number of nodes in the second mode (committees)
    #                   directed = FALSE)
    # bipartite_matrix <- as.matrix(net)
    # matrix_prod <- bipartite_matrix %*% t(bipartite_matrix)
    # diag(matrix_prod) <- 0
    gplot(matrix_prod, 
          vertex.col = vector_color_reactive(), 
          thresh = input$threshold, 
          jitter = TRUE, 
          usearrows = FALSE, 
          vertex.cex = 2)
  })
  
  # Finally, make an output table that corresponds to a hypothesis test I run on centralization 
  # calculations (are differences between the groups statistically significant? Given that each group
  # is a binary variable, I could probably do the same test for them all... cat x, cont y—difference in
  # means!! Woot. Could also make a lil boxplot to emphasize some of the more natural trends, show his-
  # togram.)
  
  output$t_test_table <-  renderDataTable({
    # mode1 <- unique(committee_edge_list_reactive()[,1])
    # mode2 <- unique(committee_edge_list_reactive()[,2])
    # net <- as.network(committee_edge_list_reactive(),
    #                   bipartite = length(mode2),   # Number of nodes in the second mode (committees)
    #                   directed = FALSE)
    # bipartite_matrix <- as.matrix(net)
    
    # Can add matrix_prod into t() to get us back to where we started
    congressperson_jaccard <- dist.binary(t(bipartite_matrix), 
                                          method = 1, upper = TRUE, 
                                          diag = FALSE)
    congressperson_jaccard <- as.matrix(congressperson_jaccard)   
    diag(congressperson_jaccard) <- 0
    congressperson_jaccard <- ifelse(congressperson_jaccard > 0.95, 1, 0)
    jacc_congressperson <- as.network(congressperson_jaccard,    # Create a statnet network
                                      directed = FALSE)
    
    IDs <- jacc_congressperson%v%"vertex.names"
    congress_deg  <- degree(jacc_congressperson)
    #congress_bet  <- betweenness(jacc_congressperson)
    #congress_clos <- closeness(jacc_congressperson,
    #             cmode="suminvdir")
    congress_eig  <- evcent(jacc_congressperson)
    
    congress_cent_df <- data.frame(IDs, 
                                   congress_deg, 
                                   #congress_bet, 
                                   #congress_clos, 
                                   congress_eig)
    
    congressperson_affiliation_new <- full_join(congress_cent_df, congressperson_affiliation_reactive(), 
                                                 by = c("IDs" = "name"))
    
    # Once we get things to work, write t.tests into if statements by the input selected
    # Currently it errors because there are NO factors to do a t.test with
    gender_deg <- tidy(t.test(congress_deg ~ title_bin, data = congressperson_affiliation_new))
    
    datatable(gender_deg,
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)