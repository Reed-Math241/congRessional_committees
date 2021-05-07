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
library(networkD3)

# ggplot theme function
robins_ggplot_theme <- function(font = "Times") {
  theme_light() +
    theme(plot.title = element_text(hjust = 0.5, family = font, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic", family = font),
          axis.title = element_text(family = font),
          axis.text = element_text(family = font),
          legend.title = element_text(family = font),
          legend.text = element_text(family = font),
          plot.caption = element_text(family = font, face = "italic"))
}

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
      plotOutput(outputId = "network"),
      dataTableOutput(outputId = "t_test_table"),
      plotOutput(outputId = "boxplot"),
      forceNetworkOutput(outputId = "interactive")
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
    
    if(input$color == "Gender") {
      hyp_test_degree <- tidy(t.test(congress_deg ~ gender_bin, data = congressperson_affiliation_new))
      hyp_test_eig <- tidy(t.test(congress_eig ~ gender_bin, data = congressperson_affiliation_new))
    }
    else if(input$color == "Leadership") {
      hyp_test_degree <- tidy(t.test(congress_deg ~ title_bin, data = congressperson_affiliation_new))
      hyp_test_eig <- tidy(t.test(congress_eig ~ title_bin, data = congressperson_affiliation_new))
    } else {
      hyp_test_degree <- tidy(t.test(congress_deg ~ party_bin, data = congressperson_affiliation_new))
      hyp_test_eig <- tidy(t.test(congress_eig ~ party_bin, data = congressperson_affiliation_new))
    }
    
    hyp_test <- rbind(hyp_test_degree, hyp_test_eig)
    hyp_test <- hyp_test %>%
      rowid_to_column("id") %>%
      mutate(Centrality_Measure = case_when(id == 1 ~ "Degree Centrality",
                                            id == 2 ~ "Eigenvector Centrality")) %>%
      select(-id) %>%
      select(Centrality_Measure, everything())
    datatable(hyp_test,
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
  
  output$boxplot <- renderPlot({
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
    
    # Make a column of centrality, facet by type
    congressperson_affiliation_box <- pivot_longer(congressperson_affiliation_new,
                                                   cols = c("congress_deg", "congress_eig"),
                                                   names_to = "Name_of_Centrality_Measure",
                                                   values_to = "Centrality_Measure")
    if(input$color == "Gender") {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(gender_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme()
    }
    else if(input$color == "Leadership") {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(title_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme()
    } else {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(party_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme()
    }
  })
  
  output$interactive <- renderForceNetwork({
    # Making my matrix into an edge list
    m <- matrix_prod
    idx = m > 0.001
    edges = data.frame(
      source = row(m)[idx],
      target = col(m)[idx],
      corr = m[idx])
    
    # Transform it to match the d3 network requirements
    nodes_d3 <- mutate(congressperson_affiliation_reactive(), id = id - 1)
    edges_d3 <- mutate(edges, source = source - 1, target = target - 1)
    
    ColourScale <- 'd3.scaleOrdinal()
            .domain(["blue", "pink2"])
           .range(["blue", "pink"]);'
    
    forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "source", Target = "target", 
                 NodeID = "name", Group = "gender_color", Value = "corr", 
                 opacity = 1, fontSize = 16, zoom = TRUE, colourScale = JS(ColourScale))
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)