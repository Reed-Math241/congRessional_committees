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
          plot.caption = element_text(family = font, face = "italic")) +
    theme(strip.background = element_rect(color="white", 
                                          fill="gray85", 
                                          size=1.5, 
                                          linetype="solid"))
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
  titlePanel("116th Congress Congressional Committee Ties"),
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
                  label = "Minimum number of committees shared between Congressfolk to have a tie
                  (*Note that this will not impact calculations of statistical significance or
                  the interactive network.)",
                  min = 1, 
                  max = 5,
                  value = 1),
      submitButton("Submit")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Static Network",
                           plotOutput(outputId = "network"),
                           tags$em("The above static network graphs are colored by the variable of interest selected
                                  on the left For gender, the network is colored blue for men and pink for women.
                                  For leadership, those who have a title (Committee Chair or Ranking Member) are colored
                                  purple, while those without a title are colored gray. And lastly, Democrats are colored
                                  blue while Republicans are colored red."),
                           tags$br(),
                           tags$br(),
                           tags$p("Navigate through the table to look at the affiliation information of all
                                  Congressfolk selected on the left. Feel free to sort by any category of interest."),
                           tags$br(),
                           dataTableOutput(outputId = "network_info"),
                           tags$br(),
                           tags$em("Note that the data used in this project was collected by govtrack by pulling
                                   information from Congressfolks' website. This opens the possibility for NA values, wherein
                                   the data collectors at govtrack were not sure of a Congressperson's gender, political party,
                                   leadership title (or lack thereof), or even what chamber they are part of. The
                                   author chooses to leave in such NA values, as it is not fair to drop an entire
                                   observation of data because one piece of data is missing. This could cause assumption
                                   issues in the statistical significance tab.")),
                  tabPanel("Explanation of Network Centrality",
                           h4("What is Degree and Eigenvector Centrality?"),
                           tags$p("Network analysis is accompanied by multiple different measures to determine power or
                                  prestige for the nodes (each individual Congressperson, in this case). This app looks at
                                  network centrality, or a calculation of how CENTRAL in the network each individual node
                                  is. The first centrality measure, degree centality, measures the number of ties to other
                                  Congressfolk each individual person has. This is helpful in determining who
                                  has the most connections—connections which provide the congressperson with
                                  larger amounts of social capital than their fellow congressfolk, helping them build coalitions
                                  to pass favorable legislation for their constituency. Degree centrality is useful in
                                  terms of how simple it is to calculate, but unfortunately has constraints in that it reflects
                                  ties in LOCAL networks instead of taking into account how meaningful ties with specific
                                  more central central congressfolk are."),
                           tags$br(),
                           tags$p("Below are two networks, each node labeled with their degree centrality score."),
                           div(img(src = "deg_ex1.png", height = 350, width = 350), style="text-align: center;"),
                           tags$br(),
                           div(img(src = "deg_ex2.png", height = 350, width = 350), style="text-align: center;"),
                           tags$br(),
                           tags$br(),
                           tags$p("My second centrality measure looks at eigenvector centrality. This measure is much more complicated
                                  than degree centrality, but can account for the strength of ties on a global level. Essentially, 
                                  folks created the 'radius of power' as a measurement for centrality. Eigenvector centrality borrows the
                                  logic of positive radius of power (the value of 0.23 shown in the graph), where a higher centrality 
                                  measure comes from being tied to other people
                                  who are more central in the network. Through eigenvectors, we can assess the important of relative differences
                                  between ties at a level of higher detail than degree centrality can provide."),
                           tags$br(),
                           tags$p("Below is a visualization and network showing how eigenvector centrality is calculated in opposition to the radii
                                  of power."),
                           div(img(src = "eig_ex1.png", height = 350, width = 550), style="text-align: center;"),
                           tags$br(),
                           div(img(src = "eig_ex2.png", height = 75, width = 425), style="text-align: center;"),
                           tags$br(),
                           tags$br(),
                           tags$p("To see this information in context, visit the 'Statistical Significane tab'.")),
                  tabPanel("Statistical Significance",
                           h4("Is the selected variable significant on measures of network centrality?"),
                           tags$p("While the 'Explanation of Network Centrality' tab explains the two types of centrality
                                  used in this analysis of the 116th congressional committee network, this tab uses statistical
                                  methods to determine if the difference in centrality score by either gender, leadership position,
                                  or political party is statistically significant."),
                           tags$br(),
                           tags$p("To begin, we look at a boxplot of the two centrality measures plotted by the variable of interest.
                                  Do you see a difference in the median scores of each group? Are there any outliers that stand
                                  out to you? Also, take note of the NA option—as explained in 'Static Network,' the data used
                                  comes from folks who used Congresspeople's websites to determine information. On occassion
                                  they were not sure, and thus coded that person with an NA value."),
                           tags$br(),
                           plotOutput(outputId = "boxplot"),
                           tags$br(),
                           tags$p("Now, to test if the two categories have a statistically significant difference from each another,
                                  the app runs a t test. This is just a fancy way to say the computer will find the PROBABILITY
                                  that the centrality results caulcuated vary by the selected variable by chance. It finds the average 
                                  difference between both measures
                                  of centrality for the two categories (either gender, leadership, or party), then decides if that average is
                                  high enough to mean there is a SIGNIFICANT, real relationship between the variable and the 
                                  centrality measures.
                                  The probability that the difference found proves that there is truly a relationship between the variables—
                                  or that the more 'interesting,' alternative possibility is true—is represented through the p-value. The lower
                                  it is, the more likely it is that this is the case."),
                           tags$br(),
                           tags$p("How small a p-value must be before it is significant differs by discipline. For our purposes, let us say
                                  that any p-value under 0.15 is significant enough to reject the notion that there is no relationship and 
                                  conclude that gender, leadership, or political party impacts
                                  a congressperson's centrality in the network. How many configurations have significant results? Are they
                                  what you would expect to see? (see the bottom of this page for some hints!)"),
                           tags$br(),
                           dataTableOutput(outputId = "t_test_table"),
                           tags$br(),
                           tags$p("Other values included in the table is the test statistic, or t. This is the statistic calculated from the t-test, 
                                  which is used to calculate
                                  the p-value using a standardized t distribution. Think of this as the cutoff point, where everything above it is added
                                  together to find the probability! For instance, later in the evening you're probably more hungry,
                                  and thus are MORE LIKELY to eat dinner (or a midnight snack!). The t is like this as well—as the t increases in magnitude,
                                  the relationship between centrality and the selected variable of interest is MORE LIKELY to be significant!. 
                                  The confidence interval is the range of values that we are 95% confident contain the true
                                  difference in centrality measures between the two categories. We choose a 95% confience value here, as we
                                  can never be 100% confident (otherwise the interval range would have to include all possible values!). 95% provides
                                  a good balance between both accuracy and precision for the interval."),
                           tags$br(),
                           tags$em("Hints: Running the t tests and looking at the different combinations of variables of interest and different configuations
                                  of chambers for the congresspeople, we observe that leadership and party are significant for members of the House,
                                  while leadership and party is significant for everyone. Gender is significant only in the Senate, with women having
                                   less centrality than men. In all instances of leadership and party, those with leadership positions had significantly more
                                   centrality than those without leadership titles, while the Democrats had more centrality. Can you replicate these results?
                                   How does this impact your thoughts about who has the most power in systems of governance? Do you think this imbalance of power
                                   is fair in a democratic system?")),
                  tabPanel("Interactive Network",
                           h4("An interactive network, modeled using "),
                           tags$code("networkD3"),
                           tags$br(),
                           tags$p("With the below network, feel free to select any of the nodes to see the name of the congressperson!
                                  Additionally, you can move around any of the nodes and watch the network reform around it."),
                           tags$br(),
                           tags$em("A word of caution: with large amounts of nodes, the graph may move slowly as it tries to plot
                                   all of the nodes. It works best with less congressfolk and patience."),
                           tags$br(),
                           forceNetworkOutput(outputId = "interactive")),
                  tabPanel("Sources",
                           h4("Many thanks to those who have paved the way!"),
                           tags$br(),
                           tags$p("For this project, I am greatly indebted to the help of multiple resources. First of all,
                                  I want to thank the folks over at govtrack (an extension of dataworld) for compiling all of this
                                  information about committee relations in the 116th Congress! Visit their site", 
                                  tags$a(href="https://data.world/govtrack/congressional-committees", "here"), "to learn more and 
                                  even use their own data yourself!"),
                           tags$br(),
                           tags$p("Additionally, I would like to thank the multitude of resources about how to code networks in R,
                                  as without their help I certainly would not have been able to pull this off! Jesse Sadler's", 
                                  tags$a(href="https://www.jessesadler.com/post/network-analysis-with-r/", "blog post"), "was
                                  a huge help in figuring out how to create nodes and edges from a dataframe, and helped me create
                                  the interactive network using", tags$code("NetworkD3"), "For resources about how to use",
                                  tags$code("statnet"), "and", tags$code("ade4"), "to turn a two-mode network into a one-mode,
                                  calculate measures of network centrality, and graph by color", tags$a(href="http://www.kateto.net/wp-content/uploads/2019/06/Sunbelt%202019%20R%20Network%20Visualization%20Workshop.pdf",
                                  "this site"), "written by Katherine Ognyanova and", tags$a(href="https://rpubs.com/pjmurphy/542335", "this site"), "
                                  written by Phil Murphy and",
                                  tags$a(href="https://www.uni-due.de/hummell/man/sna/.sna.gplot.pdf", "this site"), "created by Melissa Clarkson 
                                  were invaluable. I encourage you to check out their work!"),
                           tags$br(),
                           tags$p("And of course, I would be remiss if I did not acknowledge the wonderful work Kelly McConville, my lovely data science
                                  professor, has done to help cultivate my skills in visualization, wrangling, and data analysis this semester!
                                  Many thanks to my fellow classmates as well for all of your help in the Slack and being a positive, welcoming
                                  virtual community this semester!")))
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
  
  # Three if statements for each radio button input, where I create the vector of color I need from 
  # congressperson_affiliation csv file
  vector_color_reactive <- reactive({
    if(input$color == "Gender") {
      congressperson_affiliation_reactive()[['gender_color']]
    }
    else if(input$color == "Leadership") {
      congressperson_affiliation_reactive()[['title_color']]
    } else {
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
  
  congress_cent_df_reactive <- reactive({
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
    congress_cent_df
  })
  
  # Then, plot the network :)
  
  output$network <- renderPlot({
    gplot(matrix_prod, 
          vertex.col = vector_color_reactive(), 
          thresh = input$threshold, 
          jitter = TRUE, 
          usearrows = FALSE, 
          vertex.cex = 2)
  })
  
  output$network_info <- renderDataTable({
    congressperson_affiliation_labels <- congressperson_affiliation_reactive() %>%
      select(gender_bin, type, party_bin, name, title_bin) %>%
      mutate(gender_bin = case_when(gender_bin == 1 ~ "Woman",
                                    gender_bin == 0 ~ "Man"),
             party_bin = case_when(party_bin == 1 ~ "Democrat",
                                   party_bin == 0 ~ "Republican"),
             title_bin = case_when((party_bin == "Democrat" & title_bin == 1) ~ "Ranking Member",
                                   (party_bin == "Republican" & title_bin == 1) ~ "Committee Chair",
                                   (title_bin == 0) ~ "No Leadership Position")) %>%
      rename(Gender = gender_bin,
             Chamber = type,
             Political_Party = party_bin,
             Name = name,
             Leadership_Position = title_bin) %>%
      select(Name, everything()) %>%
      arrange(Name)
    datatable(congressperson_affiliation_labels,
              options = list(paging = TRUE,
                             searching = TRUE,
                             orderClasses = TRUE))
  })
  
  # Finally, make an output table that corresponds to a hypothesis test I run on centralization 
  # calculations (are differences between the groups statistically significant?)
  
  output$t_test_table <-  renderDataTable({
    congressperson_affiliation_new <- full_join(congress_cent_df_reactive(), 
                                                congressperson_affiliation_reactive(), 
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
      select(-id, -estimate, -estimate1, -estimate2, -parameter, -alternative) %>%
      rename(t = statistic) %>%
      select(Centrality_Measure, everything())
    datatable(hyp_test,
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
  
  output$boxplot <- renderPlot({
    congressperson_affiliation_new <- full_join(congress_cent_df_reactive(), 
                                                congressperson_affiliation_reactive(), 
                                                by = c("IDs" = "name"))
    
    # Make a column of centrality, facet by type
    congressperson_affiliation_box <- pivot_longer(congressperson_affiliation_new,
                                                   cols = c("congress_deg", "congress_eig"),
                                                   names_to = "Name_of_Centrality_Measure",
                                                   values_to = "Centrality_Measure")
    
    congressperson_affiliation_box <- congressperson_affiliation_box %>%
      mutate(Name_of_Centrality_Measure = case_when(Name_of_Centrality_Measure == "congress_deg" ~
                                                      "Degree_Centrality",
                                                    Name_of_Centrality_Measure == "congress_eig" ~
                                                      "Eigenvector_Centrality"))
  
    if(input$color == "Gender") {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(gender_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme() +
        labs(x = "Gender ~ 0 is Man, 1 is Woman",
             y = "Centrality Measure",
             title = "Does Gender Influence a Congressperson's Network Centrality?")
    }
    else if(input$color == "Leadership") {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(title_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme() +
        labs(x = "Leadership ~ 0 is No Leadership Position, 1 is Chair or Ranking Member",
             y = "Centrality Measure",
             title = "Does Leadership Influence a Congressperson's Network Centrality?")
    } else {
      ggplot(data = congressperson_affiliation_box, mapping = aes(x = factor(party_bin),
                                                                  y = Centrality_Measure)) +
        geom_boxplot() +
        facet_wrap(~Name_of_Centrality_Measure, scales = "free") +
        robins_ggplot_theme() +
        labs(x = "Party ~ 0 is Republican, 1 is Democrat",
             y = "Centrality Measure",
             title = "Does Political Party Influence a Congressperson's Network Centrality?")
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
    
    if(input$color == "Gender") {
      ColourScale <- 'd3.scaleOrdinal()
            .domain(["blue", "pink2"])
           .range(["blue", "pink"]);'
      
      forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "source", Target = "target", 
                   NodeID = "name", Group = "gender_color", Value = "corr", 
                   opacity = 1, fontSize = 16, zoom = TRUE, colourScale = JS(ColourScale))
    }
    else if(input$color == "Leadership") {
      ColourScale <- 'd3.scaleOrdinal()
            .domain(["gray", "purple"])
           .range(["gray", "purple"]);'
      
      forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "source", Target = "target", 
                   NodeID = "name", Group = "title_color", Value = "corr", 
                   opacity = 1, fontSize = 16, zoom = TRUE, colourScale = JS(ColourScale))
    } else {
      ColourScale <- 'd3.scaleOrdinal()
            .domain(["red", "blue"])
           .range(["red", "blue"]);'
      
      forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "source", Target = "target", 
                   NodeID = "name", Group = "party_color", Value = "corr", 
                   opacity = 1, fontSize = 16, zoom = TRUE, colourScale = JS(ColourScale))
    }
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)