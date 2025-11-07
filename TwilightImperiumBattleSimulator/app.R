


library(shiny)
library(reticulate)  # Runs Python Code
library(this.path)  # Robust Relative Pathing
library(readr)

# Temp
# setwd("TwilightImperiumBattleSimulator")
# getwd()


# Python Environment Setup
use_virtualenv(file.path(dirname(this.path()), "..", ".venv"), required = TRUE)
source_python("../python_test_file.py")

df <- read_csv("../data/clean/all_units_df.csv")

# df$Unit_Name
# df$Faction_Name
# df$Unit_Type


library(shiny)


ui <- fluidPage(
  titlePanel("Twilight Imperium Battle Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Attacker Army"),
      actionButton("add_attacker", "Add Attacker Unit"),
      uiOutput("attacker_rows"),
      br(),
      
      h4("Defender Army"),
      actionButton("add_defender", "Add Defender Unit"),
      uiOutput("defender_rows")
    ),
    
    mainPanel(
      h4("Selected Armies"),
      verbatimTextOutput("armies")
    )
  )
)

server <- function(input, output, session) {
  
  # Track number of rows for attacker and defender
  attacker_counter <- reactiveVal(0)
  defender_counter <- reactiveVal(0)
  
  observeEvent(input$add_attacker, {
    attacker_counter(attacker_counter() + 1)
  })
  observeEvent(input$add_defender, {
    defender_counter(defender_counter() + 1)
  })
  
  # Dynamic UI for attacker
  output$attacker_rows <- renderUI({
    n <- attacker_counter()
    if(n == 0) return(NULL)
    
    lapply(1:n, function(i) {
      fluidRow(
        column(7,
               selectInput(
                 inputId = paste0("attacker_unit_", i),
                 label = NULL,
                 choices = df$Unit_Name,
                 selected = df$Unit_Name[1]
               )
        ),
        column(5,
               numericInput(
                 inputId = paste0("attacker_count_", i),
                 label = "Count",
                 value = 1,
                 min = 1
               )
        )
      )
    })
  })
  
  # Dynamic UI for defender
  output$defender_rows <- renderUI({
    n <- defender_counter()
    if(n == 0) return(NULL)
    
    lapply(1:n, function(i) {
      fluidRow(
        column(7,
               selectInput(
                 inputId = paste0("defender_unit_", i),
                 label = NULL,
                 choices = df$Unit_Name,
                 selected = df$Unit_Name[1]
               )
        ),
        column(5,
               numericInput(
                 inputId = paste0("defender_count_", i),
                 label = "Count",
                 value = 1,
                 min = 1
               )
        )
      )
    })
  })
  
  # Combine attacker and defender into a named list
  output$armies <- renderPrint({
    # Attacker
    n_att <- attacker_counter()
    attacker_army <- if(n_att > 0) {
      setNames(
        sapply(1:n_att, function(i) input[[paste0("attacker_count_", i)]]),
        sapply(1:n_att, function(i) input[[paste0("attacker_unit_", i)]] )
      )
    } else list()
    
    # Defender
    n_def <- defender_counter()
    defender_army <- if(n_def > 0) {
      setNames(
        sapply(1:n_def, function(i) input[[paste0("defender_count_", i)]]),
        sapply(1:n_def, function(i) input[[paste0("defender_unit_", i)]] )
      )
    } else list()
    
    # Combine
    list(
      Attacker = attacker_army,
      Defender = defender_army
    )
  })
  
  
}

shinyApp(ui, server)
