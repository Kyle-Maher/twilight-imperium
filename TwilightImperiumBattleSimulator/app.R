


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

################################################################################

ui <- navbarPage("Twilight Imperium Resources",
  tabPanel("Battle Simulator",
    fluidPage(
      fluidRow(
        column(8,
          actionButton("simulate", "Simulate"),
          hr()
        )
      ),
      fluidRow(
        column(4,
          h4("Attacker's Units"),
          div(
            actionButton("add_attacker", "Add Attacking Unit", class = "btn-primary"),
            actionButton("clear_attaking_units", "Clear")
          ),
          uiOutput("atacking_unit_selection")
        ),
        column(4,
          h4("Defender's Units"),
          div(
            actionButton("add_defender", "Add Defending Unit", class = "btn-danger"),
            actionButton("clear_defending_units", "Clear")
          ),
          uiOutput("defending_unit_selection")
        )
      )
    )
  ),
  tabPanel("Wiki Data")
)

server <- function(input, output, session) {

  observeEvent(input$add_attacker, {
    insertUI(
      selector = "#add_attacker",
      where = "beforeBegin",
      ui = fluidRow(
        column(7,
          selectInput(
            inputId = paste0("attacker_unit"),
            label = "Type",
            choices = df$Unit_Name,
            selected = df$Unit_Name[1]
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("attacker_counter"),
            label = "Count",
            value = 1,
            min = 1
          )
        )
      )
    )
  })

  observeEvent(input$add_defender, {
    insertUI(
      selector = "#add_defender",
      where = "beforeBegin",
      ui = fluidRow(
        column(7,
          selectInput(
            inputId = paste0("defender_unit"),
            label = "Type",
            choices = df$Unit_Name,
            selected = df$Unit_Name[1]
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("defender_counter"),
            label = "Count",
            value = 1,
            min = 1
          )
        )
      )
    )
  })

  output$current_selection <- renderPrint({
    list(
      Attacker = list(
        Unit = input$attacker_unit,
        Count = input$attacker_count
      ),
      Defender = list(
        Unit = input$defender_unit,
        Count = input$defender_count
      )
    )
  })
}

shinyApp(ui, server)
