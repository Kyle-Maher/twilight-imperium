
##################
# To Do:
# Adjust selection so that previous selections are not available
# Adjust Reset to go to one Options
# Add selection option to show faction specific units
# Add num rounds selection
# Add filters to the wiki data
##################


library(shiny)
library(reticulate)  # Runs Python Code
library(this.path)  # Robust Relative Pathing
library(readr)
library(DT)

# Temp
# setwd("TwilightImperiumBattleSimulator")
# getwd()


# Python Environment Setup
use_virtualenv(file.path(dirname(this.path()), "..", ".venv"), required = TRUE)
source_python("../src/simulate.py")

df <- read_csv("../data/clean/all_units_df.csv")

base_unit_choices <- df$Unit_Name[df$Faction_Name == "Common Unit"]
faction_specific_unit_choices <- df$Unit_Name
default_unit <- df$Unit_Name[df$Faction_Name == "Common Unit"][1]

# df$Unit_Name
# df$Faction_Name
# df$Unit_Type

################################################################################

ui <- navbarPage("Twilight Imperium Resources",
  tabPanel("Battle Simulator",
    fluidPage(
      fluidRow(
        column(7,
          actionButton("simulate", "Simulate"),
          actionButton("clear", "Reset"),
          checkboxInput("show_faction_specific", "Show Faction Specific Units", FALSE),
          hr()
        )
      ),
      fluidRow(
        column(4,
          h4("Attacker's Units"),
          div(
            actionButton("add_attacker", "Add Attacking Unit", class = "btn-primary"),
            # actionButton("clear_attaking_units", "Clear")
          ),
          uiOutput("atacking_unit_selection")
        ),
        column(4,
          h4("Defender's Units"),
          div(
            actionButton("add_defender", "Add Defending Unit", class = "btn-danger"),
            # actionButton("clear_defending_units", "Clear")
          ),
          uiOutput("defending_unit_selection")
        ),
        column(4,
          verbatimTextOutput("current_selection")
        )
      )
    )
  ),
  tabPanel("Wiki Data",
    fluidPage(
      fluidRow(
        column(12,
          DTOutput("all_units")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  unit_choices <- base_unit_choices  # Set default

  attacking_unit_count <- reactiveVal(0)
  defending_unit_count <- reactiveVal(0)

  observeEvent(input$add_attacker, {
    attacking_unit_count(attacking_unit_count() + 1)

    insertUI(
      selector = "#add_attacker",
      where = "beforeBegin",
      ui = fluidRow(
        column(7,
          selectInput(
            inputId = paste0("attacker_unit_", attacking_unit_count()),
            label = "Type",
            choices = unit_choices,
            selected = default_unit
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("attacker_counter_", attacking_unit_count()),
            label = "Count",
            value = 1,
            min = 1
          )
        )
      )
    )
  })

  observeEvent(input$add_defender, {
    defending_unit_count(defending_unit_count() + 1)

    insertUI(
      selector = "#add_defender",
      where = "beforeBegin",
      ui = fluidRow(
        column(7,
          selectInput(
            inputId = paste0("defender_unit_", defending_unit_count()),
            label = "Type",
            choices = unit_choices,
            selected = default_unit
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("defender_counter_", defending_unit_count()),
            label = "Count",
            value = 1,
            min = 1
          )
        )
      )
    )
  })

  observeEvent(input$clear, {
    removeUI(selector = "div.form-group.shiny-input-container", multiple = TRUE)
    attacking_unit_count(0)
    defending_unit_count(0)
    updateCheckboxInput(session, "show_faction_specific", value = FALSE)
  })


  observeEvent(input$simulate, {
    output$current_selection <- renderPrint({

      # Collect attacker units
      attackers <- list()
      for(i in seq_len(attacking_unit_count())){
        unit <- input[[paste0("attacker_unit_", i)]]
        count <- input[[paste0("attacker_counter_", i)]]
        if(!is.null(unit) && !is.null(count)){
          attackers[[unit]] <- count
        }
      }

      # Collect defender units
      defenders <- list()
      for(i in seq_len(defending_unit_count())){
        unit <- input[[paste0("defender_unit_", i)]]
        count <- input[[paste0("defender_counter_", i)]]
        if(!is.null(unit) && !is.null(count)){
          defenders[[unit]] <- count
        }
      }

      attacker_units_dict <- r_to_py(lapply(attackers, as.integer))
      defender_units_dict <- r_to_py(lapply(defenders, as.integer))

      # simulate_battles() is from simulate.py
      sim <- simulate_battles(attacker_units_dict, defender_units_dict)

      sim

    })
  })

  observeEvent(input$show_faction_specific, {
    unit_choices <- if(input$show_faction_specific) faction_specific_unit_choices else base_unit_choices

    # Update Attacker Selection Inputs
    auc <- attacking_unit_count()
    for (i in seq_len(auc)){
      selected <- input[[paste0("attacker_unit_", i)]]
      selection <- if (selected %in% unit_choices) selected else default_unit
      updateSelectInput(
        session,
        inputId = paste0("attacker_unit_", i),
        choices = unit_choices,
        selected = selection
      )
    }

    # Update Defender Selection Inputs
    duc <- defending_unit_count()
    for (i in seq_len(duc)){
      selected <- input[[paste0("defender_unit_", i)]]
      selection <- if (selected %in% unit_choices) selected else default_unit
      updateSelectInput(
        session,
        inputId = paste0("defender_unit_", i),
        choices = unit_choices,
        selected = selection
      )
    }
  })

  output$all_units <- renderDT(df)

}

shinyApp(ui, server)
