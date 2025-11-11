################################################################################
# To Do:
# Adjust unit selection so that previous selections are not available
# Add filters to the wiki data
# Fix anti fighter not caring if it removes non-fighters (whoops)
################################################################################


library(shiny)
library(reticulate)
library(readr)
library(DT)
library(dplyr)

# If Running Locally:
# setwd("TwilightImperiumBattleSimulator")
# use_virtualenv("../.venv", required = TRUE)

# If Publishing:
virtualenv_create("r-reticulate")
virtualenv_install("r-reticulate", packages = c("pandas", "numpy"))
use_virtualenv("r-reticulate", required = TRUE)


source_python("simulate.py")
df <- read_csv("all_units_df.csv")

common_mech <- df %>%
  filter(Unit_Name == "Aerie Sentinel") %>%
  mutate(
    Faction_Name = "Common Unit",
    Unit_Name = "Mech"
  ) %>%
  select(-Unit_Abilities)

df <- bind_rows(df, common_mech)

base_unit_choices <- df$Unit_Name[df$Faction_Name == "Common Unit"]
all_unit_choices <- df$Unit_Name
default_unit <- df$Unit_Name[df$Faction_Name == "Common Unit"][1]

################################################################################

ui <- navbarPage("Twilight Imperium",
  tabPanel("Battle Simulator",
    fluidPage(
      fluidRow(
        column(4,
          actionButton("simulate", "Simulate"),
          actionButton("clear", "Reset"),
          div(
            h3(" ")
          ),
          sliderInput("rounds", "Battles to Simulate", 100, 10000, 500, step = 100, ticks = FALSE),
          checkboxInput("show_faction_specific", "Show Faction Specific Units", FALSE),
          hr(),
          h4("Attacker's Units"),
          div(
            actionButton("add_attacker", "Add Attacking Unit", class = "btn-primary"),
            actionButton("remove_attacker", "Back")
          ),
          uiOutput("atacking_unit_selection"),
          br(),
          h4("Defender's Units"),
          div(
            actionButton("add_defender", "Add Defending Unit", class = "btn-danger"),
            actionButton("remove_defender", "Back")
          ),
          uiOutput("defending_unit_selection")
        ),
        column(8,
          column(6,
            DTOutput("results")
          ),
          column(6,
            DTOutput("metadata")
          ),
          br(),
          DTOutput("attacker_stats"),
          br(),
          DTOutput("defender_stats")
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
  ),
  tabPanel("info",
    fluidPage(
      fluidRow(
        column(12,
          h3("Data:"),
          h4("https://twilight-imperium.fandom.com/wiki/Twilight_Imperium_Wiki"),
          h3("Created by:"),
          h4("Kyle Maher")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # Functions

  add_attacker <- function() {
    attacking_unit_count(attacking_unit_count() + 1)
    unit_choices <- if(input$show_faction_specific) all_unit_choices else base_unit_choices

    insertUI(
      selector = "#add_attacker",
      where = "beforeBegin",
      ui = div(class = "attacker_input",
        fluidRow(
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
    )
  }

  add_defender <- function() {
    defending_unit_count(defending_unit_count() + 1)
    unit_choices <- if(input$show_faction_specific) all_unit_choices else base_unit_choices

    insertUI(
      selector = "#add_defender",
      where = "beforeBegin",
      ui = div(class = "defender_input",
        fluidRow(
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
    )
  }

  # Sever Setup
  attacking_unit_count <- reactiveVal(0)
  defending_unit_count <- reactiveVal(0)


  # Action buttons to add units
  observeEvent(input$add_attacker, {
    add_attacker()
  })
  observeEvent(input$add_defender, {
    add_defender()
  })

  # Remove Last Attacker
  observeEvent(input$remove_attacker, {
    auc <- attacking_unit_count()
    if (auc > 0) {
      removeUI(
        selector = paste0("div.attacker_input:nth-of-type(", auc, ")"),
        multiple = FALSE
      )
      attacking_unit_count(auc - 1)
    }
  })

  # Remove Last Defender
  observeEvent(input$remove_defender, {
    duc <- defending_unit_count()
    if (duc > 0) {
      removeUI(
        selector = paste0("div.defender_input:nth-of-type(", duc, ")"),
        multiple = FALSE
      )
      defending_unit_count(duc - 1)
    }
  })

  # Reset
  observeEvent(input$clear, {
    removeUI(selector = "div.attacker_input", multiple = TRUE)
    removeUI(selector = "div.defender_input", multiple = TRUE)
    attacking_unit_count(0)
    defending_unit_count(0)
    updateCheckboxInput(session, "show_faction_specific", value = FALSE)
    add_attacker()
    add_defender()
    output$results <- renderDT(data.frame())
    output$metadata <- renderDT(data.frame())
    output$attacker_stats <- renderDT(data.frame())
    output$defender_stats <- renderDT(data.frame())
  })


  observeEvent(input$show_faction_specific, {
    unit_choices <- if(input$show_faction_specific) all_unit_choices else base_unit_choices

    # Update Attacker Selection Inputs
    auc <- attacking_unit_count()
    for (i in seq_len(auc)){
      selected <- input[[paste0("attacker_unit_", i)]]
      updateSelectInput(
        session,
        inputId = paste0("attacker_unit_", i),
        choices = unique(c(unit_choices, selected)),  # Keep prev selection
        selected = selected
      )
    }

    # Update Defender Selection Inputs
    duc <- defending_unit_count()
    for (i in seq_len(duc)){
      selected <- input[[paste0("defender_unit_", i)]]
      updateSelectInput(
        session,
        inputId = paste0("defender_unit_", i),
        choices = unique(c(unit_choices, selected)),  # Keep prev selection
        selected = selected
      )
    }
  })


  observeEvent(input$simulate, {

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

    # Stop if units not selected
    if(length(attackers) == 0 || length(defenders) == 0) {
      showNotification("Select both attacking and defending units.", type = "error")
      req(FALSE)
    }

    # Call simulate_battles() from simulate.py
    sim <- simulate_battles(attacker_units_dict, defender_units_dict, input$rounds)

    results <- sim[[1]]
    metadata <- sim[[2]]
    attacker_stats <- sim[[3]] %>%
      select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>%
      select(where(~ !all(.x == 0)))  # Remove Columns will all zeros
    defender_stats <- sim[[4]] %>%
      select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>%
      select(where(~ !all(.x == 0)))  # Remove Columns will all zeros

    output$results <- renderDT(results, options = list(dom = "t"))
    output$metadata <- renderDT(metadata, options = list(dom = "t"))
    output$attacker_stats <- renderDT(attacker_stats, options = list(dom = "t"))
    output$defender_stats <- renderDT(defender_stats, options = list(dom = "t"))
  })


  # Wiki Data Output
  output$all_units <- renderDT(df)

}

shinyApp(ui, server)