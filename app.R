Create this in python instead of r


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
        )
      ),
      fluidRow(
        column(4,
          DTOutput("results")
        ),
        column(4,
          DTOutput("meta_data")
        )
      ),
      fluidRow(
        column(8,
          DTOutput("attacker_stats")
        )
      ),
      fluidRow(
        column(8,
          DTOutput("defender_stats")
        )
      ),
      fluidRow(
        column(8,
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

  # Reset
  observeEvent(input$clear, {
    removeUI(selector = "div.attacker_input", multiple = TRUE)
    removeUI(selector = "div.defender_input", multiple = TRUE)
    attacking_unit_count(0)
    defending_unit_count(0)
    updateCheckboxInput(session, "show_faction_specific", value = FALSE)
    add_attacker()
    add_defender()
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

    # Call simulate_battles() from simulate.py
    sim <- simulate_battles(attacker_units_dict, defender_units_dict)

    output$results <- renderDT(sim[[1]], options = list(dom = "t"))
    output$meta_data <- renderDT(sim[[2]], options = list(dom = "t"))
    output$attacker_stats <- renderDT(sim[[3]], options = list(dom = "t"))
    output$defender_stats <- renderDT(sim[[4]], options = list(dom = "t"))
    output$current_selection <- renderPrint(list(attackers, defenders))
  })



  # Wiki Data Output
  output$all_units <- renderDT(df)

}

shinyApp(ui, server)