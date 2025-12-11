################################################################################
# To Do:
# Adjust unit selection so that previous selections are not available
# Add filters to the wiki data
# Add Loading Bar
# Add Capacity Considerations
# Add Space Cannon Units and Combat Steps
################################################################################


library(shiny) # Shiny App
library(reticulate) # Runing Python Code
library(readr) # Read CSV
library(DT) # Table Renders
library(dplyr) # DataFrame manipulaitons
library(tidyr) # DataFrame manipulaitons
library(binom) # Confidence Intervals
library(ggplot2) # Plots

# If Running Locally:
# setwd("TwilightImperiumBattleSimulator")
# use_virtualenv("../.venv", required = TRUE)

# If Publishing:
virtualenv_create("r-reticulate")
virtualenv_install("r-reticulate", packages = c("pandas", "numpy"))
use_virtualenv("r-reticulate", required = TRUE)

source_python("simulate.py")
df <- read_csv("all_units_df.csv")

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
          sliderInput("rounds", "Battles to Simulate", 100, 2000, 500, step = 100, ticks = FALSE),
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
          plotOutput("donut_chart"),
          br(),
          DTOutput("metadata"),
          br(),
          DTOutput("ci"),
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
  tabPanel("Info",
    fluidPage(
      fluidRow(
        column(12,
          h3("Data:"),
          h4(
            tags$a(
              href = "https://twilight-imperium.fandom.com/wiki/Twilight_Imperium_Wiki",
              "Twilight Imperium Wiki",
              target = "_blank"   # open link in new tab
            )
          ),
          h3("Created by:"),
          h4("Kyle Maher")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # Functions
  format_table_output <- function(table, table_name) {
    formatted_table <- datatable(
      table,
      rownames = FALSE,
      options = list(dom = "t"),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-weight: bold; font-size: 16px;',
        table_name
      )
    )
    return(formatted_table)
  }

  add_attacker <- function() {
    attacking_unit_count(attacking_unit_count() + 1)
    unit_choices <- if(input$show_faction_specific) all_unit_choices else base_unit_choices

    insertUI(
      selector = "#add_attacker",
      where = "beforeBegin",
      ui = div(class = "attacker_input",
        fluidRow(
          column(5,
            style = "padding-right:5px;",   # reduce right padding
            selectInput(
              inputId = paste0("attacker_unit_", attacking_unit_count()),
              label = "Type",
              choices = unit_choices,
              selected = default_unit
            )
          ),
          column(2,
            style = "padding-left:5px;",    # reduce left padding
            numericInput(
              inputId = paste0("attacker_counter_", attacking_unit_count()),
              label = "Count",
              value = 1,
              min = 1
            )
          ),
          column(3), # Spacer
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
          column(5,
            style = "padding-right:5px;",   # reduce right padding
            selectInput(
              inputId = paste0("defender_unit_", defending_unit_count()),
              label = "Type",
              choices = unit_choices,
              selected = default_unit
            )
          ),
          column(2,
            style = "padding-left:5px;",    # reduce left padding
            numericInput(
              inputId = paste0("defender_counter_", defending_unit_count()),
              label = "Count",
              value = 1,
              min = 1
            )
          ),
          column(3), # Spacer
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

    results <- sim[[1]] %>%
      mutate(
        "Combat Type" = c("Space", "Ground", "Overall")
      ) %>%
      select(
        "Combat Type",
        "Attacker Wins",
        "Defender Wins",
        "Draw"
      )

    metadata <- sim[[2]] %>%
      mutate(
        "Combat Type" = c("Space", "Ground")
      ) %>%
      select(
        "Combat Type",
        "Attacker Wins",
        "Defender Wins",
        "Draws",
        "Average Rounds",
        "Combats Simulated"
      )

    attacker_stats <- sim[[3]] %>%
      select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>%
      select(where(~ !all(.x == 0))) %>%  # Remove Columns will all zeros
      rename(
        "Combat Value" = "Unit_Combat_Value",
        "Type" = "Unit_Type"
      )
    defender_stats <- sim[[4]] %>%
      select(-Has_Anti_Fighter, -Has_Bombardment, -Has_Space_Cannon) %>%
      select(where(~ !all(.x == 0))) %>%  # Remove Columns will all zeros
      rename(
        "Combat Value" = "Unit_Combat_Value",
        "Type" = "Unit_Type"
      )

    output$metadata <- renderDT(format_table_output(metadata, "Battles Simulated:"))
    output$attacker_stats <- renderDT(format_table_output(attacker_stats, "Attacker Unit Stats:"))
    output$defender_stats <- renderDT(format_table_output(defender_stats, "Defender Unit Stats:"))

    # Compute Confidence Intervals

    # Bonferroni-adjusted Confidence Level
    alpha <- 0.05 / 3
    level = 1 - alpha

    # Wilson Confidence Interval Bounds
    get_wilson_ci <- function(k, n, conf.level = level) {
      ci <- binom.confint(k, n, conf.level = level, methods = "wilson")
      return(c(lower = ci$lower, upper = ci$upper))
    }

    metadata_ci <- metadata %>%
      rename(
        "Attacker_Wins" = "Attacker Wins",
        "Defender_Wins" = "Defender Wins",
        "Combats_Simulated" = "Combats Simulated"
      ) %>%
      rowwise() %>%
      mutate(
        ci_attacker = list(get_wilson_ci(Attacker_Wins, Combats_Simulated)),
        lwr_attacker = ci_attacker[["lower"]],
        upr_attacker = ci_attacker[["upper"]],
        
        ci_defender = list(get_wilson_ci(Defender_Wins, Combats_Simulated)),
        lwr_defender = ci_defender[["lower"]],
        upr_defender = ci_defender[["upper"]],
        
        ci_draw = list(get_wilson_ci(Draws, Combats_Simulated)),
        lwr_draw = ci_draw[["lower"]],
        upr_draw = ci_draw[["upper"]]
      ) %>%
      ungroup() %>%
      select(Combats_Simulated,
            lwr_attacker, upr_attacker,
            lwr_defender, upr_defender,
            lwr_draw, upr_draw) %>%
      round(2)

    ci <- metadata_ci %>%
      mutate(
        "Combat Type" = c("Space", "Ground")
      ) %>%
      rowwise() %>%
      mutate(
        "Attacker 95% CI" = paste(lwr_attacker, "-", upr_attacker),
        "Defender 95% CI" = paste(lwr_defender, "-", upr_defender),
        "Draw 95% CI" = paste(lwr_draw, "-", upr_draw),
      ) %>%
      select(
        "Combat Type",
        "Attacker 95% CI",
        "Defender 95% CI",
        "Draw 95% CI"
      )

    output$ci <- renderDT({format_table_output(ci, "Confidence Intervals:")})

    # Plot Outputs
    results_long <- results %>%
      mutate("Combat Type" = c("Space", "Ground", "Overall")) %>%
      pivot_longer(
        cols = c("Attacker Wins", "Defender Wins", "Draw"),
        names_to = "Battle Result",
        values_to = "Percentage"
      ) %>%
      mutate("Combat Type" = factor(
        `Combat Type`,
        levels = c("Space", "Ground", "Overall")
      ))


    # Custom colors (match button colors)
    colors <- c(
      "Attacker Wins" = "#1f77b4",
      "Defender Wins" = "#d62728",
      "Draw"          = "#7f7f7f"
    )

    plot_data <- results_long %>%
      group_by(`Combat Type`) %>%
      mutate(
        Fraction = Percentage / sum(Percentage),
        ymax = cumsum(Fraction),
        ymin = c(0, head(ymax, n = -1)),
        label_pos = (ymin + ymax) / 2,
        label = paste0(Percentage, "%")
      )

    output$donut_chart <- renderPlot({
      ggplot(plot_data) +
        geom_rect(
          aes(
            ymin = ymin,
            ymax = ymax,
            xmin = 3,
            xmax = 4,
            fill = `Battle Result`
          )
        ) +
        # Percentage Labels
        geom_text(
          aes(
            x = 3.5,
            y = label_pos,
            label = label
          ),
          size = 5.5,
          color = "black"
          # fontface = "bold"
        ) +
        scale_fill_manual(values = colors) +
        coord_polar(theta = "y") +
        xlim(c(1, 4)) +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          legend.position = "right",
          # Facet Wrap Label Size
          strip.text = element_text(
            size = 16,
            face = "bold"
          )
        ) +
        facet_wrap(~ `Combat Type`)
    })

  })
  # End Simulate Button Effects



  # Wiki Data Output
  output$all_units <- renderDT(df)

}

shinyApp(ui, server)
