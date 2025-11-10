from simulate import all_units_df, simulate_battles
from shiny import App, ui, render
import pandas as pd

test_attacking_units = {"Dreadnought": 3, "Infantry": 3}
test_defending_units = {"Cruiser": 3, "Infantry": 5}
test_rounds = 100

test_results, test_metadata, test_attacking_stats, test_defending_stats = simulate_battles(test_attacking_units, test_defending_units, test_rounds)
unit_choices = ["Dreadnought", "Infantry", "Cruiser"]
default_unit = "Dreadnought"

app_ui = ui.page_fluid(
    ui.h2("Twilight Imperium Resources"),

    ui.navset_tab(
        ui.nav_panel(
            "Battle Simulator",
            ui.br(),
            ui.row(
                    ui.column(
                    6,
                    ui.row(
                        ui.column(
                            2,
                            ui.input_action_button("simulate", "Simulate")
                        ),
                        ui.column(
                            2,
                            ui.input_action_button("reset", "Reset")
                        )
                    ),
                    ui.br(),
                    ui.input_checkbox("show_faciton_units", "Show Faction Specific Units", value=False),
                    ui.br(),
                    ui.h4("Attacking Units"),
                    ui.input_action_button("add_attacker", "Add Attacking Unit", class_ = "btn-primary"),
                    ui.output_ui("attacker_inputs"),
                    ui.br(),
                    ui.br(),
                    ui.h4("Defending Units"),
                    ui.input_action_button("add_defender", "Add Defending Unit", class_ = "btn-danger")
                ),
                ui.column(1),
                ui.column(
                    4,
                    ui.output_data_frame("results"),
                    ui.br(),
                    ui.output_data_frame("metadata"),
                    ui.br(),
                    ui.output_data_frame("attacking_stats"),
                    ui.br(),
                    ui.output_data_frame("defending_stats")
                )
            )
        ),
        ui.nav_panel(
            "Wiki Data",
            ui.output_data_frame("all_units")
        )
    )
)

def server(input, output, session):


    # Keep track of how many attacker units have been added
    attacker_units = []

    @output
    @render.ui
    def attacker_inputs():
        n_clicks = input.add_attacker()
        if n_clicks > len(attacker_units):
            attacker_units.append(n_clicks)  # store a placeholder

        # Build a list of UI elements for each added unit
        ui_elements = []
        for i, _ in enumerate(attacker_units, start=1):
            ui_elements.append(
                ui.div(
                    {"class": "attacker_input"},
                    ui.row(
                        ui.column(7,
                                  ui.input_select(f"attacker_unit_{i}",
                                                  "Type",
                                                  choices=unit_choices,
                                                  selected=default_unit)
                        ),
                        ui.column(3,
                                  ui.input_numeric(f"attacker_counter_{i}",
                                                   "Count",
                                                   value=1,
                                                   min=1)
                        )
                    )
                )
            )
        return ui_elements

    @output
    @render.data_frame
    def all_units():
        return all_units_df[["Faction_Name", "Unit_Type", "Cost", "Combat_Value", "Shots", "Move_Value", "Capacity_Value"]]
    
    @output
    @render.data_frame
    def results():
        return test_results.reset_index().rename(columns={"index": " "})
    
    @output
    @render.data_frame
    def metadata():
        return test_metadata.reset_index().rename(columns={"index": " "})

    @output
    @render.data_frame
    def attacking_stats():
        return test_attacking_stats[["Name", "Unit_Combat_Value", "Shots"]].rename(columns={"index": " "})
    
    @output
    @render.data_frame
    def defending_stats():
        return test_defending_stats[["Name", "Unit_Combat_Value", "Shots"]].rename(columns={"index": " "})
    

app = App(app_ui, server)
