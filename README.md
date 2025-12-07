# twilight-imperium

Fun challenges using the board game twilight imperium 4.0 to practice code.
Published app: https://kylemaher.shinyapps.io/TIBS/

## scrape.ipynb

Utilizes BeautifulSoup from bs4 to scrape data from tables on the Twilight Imperium Wiki.
https://twilight-imperium.fandom.com/wiki/Twilight_Imperium_Wiki.

## clean.ipynb

Merges ground and space units into all_units_df.csv. Identifies Anti-fighter, Bombardment, and Space Cannon ability values.

## simulate.ipynb

Contains simulate_battles() which runs the Monte Carlo simulation returning the win resutls, round metadata, and unit stats for attacking and defending factions.

## simulate.py

Created from simulate.ipynb using "jupyter nbconvert --to script simulate.ipynb --TemplateExporter.exclude_input_prompt=True".

## app.R

R shiny app that reads the copied versions of all_units_df.csv and simulate.py in the same directory. Creates a python virtual environment on the shinyapps.io cloud server to run simulate.py.
