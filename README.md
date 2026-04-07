# twilight-imperium

The web version can be found here: https://kyle-maher.github.io/twilight-imperium/. Utilizes data scraped from the twilight imperium wiki: https://twilight-imperium.fandom.com/wiki/Factions. Python jupyter notebooks handle the webage scrape and cleaning. The simulation logic was initially written manually in Python and was refactored by Claude Code into typescript. Similarly, the manually written R Shiny app was refactored into the current web app using Claude Code.

The legacy version of the R Shiny app can be found here: https://kylemaher.shinyapps.io/TIBS/. Note the integration between R and Python causes a long startup time which results in “An error has occurred” on the first open if the app is not currently running. Simply close the tab, re-open the link, and wait a few seconds. Shiny for python likely would have been a better choice, but I was sneaking this in as a class project requirement where the use of R was expected.

# Original File Descriptions

## scrape.ipynb

Utilizes BeautifulSoup from bs4 to scrape data from tables on the Twilight Imperium Wiki.

## clean.ipynb

Merges ground and space units into all_units_df.csv. Identifies Anti-fighter, Bombardment, and Space Cannon ability values.

## simulate.ipynb

Contains simulate_battles() which runs the Monte Carlo simulation returning the win resutls, round metadata, and unit stats for attacking and defending factions.

## simulate.py

Created from simulate.ipynb using "jupyter nbconvert --to script simulate.ipynb --TemplateExporter.exclude_input_prompt=True".

## app.R

R shiny app that reads the copied versions of all_units_df.csv and simulate.py in the same directory. Creates a python virtual environment on the shinyapps.io cloud server to run simulate.py.
