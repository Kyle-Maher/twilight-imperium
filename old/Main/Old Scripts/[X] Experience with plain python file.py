import pandas as pd
from bs4 import BeautifulSoup
import requests

URL = 'https://twilight-imperium.fandom.com/wiki/Flagship'

# Fetch the webpage
page = requests.get(URL)
soup = BeautifulSoup(page.content, 'html.parser')

# Locate Each Faction Heading
headings = soup.find_all('h3')
clean_headings = []
for heading in headings:
    if heading.find('span', class_='mw-headline'):
        clean_headings.append(heading)
headings = clean_headings

# Locate Each Heading Span
spans = []
for heading in headings:
    span = heading.find('span', class_='mw-headline')
    if span:
        spans.append(span)

# Locate Faction Name from each Span
factions = []
for span in spans:
    name = span.get_text(strip=True).strip('[]')
    factions.append(name)

# Locate Each Faction Table from each Heading
tables = []
for heading in headings:
    table = heading.find_next_sibling()
    tables.append(table)
# Currently missing The Nomad's flagship v2

all_flagships = []
for table in tables:
    rows = table.find_all('tr')
    name = rows[0].get_text(strip=True)
    ability = rows[1].get_text(strip=True).strip('Sustain Damage')

    paragraphs = rows[1].find_all('p')
    bonus_abilities = [paragraph.get_text(strip=True) for paragraph in paragraphs]
    bonus_abilities = ' & '.join(bonus_abilities)

    cells = rows[2].find_all('td')
    row_data = [cell.get_text(strip=True) for cell in cells]
    cost = row_data[0]
    combat = row_data[1]
    move = row_data[2]
    capacity = row_data[3]

    flagship_stats = {'Name': name, 'Ability': ability, 'Bonus_Abilities': bonus_abilities, 'Cost': cost, 'Combat': combat, 'Move': move, 'Capacity': capacity}
    all_flagships.append(flagship_stats)

df = pd.DataFrame(all_flagships, index=factions)
df.loc['The Barony of Letnev']['Ability']