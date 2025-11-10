#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np

pd.set_option('display.max_columns', None)


# In[2]:


dtype_dict = {
    'Faction_Name': str,
    'Unit_Abilities': str,
    'Standard_Abilities': str,
    'Has_Sustain_Damage': bool,
    'Cost': str,        # 1 (2x) for infantry and fighters
    'Combat': str,      # Contains Value and Shots
    'Combat_Value': int,
    'Shots': int,
    'Move': str,        # ^ to indicate upgradeable 
    'Capacity': str,    # ^ to indicate upgradeable 
    'Unit_Type': str,
    'Is_Ship': bool,
    'Is_Ground_Force': bool,
    'Cost_Value' : str,
    'Move_Value' : str,
    'Capacity_Value' : str,
    'Has_Anti_Fighter': bool,
    'Anti_Fighter_Value': int,
    'Anti_Fighter_Shots': int,
    'Has_Bombardment': bool,
    'Bombardment_Value': int,
    'Bombardment_Shots': int,
    'Has_Space_Cannon': bool,
    'Space_Cannon_Value': int,
    'Space_Cannon_Shots': int
}

all_units_df = pd.read_csv('all_units_df.csv', index_col='Unit_Name', dtype=dtype_dict)


# ### Functions

# In[3]:


# Split Units into Ships and Ground Forces

def split_units(units):
    ships = {}
    ground_forces = {}

    for unit, value in units.items():
        if all_units_df.loc[unit]['Is_Ship']:
            ships[unit] = value
        elif all_units_df.loc[unit]['Is_Ground_Force']:
            ground_forces[unit] = value
        else:
            print(f"Warning! Unit: {unit} not found!")

    return ships, ground_forces


# In[4]:


# Find Combat Values

def get_unit_stats(faction_units):
    faction_unit_stats = []
    for unit in faction_units:
        number_units = faction_units[unit]
        for i in range(number_units):
            name = unit
            unit_combat_value = int(all_units_df.loc[unit]['Combat_Value'])
            has_sustain_damage = all_units_df.loc[unit]['Has_Sustain_Damage']
            shots = all_units_df.loc[unit]['Shots']

            # New Parameters
            anti_fighter_value = all_units_df.loc[unit]['Anti_Fighter_Value'] if 'Anti_Fighter_Value' in all_units_df.columns else None
            anti_fighter_shots = all_units_df.loc[unit]['Anti_Fighter_Shots'] if 'Anti_Fighter_Shots' in all_units_df.columns else None
            bombardment_value = all_units_df.loc[unit]['Bombardment_Value'] if 'Bombardment_Value' in all_units_df.columns else None
            bombardment_shots = all_units_df.loc[unit]['Bombardment_Shots'] if 'Bombardment_Shots' in all_units_df.columns else None
            space_cannon_value = all_units_df.loc[unit]['Space_Cannon_Value'] if 'Space_Cannon_Value' in all_units_df.columns else None
            space_cannon_shots = all_units_df.loc[unit]['Space_Cannon_Shots'] if 'Space_Cannon_Shots' in all_units_df.columns else None
            has_anti_fighter = all_units_df.loc[unit]['Has_Anti_Fighter']
            has_bombardment = all_units_df.loc[unit]['Has_Bombardment']
            has_space_cannon = all_units_df.loc[unit]['Has_Space_Cannon']

            unit_stats = {
                'Name': name,
                'Unit_Combat_Value': unit_combat_value,
                'Shots': shots,
                'Has_Sustain_Damage': has_sustain_damage,
                'Anti_Fighter_Value': anti_fighter_value,
                'Anti_Fighter_Shots': anti_fighter_shots,
                'Bombardment_Value': bombardment_value,
                'Bombardment_Shots': bombardment_shots,
                'Space_Cannon_Value': space_cannon_value,
                'Space_Cannon_Shots': space_cannon_shots,
                'Has_Anti_Fighter': has_anti_fighter,
                'Has_Bombardment': has_bombardment,
                'Has_Space_Cannon': has_space_cannon
            }
            faction_unit_stats.append(unit_stats)

    # Sort the units
    faction_unit_stats.sort(key=lambda x: (x['Has_Sustain_Damage'], x['Unit_Combat_Value'], -x['Shots']), reverse=True)

    return faction_unit_stats


# In[5]:


# Determine Anti Fighter Barrage Hits

def get_anti_fighter_hits(faction_units):
    hits = 0
    for unit in faction_units:
        if unit['Has_Anti_Fighter']:
            for i in range(unit['Anti_Fighter_Shots']):
                roll = np.random.randint(1, 11)
                if roll >= unit['Anti_Fighter_Value']:
                    hits += 1
    return hits


# In[6]:


# Determine Hits

def get_hits(faction_units):
    hits = 0
    for unit in faction_units:
        for i in range(unit['Shots']):
            roll = np.random.randint(1, 11)
            if roll >= unit['Unit_Combat_Value']:
                hits += 1
    return hits


# In[7]:


# Assign Hits

def assign_hits(hits, faction_units):
    while hits > 0 and faction_units:
        if faction_units[0]['Has_Sustain_Damage']:
            faction_units[0]['Has_Sustain_Damage'] = False
            faction_units.sort(key = lambda x: (x['Has_Sustain_Damage'], x['Unit_Combat_Value'], -x['Shots']), reverse=True)
        else:
            faction_units.remove(faction_units[0])
        hits -= 1


# In[8]:


# Determine Bombardment Hits

def get_bombardment_hits(faction_units):
    hits = 0
    for unit in faction_units:
        if unit['Has_Bombardment']:
            for i in range(unit['Bombardment_Shots']):
                roll = np.random.randint(1, 11)
                if roll >= unit['Bombardment_Value']:
                    hits += 1
    return hits


# ### Simulate Battles

# In[9]:


# For the purpose of this simulation faction_A is the attacker and faction_B is the defender
# If faction_B wins there is no ground combat.

# To Do:
# Account for Space Cannon

def simulate_battles(attacker_units, defender_units, rounds = 100):

    # Ensure Integer Inputs
    attacker_units = dict((k, int(v)) for k, v in attacker_units.items())
    attacker_units = dict((k, int(v)) for k, v in defender_units.items())

    faction_A_space_wins, faction_B_space_wins, space_draws = 0, 0, 0
    faction_A_ground_wins, faction_B_ground_wins, ground_draws = 0, 0, 0
    space_round_counts = []
    ground_round_counts = []

    for i in range(0, rounds):

        faction_A_units = attacker_units
        faction_B_units = defender_units

        # Split Ground and Space Units
        faction_A_ships, faction_A_ground_forces = split_units(faction_A_units)
        faction_B_ships, faction_B_ground_forces = split_units(faction_B_units)

        # Find Combat Values
        faction_A_ships = get_unit_stats(faction_A_ships)
        faction_B_ships = get_unit_stats(faction_B_ships)
        faction_A_ground_forces = get_unit_stats(faction_A_ground_forces)
        faction_B_ground_forces = get_unit_stats(faction_B_ground_forces)

        # Store Unit Stats for Reference
        faction_A_unit_stats = pd.concat([pd.DataFrame(faction_A_ships), pd.DataFrame(faction_A_ground_forces)]).reset_index(drop=True)
        faction_B_unit_stats = pd.concat([pd.DataFrame(faction_B_ships), pd.DataFrame(faction_B_ground_forces)]).reset_index(drop=True)

        # Conduct Space Cannon (in progress)


        # ANTI FIGHTER BARRAGE
        # Determine Hits
        faction_A_hits = get_anti_fighter_hits(faction_A_ships)
        faction_B_hits = get_anti_fighter_hits(faction_B_ships)
        # Assign Hits
        assign_hits(faction_A_hits, faction_B_ships)
        assign_hits(faction_B_hits, faction_A_ships)    


        space_round_count = 0
        while faction_A_ships and faction_B_ships:
            # Determine Hits
            faction_A_hits = get_hits(faction_A_ships)
            faction_B_hits = get_hits(faction_B_ships)

            # Assign Hits
            assign_hits(faction_A_hits, faction_B_ships)
            assign_hits(faction_B_hits, faction_A_ships)

            space_round_count += 1

        space_round_counts.append(space_round_count)

        if faction_A_ships:
            faction_A_space_wins += 1

            # BOMBARDMENT
            # Determine Hits From 'A' Ships
            faction_A_hits = get_bombardment_hits(faction_A_ships)
            # Assign Hits to 'B' Ground Forces
            assign_hits(faction_A_hits, faction_B_ground_forces)

            ground_round_count = 0
            while faction_A_ground_forces and faction_B_ground_forces:
                # Determine Hits
                faction_A_hits = get_hits(faction_A_ground_forces)
                faction_B_hits = get_hits(faction_B_ground_forces)

                # Assign Hits
                assign_hits(faction_A_hits, faction_B_ground_forces)
                assign_hits(faction_B_hits, faction_A_ground_forces)

                ground_round_count += 1

            ground_round_counts.append(ground_round_count)

            if faction_A_ground_forces:
                faction_A_ground_wins += 1
            elif faction_B_ground_forces:
                faction_B_ground_wins += 1
            else:
                ground_draws += 1

        elif faction_B_ships:
            faction_B_space_wins += 1
        else:
            space_draws += 1


    average_num_space_rounds = sum(space_round_counts) / len(space_round_counts)
    average_num_ground_rounds = sum(ground_round_counts) / len(ground_round_counts) if ground_round_counts else 0
    total_space_games = faction_A_space_wins + faction_B_space_wins + space_draws
    total_ground_games = faction_A_ground_wins + faction_B_ground_wins + ground_draws

    combat_metadata = pd.DataFrame([{'Average Rounds': average_num_space_rounds, 'Combats Simulated': total_space_games},
                                    {'Average Rounds': average_num_ground_rounds, 'Combats Simulated': total_ground_games}],
                                    index = ['Space', 'Ground'])

    df = pd.DataFrame([{'Faction A wins': faction_A_space_wins, 'Faction B wins': faction_B_space_wins, 'Draw': space_draws},
                    {'Faction A wins': faction_A_ground_wins, 'Faction B wins': faction_B_ground_wins, 'Draw': ground_draws}],
                    index = ['Space Percentages', 'Ground Percentages'])

    divisors = pd.Series([total_space_games, total_ground_games if total_ground_games > 0 else 1], index=df.index)
    df = df.div(divisors, axis=0)

    faction_A_wins_both = df.loc['Space Percentages', 'Faction A wins'] * df.loc['Ground Percentages', 'Faction A wins']
    faction_B_wins_either = df.loc['Space Percentages', 'Faction B wins'] + (df.loc['Space Percentages', 'Faction A wins'] * df.loc['Ground Percentages', 'Faction B wins'])
    draw_either = df.loc['Space Percentages', 'Draw'] + df.loc['Ground Percentages', 'Draw']
    df.loc['Overall Percentages'] = {'Faction A wins': faction_A_wins_both, 'Faction B wins': faction_B_wins_either, 'Draw': draw_either}
    df = df * 100


    # Results:

    # print(f'Average Number of Space Rounds: {average_num_space_rounds:.2f}')
    # print(f'Average Number of Ground Rounds: {average_num_ground_rounds:.2f}')
    # print(f'Number of Space Games Ran: {total_space_games}')
    # print(f'Number of Ground Games Ran: {total_ground_games}')

    return (df.round(1), combat_metadata.round(1), faction_A_unit_stats, faction_B_unit_stats)


# In[ ]:


# a = {'Dreadnought':3, 'Infantry':1}
# d = {'Cruiser':5, 'Infantry':2}

# results, metadata, attacker_stats, defender_stats = simulate_battles(a, d, 100)


# In[ ]:


# results


# In[ ]:


# metadata


# In[ ]:


# attacker_stats


# In[ ]:


# defender_stats

