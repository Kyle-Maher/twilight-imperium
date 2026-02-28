/**
 * Converts data/clean/all_units_df.csv → web/src/lib/data/units.ts
 * Usage: node src/convert_units.mjs
 */

import { readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const repoRoot = join(__dirname, '..');

const CSV_PATH = join(repoRoot, 'data', 'clean', 'all_units_df.csv');
const OUT_PATH = join(repoRoot, 'web', 'src', 'lib', 'data', 'units.ts');

// RFC-4180 CSV row parser. Handles quoted fields with embedded commas and "" escapes.
function parseCSVLine(line) {
	const fields = [];
	let i = 0;
	while (i <= line.length) {
		if (i === line.length) {
			fields.push('');
			break;
		}
		if (line[i] === '"') {
			let field = '';
			i++; // skip opening quote
			while (i < line.length) {
				if (line[i] === '"') {
					if (line[i + 1] === '"') {
						field += '"';
						i += 2;
					} else {
						i++; // skip closing quote
						break;
					}
				} else {
					field += line[i++];
				}
			}
			fields.push(field);
			if (line[i] === ',') i++;
		} else {
			const end = line.indexOf(',', i);
			if (end === -1) {
				fields.push(line.slice(i));
				break;
			}
			fields.push(line.slice(i, end));
			i = end + 1;
		}
	}
	return fields;
}

const BOOL_FIELDS = new Set([
	'Has_Sustain_Damage', 'Is_Ship', 'Is_Ground_Force',
	'Has_Anti_Fighter', 'Has_Bombardment', 'Has_Space_Cannon',
]);

const NUM_FIELDS = new Set([
	'Combat_Value', 'Shots', 'Cost_Value', 'Move_Value', 'Capacity_Value',
	'Anti_Fighter_Value', 'Anti_Fighter_Shots',
	'Bombardment_Value', 'Bombardment_Shots',
	'Space_Cannon_Value', 'Space_Cannon_Shots',
]);

const raw = readFileSync(CSV_PATH, 'utf8').replace(/\r\n/g, '\n').replace(/\r/g, '\n');
const lines = raw.split('\n').filter(l => l.trim() !== '');

const headers = parseCSVLine(lines[0]);

const units = [];
const seenNames = new Set();
for (let i = 1; i < lines.length; i++) {
	const values = parseCSVLine(lines[i]);
	if (values.length < headers.length) continue;

	const unit = {};
	for (let j = 0; j < headers.length; j++) {
		const key = headers[j];
		const val = values[j];
		if (BOOL_FIELDS.has(key)) {
			unit[key] = val === 'True';
		} else if (NUM_FIELDS.has(key)) {
			unit[key] = parseFloat(val);
		} else {
			unit[key] = val;
		}
	}

	if (seenNames.has(unit.Unit_Name)) {
		console.warn(`Skipping duplicate Unit_Name: "${unit.Unit_Name}" (${unit.Faction_Name})`);
		continue;
	}
	seenNames.add(unit.Unit_Name);
	units.push(unit);
}

const interfaceBlock = `export interface UnitData {
\tUnit_Name: string;
\tFaction_Name: string;
\tUnit_Abilities: string;
\tStandard_Abilities: string;
\tHas_Sustain_Damage: boolean;
\tCost: string;
\tCombat: string;
\tCombat_Value: number;
\tShots: number;
\tMove: string;
\tCapacity: string;
\tUnit_Type: string;
\tIs_Ship: boolean;
\tIs_Ground_Force: boolean;
\tCost_Value: number;
\tMove_Value: number;
\tCapacity_Value: number;
\tHas_Anti_Fighter: boolean;
\tAnti_Fighter_Value: number;
\tAnti_Fighter_Shots: number;
\tHas_Bombardment: boolean;
\tBombardment_Value: number;
\tBombardment_Shots: number;
\tHas_Space_Cannon: boolean;
\tSpace_Cannon_Value: number;
\tSpace_Cannon_Shots: number;
}`;

const entries = units
	.map((u, i) => {
		const comma = i < units.length - 1 ? ',' : '';
		return `\t${JSON.stringify(u.Unit_Name)}: ${JSON.stringify(u)}${comma}`;
	})
	.join('\n');

const helperFunctions = `
export function getCommonUnits(): string[] {
\treturn Object.keys(allUnits).filter(name => allUnits[name].Faction_Name === 'Common Unit');
}

export function getFactionUnits(): string[] {
\treturn Object.keys(allUnits).filter(name => allUnits[name].Faction_Name !== 'Common Unit');
}

export function getAllUnitNames(): string[] {
\treturn Object.keys(allUnits);
}

export function getFactions(): string[] {
\tconst factions = new Set(Object.values(allUnits).map(u => u.Faction_Name));
\tfactions.delete('Common Unit');
\treturn ['Common Unit', ...Array.from(factions).sort()];
}
`;

const output = `${interfaceBlock}

export const allUnits: Record<string, UnitData> = {
${entries}
};
${helperFunctions}`;

writeFileSync(OUT_PATH, output, 'utf8');
console.log(`Wrote ${units.length} units to web/src/lib/data/units.ts`);
