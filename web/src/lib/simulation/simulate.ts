import { allUnits } from '$lib/data/units';

export interface UnitStats {
	Name: string;
	Unit_Type: string;
	Unit_Combat_Value: number;
	Shots: number;
	Has_Sustain_Damage: boolean;
	Anti_Fighter_Value: number;
	Anti_Fighter_Shots: number;
	Bombardment_Value: number;
	Bombardment_Shots: number;
	Space_Cannon_Value: number;
	Space_Cannon_Shots: number;
	Has_Anti_Fighter: boolean;
	Has_Bombardment: boolean;
	Has_Space_Cannon: boolean;
}

export interface WinPercentages {
	space: { attacker: number; defender: number; draw: number };
	ground: { attacker: number; defender: number; draw: number };
	overall: { attacker: number; defender: number; draw: number };
}

export interface CombatMetadata {
	space: { attackerWins: number; defenderWins: number; draws: number; averageRounds: number; combatsSimulated: number };
	ground: { attackerWins: number; defenderWins: number; draws: number; averageRounds: number; combatsSimulated: number };
}

export interface SimulationResults {
	winPercentages: WinPercentages;
	metadata: CombatMetadata;
	attackerUnitStats: UnitStats[];
	defenderUnitStats: UnitStats[];
}

function rollD10(): number {
	return Math.floor(Math.random() * 10) + 1;
}

function splitUnits(units: Record<string, number>): { ships: Record<string, number>; groundForces: Record<string, number> } {
	const ships: Record<string, number> = {};
	const groundForces: Record<string, number> = {};

	for (const [unit, value] of Object.entries(units)) {
		const unitData = allUnits[unit];
		if (!unitData) {
			console.warn(`Warning! Unit: ${unit} not found!`);
			continue;
		}
		if (unitData.Is_Ship) {
			ships[unit] = value;
		} else if (unitData.Is_Ground_Force) {
			groundForces[unit] = value;
		}
	}

	return { ships, groundForces };
}

function getUnitStats(factionUnits: Record<string, number>): UnitStats[] {
	const stats: UnitStats[] = [];

	for (const [unit, count] of Object.entries(factionUnits)) {
		const unitData = allUnits[unit];
		for (let i = 0; i < count; i++) {
			stats.push({
				Name: unit,
				Unit_Type: unitData.Unit_Type,
				Unit_Combat_Value: unitData.Combat_Value,
				Shots: unitData.Shots,
				Has_Sustain_Damage: unitData.Has_Sustain_Damage,
				Anti_Fighter_Value: unitData.Anti_Fighter_Value,
				Anti_Fighter_Shots: unitData.Anti_Fighter_Shots,
				Bombardment_Value: unitData.Bombardment_Value,
				Bombardment_Shots: unitData.Bombardment_Shots,
				Space_Cannon_Value: unitData.Space_Cannon_Value,
				Space_Cannon_Shots: unitData.Space_Cannon_Shots,
				Has_Anti_Fighter: unitData.Has_Anti_Fighter,
				Has_Bombardment: unitData.Has_Bombardment,
				Has_Space_Cannon: unitData.Has_Space_Cannon
			});
		}
	}

	stats.sort((a, b) => {
		if (a.Has_Sustain_Damage !== b.Has_Sustain_Damage) return a.Has_Sustain_Damage ? -1 : 1;
		if (a.Unit_Combat_Value !== b.Unit_Combat_Value) return a.Unit_Combat_Value - b.Unit_Combat_Value;
		return b.Shots - a.Shots;
	});

	return stats;
}

function sortUnits(units: UnitStats[]): void {
	units.sort((a, b) => {
		if (a.Has_Sustain_Damage !== b.Has_Sustain_Damage) return a.Has_Sustain_Damage ? -1 : 1;
		if (a.Unit_Combat_Value !== b.Unit_Combat_Value) return a.Unit_Combat_Value - b.Unit_Combat_Value;
		return b.Shots - a.Shots;
	});
}

function getAntiFighterHits(factionUnits: UnitStats[]): number {
	let hits = 0;
	for (const unit of factionUnits) {
		if (unit.Has_Anti_Fighter) {
			for (let i = 0; i < unit.Anti_Fighter_Shots; i++) {
				if (rollD10() >= unit.Anti_Fighter_Value) {
					hits++;
				}
			}
		}
	}
	return hits;
}

function assignAntiFighterHits(hits: number, factionUnits: UnitStats[]): void {
	let remaining = hits;
	while (remaining > 0) {
		const fighterIdx = factionUnits.findIndex(u => u.Unit_Type === 'Fighter');
		if (fighterIdx === -1) break;
		factionUnits.splice(fighterIdx, 1);
		remaining--;
	}
}

function getHits(factionUnits: UnitStats[]): number {
	let hits = 0;
	for (const unit of factionUnits) {
		for (let i = 0; i < unit.Shots; i++) {
			if (rollD10() >= unit.Unit_Combat_Value) {
				hits++;
			}
		}
	}
	return hits;
}

function assignHits(hits: number, factionUnits: UnitStats[]): void {
	let remaining = hits;
	while (remaining > 0 && factionUnits.length > 0) {
		if (factionUnits[0].Has_Sustain_Damage) {
			factionUnits[0].Has_Sustain_Damage = false;
			sortUnits(factionUnits);
		} else {
			factionUnits.splice(0, 1);
		}
		remaining--;
	}
}

function getBombardmentHits(factionUnits: UnitStats[]): number {
	let hits = 0;
	for (const unit of factionUnits) {
		if (unit.Has_Bombardment) {
			for (let i = 0; i < unit.Bombardment_Shots; i++) {
				if (rollD10() >= unit.Bombardment_Value) {
					hits++;
				}
			}
		}
	}
	return hits;
}

function round1(value: number): number {
	return Math.round(value * 10) / 10;
}

export function simulateBattles(
	attackerUnits: Record<string, number>,
	defenderUnits: Record<string, number>,
	rounds: number = 100
): SimulationResults {
	let factionASpaceWins = 0, factionBSpaceWins = 0, spaceDraws = 0;
	let factionAGroundWins = 0, factionBGroundWins = 0, groundDraws = 0;
	const spaceRoundCounts: number[] = [];
	const groundRoundCounts: number[] = [];

	let lastAttackerStats: UnitStats[] = [];
	let lastDefenderStats: UnitStats[] = [];

	for (let i = 0; i < rounds; i++) {
		const { ships: aShipsDict, groundForces: aGroundDict } = splitUnits(attackerUnits);
		const { ships: bShipsDict, groundForces: bGroundDict } = splitUnits(defenderUnits);

		const aShips = getUnitStats(aShipsDict);
		const bShips = getUnitStats(bShipsDict);
		const aGround = getUnitStats(aGroundDict);
		const bGround = getUnitStats(bGroundDict);

		if (i === 0) {
			lastAttackerStats = [...aShips, ...aGround].map(u => ({ ...u }));
			lastDefenderStats = [...bShips, ...bGround].map(u => ({ ...u }));
		}

		// Anti-Fighter Barrage
		const aAFHits = getAntiFighterHits(aShips);
		const bAFHits = getAntiFighterHits(bShips);
		assignAntiFighterHits(aAFHits, bShips);
		assignAntiFighterHits(bAFHits, aShips);

		// Space Combat
		let spaceRoundCount = 0;
		while (aShips.length > 0 && bShips.length > 0) {
			const aHits = getHits(aShips);
			const bHits = getHits(bShips);
			assignHits(aHits, bShips);
			assignHits(bHits, aShips);
			spaceRoundCount++;
		}
		spaceRoundCounts.push(spaceRoundCount);

		if (aShips.length > 0) {
			factionASpaceWins++;

			// Bombardment
			const bombardmentHits = getBombardmentHits(aShips);
			assignHits(bombardmentHits, bGround);

			// Ground Combat
			let groundRoundCount = 0;
			while (aGround.length > 0 && bGround.length > 0) {
				const aHits = getHits(aGround);
				const bHits = getHits(bGround);
				assignHits(aHits, bGround);
				assignHits(bHits, aGround);
				groundRoundCount++;
			}
			groundRoundCounts.push(groundRoundCount);

			if (aGround.length > 0) {
				factionAGroundWins++;
			} else if (bGround.length > 0) {
				factionBGroundWins++;
			} else {
				groundDraws++;
			}
		} else if (bShips.length > 0) {
			factionBSpaceWins++;
		} else {
			spaceDraws++;
		}
	}

	const avgSpaceRounds = spaceRoundCounts.length > 0
		? spaceRoundCounts.reduce((a, b) => a + b, 0) / spaceRoundCounts.length
		: 0;
	const avgGroundRounds = groundRoundCounts.length > 0
		? groundRoundCounts.reduce((a, b) => a + b, 0) / groundRoundCounts.length
		: 0;

	const totalSpaceGames = factionASpaceWins + factionBSpaceWins + spaceDraws;
	const totalGroundGames = factionAGroundWins + factionBGroundWins + groundDraws;

	const spacePctA = totalSpaceGames > 0 ? factionASpaceWins / totalSpaceGames : 0;
	const spacePctB = totalSpaceGames > 0 ? factionBSpaceWins / totalSpaceGames : 0;
	const spacePctDraw = totalSpaceGames > 0 ? spaceDraws / totalSpaceGames : 0;

	const groundPctA = totalGroundGames > 0 ? factionAGroundWins / totalGroundGames : 0;
	const groundPctB = totalGroundGames > 0 ? factionBGroundWins / totalGroundGames : 0;
	const groundPctDraw = totalGroundGames > 0 ? groundDraws / totalGroundGames : 0;

	// Overall: conditional probability
	const overallA = spacePctA * groundPctA;
	const overallB = spacePctB + (spacePctA * groundPctB);
	const overallDraw = spacePctDraw + (spacePctA * groundPctDraw);

	return {
		winPercentages: {
			space: { attacker: round1(spacePctA * 100), defender: round1(spacePctB * 100), draw: round1(spacePctDraw * 100) },
			ground: { attacker: round1(groundPctA * 100), defender: round1(groundPctB * 100), draw: round1(groundPctDraw * 100) },
			overall: { attacker: round1(overallA * 100), defender: round1(overallB * 100), draw: round1(overallDraw * 100) }
		},
		metadata: {
			space: { attackerWins: factionASpaceWins, defenderWins: factionBSpaceWins, draws: spaceDraws, averageRounds: round1(avgSpaceRounds), combatsSimulated: totalSpaceGames },
			ground: { attackerWins: factionAGroundWins, defenderWins: factionBGroundWins, draws: groundDraws, averageRounds: round1(avgGroundRounds), combatsSimulated: totalGroundGames }
		},
		attackerUnitStats: lastAttackerStats,
		defenderUnitStats: lastDefenderStats
	};
}
