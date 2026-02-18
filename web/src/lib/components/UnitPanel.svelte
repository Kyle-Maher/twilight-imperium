<script lang="ts">
	import { allUnits, getCommonUnits, getFactionUnits } from '$lib/data/units';
	import { getUnitImage } from '$lib/data/unitImages';

	interface Props {
		label: string;
		color: string;
		units: { name: string; count: number }[];
		onUpdate: (units: { name: string; count: number }[]) => void;
		showFactionUnits: boolean;
	}

	let { label, color, units, onUpdate, showFactionUnits }: Props = $props();

	// Group available units by type for the picker grid
	const typeOrder = ['Fighter', 'Destroyer', 'Cruiser', 'Carrier', 'Dreadnought', 'War_Sun', 'Flagship', 'Infantry', 'Mechs'];
	const typeLabels: Record<string, string> = {
		Fighter: 'Fighter', Destroyer: 'Destroyer', Cruiser: 'Cruiser',
		Carrier: 'Carrier', Dreadnought: 'Dreadnought', War_Sun: 'War Sun',
		Flagship: 'Flagship', Infantry: 'Infantry', Mechs: 'Mech',
	};

	function availableUnits(): string[] {
		const base = getCommonUnits();
		const faction = showFactionUnits ? getFactionUnits() : [];
		return [...base, ...faction].sort();
	}

	function unitsByType(): Record<string, string[]> {
		const result: Record<string, string[]> = {};
		for (const name of availableUnits()) {
			const type = allUnits[name]?.Unit_Type ?? 'Other';
			if (!result[type]) result[type] = [];
			result[type].push(name);
		}
		return result;
	}

	function isInArmy(name: string): boolean {
		return units.some(u => u.name === name);
	}

	function getCount(name: string): number {
		return units.find(u => u.name === name)?.count ?? 0;
	}

	function addOrIncrement(name: string) {
		const existing = units.find(u => u.name === name);
		if (existing) {
			onUpdate(units.map(u => u.name === name ? { ...u, count: u.count + 1 } : u));
		} else {
			onUpdate([...units, { name, count: 1 }]);
		}
	}

	function decrement(name: string) {
		const existing = units.find(u => u.name === name);
		if (!existing) return;
		if (existing.count <= 1) {
			onUpdate(units.filter(u => u.name !== name));
		} else {
			onUpdate(units.map(u => u.name === name ? { ...u, count: u.count - 1 } : u));
		}
	}

	function remove(name: string) {
		onUpdate(units.filter(u => u.name !== name));
	}

	function setCount(name: string, count: number) {
		if (count <= 0) {
			onUpdate(units.filter(u => u.name !== name));
		} else {
			const existing = units.find(u => u.name === name);
			if (existing) {
				onUpdate(units.map(u => u.name === name ? { ...u, count } : u));
			} else {
				onUpdate([...units, { name, count }]);
			}
		}
	}

	let isAttacker = $derived(color === '#ef4444');
	let glowClass = $derived(isAttacker ? 'glow-attacker' : 'glow-defender');
	let borderStyle = $derived(`border-color: ${color}40;`);
	let headerGrad = $derived(
		isAttacker
			? 'linear-gradient(135deg, rgba(239,68,68,0.15) 0%, rgba(239,68,68,0.03) 100%)'
			: 'linear-gradient(135deg, rgba(59,130,246,0.15) 0%, rgba(59,130,246,0.03) 100%)'
	);
</script>

<div class="panel flex flex-col h-full" style="{borderStyle}">
	<!-- Panel Header -->
	<div class="px-5 py-4 border-b border-[var(--color-border)] flex items-center justify-between"
		 style="background: {headerGrad};">
		<div class="flex items-center gap-3">
			<div class="w-3 h-3 rounded-full" style="background: {color}; box-shadow: 0 0 8px {color};"></div>
			<h2 class="text-xl font-bold font-orbitron tracking-widest uppercase" style="color: {color};">{label}</h2>
		</div>
		<div class="text-xs text-[var(--color-text-muted)] font-mono">
			{units.reduce((s, u) => s + u.count, 0)} units
		</div>
	</div>

	<!-- Active Army Summary -->
	{#if units.length > 0}
		<div class="px-4 py-3 border-b border-[var(--color-border)] bg-[var(--color-surface)]">
			<div class="flex flex-wrap gap-2">
				{#each units as unit}
					<div class="flex items-center gap-1 px-2 py-1 rounded-lg text-xs font-medium"
						 style="background: {color}18; border: 1px solid {color}40; color: {color};">
						<span class="font-mono font-bold">{unit.count}×</span>
						<span>{unit.name}</span>
						<button
							onclick={() => remove(unit.name)}
							class="ml-1 opacity-60 hover:opacity-100 transition-opacity cursor-pointer leading-none"
							aria-label="Remove {unit.name}"
						>&times;</button>
					</div>
				{/each}
			</div>
		</div>
	{/if}

	<!-- Unit Picker Grid -->
	<div class="flex-1 overflow-y-auto p-4 space-y-5">
		{#if units.length === 0}
			<p class="text-center text-[var(--color-text-muted)] text-sm pt-2 pb-1 italic">
				Select units below to add them to your fleet
			</p>
		{/if}

		{#each typeOrder as type}
			{@const typeUnits = unitsByType()[type]}
			{#if typeUnits && typeUnits.length > 0}
				<div>
					<h4 class="text-xs font-bold uppercase tracking-widest text-[var(--color-text-muted)] mb-2 px-1 flex items-center gap-2">
						<span class="flex-1 h-px bg-[var(--color-border)]"></span>
						{typeLabels[type] ?? type}
						<span class="flex-1 h-px bg-[var(--color-border)]"></span>
					</h4>
					<div class="grid grid-cols-2 sm:grid-cols-3 gap-2">
						{#each typeUnits as unitName}
							{@const unitData = allUnits[unitName]}
							{@const count = getCount(unitName)}
							{@const active = count > 0}
							<div class="unit-card {active ? (isAttacker ? 'selected-attacker' : 'selected-defender') : ''}">
								<!-- Unit Image -->
								<div class="relative h-20 overflow-hidden rounded-t-[9px]">
									<img
										src={getUnitImage(unitData?.Unit_Type)}
										alt={unitName}
										class="w-full h-full object-cover object-center"
										style="filter: {active ? 'brightness(0.9)' : 'brightness(0.5) saturate(0.6)'}; transition: filter 0.2s;"
										onerror={(e) => { (e.target as HTMLImageElement).style.display = 'none'; }}
									/>
									{#if active}
										<!-- Count badge overlay -->
										<div class="absolute top-1 right-1 w-6 h-6 rounded-full flex items-center justify-center text-xs font-bold text-white"
											 style="background: {color}; box-shadow: 0 0 8px {color};">
											{count}
										</div>
									{/if}
									<!-- Sustain damage indicator -->
									{#if unitData?.Has_Sustain_Damage}
										<div class="absolute top-1 left-1 w-4 h-4 rounded-full bg-yellow-500/80 flex items-center justify-center" title="Sustain Damage">
											<span class="text-[8px] font-bold text-black">S</span>
										</div>
									{/if}
								</div>

								<!-- Unit Info -->
								<div class="p-2">
									<div class="text-xs font-semibold text-[var(--color-text)] truncate leading-tight mb-1" title={unitName}>
										{unitName.replace(' I', ' Ⅰ').replace(' II', ' Ⅱ')}
									</div>
									{#if unitData?.Faction_Name !== 'Common Unit'}
										<div class="text-[10px] text-[var(--color-text-muted)] truncate leading-tight mb-1.5" title={unitData?.Faction_Name}>
											{unitData?.Faction_Name}
										</div>
									{/if}
									<!-- Stats row -->
									<div class="flex gap-1 flex-wrap mb-2">
										<span class="stat-badge text-amber-400" title="Combat value">⚔ {unitData?.Combat_Value}</span>
										{#if unitData?.Shots > 1}
											<span class="stat-badge text-orange-400" title="Shots">×{unitData?.Shots}</span>
										{/if}
									</div>

									<!-- +/- Controls -->
									<div class="flex items-center justify-between gap-1">
										<button
											onclick={() => decrement(unitName)}
											disabled={!active}
											class="flex-1 h-6 rounded text-sm font-bold transition-colors cursor-pointer disabled:opacity-20 disabled:cursor-not-allowed"
											style="background: {color}22; border: 1px solid {color}44; color: {color};"
										>−</button>
										<button
											onclick={() => addOrIncrement(unitName)}
											class="flex-1 h-6 rounded text-sm font-bold transition-all cursor-pointer hover:brightness-110"
											style="background: {color}33; border: 1px solid {color}66; color: {color};"
										>+</button>
									</div>
								</div>
							</div>
						{/each}
					</div>
				</div>
			{/if}
		{/each}
	</div>
</div>
