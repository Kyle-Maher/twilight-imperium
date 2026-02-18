<script lang="ts">
	import { allUnits, getCommonUnits, getFactionUnits } from '$lib/data/units';

	interface Props {
		label: string;
		color: string;
		units: { name: string; count: number }[];
		onUpdate: (units: { name: string; count: number }[]) => void;
		showFactionUnits: boolean;
	}

	let { label, color, units, onUpdate, showFactionUnits }: Props = $props();

	function availableUnits(): string[] {
		const base = getCommonUnits();
		const faction = showFactionUnits ? getFactionUnits() : [];
		return [...base, ...faction].sort();
	}

	function addUnit() {
		const available = availableUnits();
		const used = new Set(units.map(u => u.name));
		const next = available.find(u => !used.has(u)) || available[0];
		onUpdate([...units, { name: next, count: 1 }]);
	}

	function removeUnit(index: number) {
		const updated = units.filter((_, i) => i !== index);
		onUpdate(updated);
	}

	function updateUnitName(index: number, name: string) {
		const updated = units.map((u, i) => i === index ? { ...u, name } : u);
		onUpdate(updated);
	}

	function updateUnitCount(index: number, count: number) {
		const updated = units.map((u, i) => i === index ? { ...u, count: Math.max(1, count) } : u);
		onUpdate(updated);
	}
</script>

<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] overflow-hidden">
	<div class="px-4 py-3 border-b border-[var(--color-border)] flex items-center justify-between"
		 style="background: linear-gradient(135deg, {color}15, {color}05);">
		<h3 class="text-lg font-bold" style="color: {color};">{label}</h3>
		<button
			onclick={addUnit}
			class="px-3 py-1 rounded-lg text-sm font-medium border border-[var(--color-border)] bg-[var(--color-surface-hover)] hover:bg-[var(--color-border)] text-[var(--color-text)] transition-colors cursor-pointer"
		>
			+ Add Unit
		</button>
	</div>

	<div class="p-4 space-y-2">
		{#if units.length === 0}
			<p class="text-sm text-[var(--color-text-muted)] italic text-center py-4">No units added. Click "Add Unit" to begin.</p>
		{/if}

		{#each units as unit, i}
			<div class="flex items-center gap-2">
				<select
					value={unit.name}
					onchange={(e) => updateUnitName(i, (e.target as HTMLSelectElement).value)}
					class="flex-1 px-3 py-2 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm focus:outline-none focus:ring-2 focus:ring-[var(--color-accent)] appearance-none cursor-pointer"
				>
					{#each availableUnits() as unitName}
						<option value={unitName}>{unitName} ({allUnits[unitName]?.Faction_Name})</option>
					{/each}
				</select>

				<div class="flex items-center gap-1">
					<button
						onclick={() => updateUnitCount(i, unit.count - 1)}
						class="w-8 h-8 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm font-bold hover:bg-[var(--color-surface-hover)] transition-colors cursor-pointer flex items-center justify-center"
					>-</button>
					<input
						type="number"
						value={unit.count}
						min="1"
						onchange={(e) => updateUnitCount(i, parseInt((e.target as HTMLInputElement).value) || 1)}
						class="w-14 px-2 py-2 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm text-center focus:outline-none focus:ring-2 focus:ring-[var(--color-accent)] [appearance:textfield] [&::-webkit-outer-spin-button]:appearance-none [&::-webkit-inner-spin-button]:appearance-none"
					/>
					<button
						onclick={() => updateUnitCount(i, unit.count + 1)}
						class="w-8 h-8 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm font-bold hover:bg-[var(--color-surface-hover)] transition-colors cursor-pointer flex items-center justify-center"
					>+</button>
				</div>

				<button
					onclick={() => removeUnit(i)}
					class="w-8 h-8 rounded-lg bg-[var(--color-bg)] border border-red-900/50 text-red-400 text-sm hover:bg-red-900/30 transition-colors cursor-pointer flex items-center justify-center"
				>&times;</button>
			</div>
		{/each}
	</div>
</div>
