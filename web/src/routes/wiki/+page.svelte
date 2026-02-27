<script lang="ts">
	import { allUnits, getFactions } from '$lib/data/units';

	let search = $state('');
	let factionFilter = $state('All');
	let typeFilter = $state('All');
	let sortColumn = $state('Unit_Name');
	let sortAsc = $state(true);

	const unitList = Object.values(allUnits);
	const factions = ['All', ...getFactions()];
	const unitTypes = ['All', ...new Set(unitList.map(u => u.Unit_Type)).values()].sort();

	const filtered = $derived(() => {
		let list = unitList;

		if (search) {
			const q = search.toLowerCase();
			list = list.filter(u =>
				u.Unit_Name.toLowerCase().includes(q) ||
				u.Faction_Name.toLowerCase().includes(q) ||
				u.Unit_Type.toLowerCase().includes(q)
			);
		}

		if (factionFilter !== 'All') {
			list = list.filter(u => u.Faction_Name === factionFilter);
		}

		if (typeFilter !== 'All') {
			list = list.filter(u => u.Unit_Type === typeFilter);
		}

		list = [...list].sort((a, b) => {
			const aVal = a[sortColumn as keyof typeof a];
			const bVal = b[sortColumn as keyof typeof b];
			if (typeof aVal === 'number' && typeof bVal === 'number') {
				return sortAsc ? aVal - bVal : bVal - aVal;
			}
			const aStr = String(aVal ?? '');
			const bStr = String(bVal ?? '');
			return sortAsc ? aStr.localeCompare(bStr) : bStr.localeCompare(aStr);
		});

		return list;
	});

	function toggleSort(col: string) {
		if (sortColumn === col) {
			sortAsc = !sortAsc;
		} else {
			sortColumn = col;
			sortAsc = true;
		}
	}

	function sortIndicator(col: string): string {
		if (sortColumn !== col) return '';
		return sortAsc ? ' \u25B2' : ' \u25BC';
	}
</script>

<svelte:head>
	<title>Wiki - TIBS</title>
</svelte:head>

<div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
	<div class="text-center mb-8">
		<h1 class="text-4xl font-extrabold tracking-tight mb-2" style="font-family: 'Orbitron', sans-serif;">
			<span class="text-[var(--color-accent)]">Unit</span>
			<span class="text-[var(--color-text)]"> Database</span>
		</h1>
		<p class="text-[var(--color-text-muted)]">Browse all {unitList.length} units from Twilight Imperium Fourth Edition</p>
	</div>

	<!-- Filters -->
	<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-4 mb-6">
		<div class="flex flex-wrap gap-4 items-center">
			<input
				type="text"
				bind:value={search}
				placeholder="Search units..."
				class="flex-1 min-w-48 px-4 py-2 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm placeholder:text-[var(--color-text-muted)] focus:outline-none focus:ring-2 focus:ring-[var(--color-accent)]"
			/>
			<select
				bind:value={factionFilter}
				class="px-3 py-2 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm focus:outline-none focus:ring-2 focus:ring-[var(--color-accent)] cursor-pointer"
			>
				{#each factions as faction}
					<option value={faction}>{faction}</option>
				{/each}
			</select>
			<select
				bind:value={typeFilter}
				class="px-3 py-2 rounded-lg bg-[var(--color-bg)] border border-[var(--color-border)] text-[var(--color-text)] text-sm focus:outline-none focus:ring-2 focus:ring-[var(--color-accent)] cursor-pointer"
			>
				{#each unitTypes as type}
					<option value={type}>{type}</option>
				{/each}
			</select>
			<span class="text-sm text-[var(--color-text-muted)]">{filtered().length} units</span>
		</div>
	</div>

	<!-- Table -->
	<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] overflow-hidden">
		<div class="overflow-x-auto">
			<table class="w-full text-sm">
				<thead>
					<tr class="border-b border-[var(--color-border)] bg-[var(--color-bg)]">
						{#each [
							['Unit_Name', 'Unit'],
							['Faction_Name', 'Faction'],
							['Unit_Type', 'Type'],
							['Combat_Value', 'Combat'],
							['Shots', 'Shots'],
							['Has_Sustain_Damage', 'Sustain'],
							['Cost', 'Cost'],
							['Is_Ship', 'Ship'],
							['Is_Ground_Force', 'Ground']
						] as [col, label]}
							<th
								class="px-3 py-3 text-left text-[var(--color-text-muted)] font-medium cursor-pointer hover:text-[var(--color-text)] transition-colors select-none whitespace-nowrap"
								onclick={() => toggleSort(col)}
							>
								{label}{sortIndicator(col)}
							</th>
						{/each}
					</tr>
				</thead>
				<tbody>
					{#each filtered() as unit}
						<tr class="border-b border-[var(--color-border)]/30 hover:bg-[var(--color-surface-hover)] transition-colors">
							<td class="px-3 py-2.5 font-medium">{unit.Unit_Name}</td>
							<td class="px-3 py-2.5 text-[var(--color-text-muted)]">{unit.Faction_Name}</td>
							<td class="px-3 py-2.5">
								<span class="px-2 py-0.5 rounded-full text-xs font-medium bg-[var(--color-bg)] border border-[var(--color-border)]">
									{unit.Unit_Type}
								</span>
							</td>
							<td class="px-3 py-2.5">{unit.Combat}</td>
							<td class="px-3 py-2.5">{unit.Shots}</td>
							<td class="px-3 py-2.5">
								{#if unit.Has_Sustain_Damage}
									<span class="text-green-400">Yes</span>
								{:else}
									<span class="text-[var(--color-text-muted)]">-</span>
								{/if}
							</td>
							<td class="px-3 py-2.5">{unit.Cost}</td>
							<td class="px-3 py-2.5">{unit.Is_Ship ? 'Yes' : '-'}</td>
							<td class="px-3 py-2.5">{unit.Is_Ground_Force ? 'Yes' : '-'}</td>
						</tr>
					{/each}
				</tbody>
			</table>
		</div>
	</div>
</div>
