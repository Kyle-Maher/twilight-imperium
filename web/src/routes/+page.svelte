<script lang="ts">
	import UnitPanel from '$lib/components/UnitPanel.svelte';
	import ResultsDisplay from '$lib/components/ResultsDisplay.svelte';
	import { simulateBattles, type SimulationResults } from '$lib/simulation/simulate';

	let attackerUnits = $state<{ name: string; count: number }[]>([]);
	let defenderUnits = $state<{ name: string; count: number }[]>([]);
	let showFactionUnits = $state(false);
	let rounds = $state(50000);
	let results = $state<SimulationResults | null>(null);
	let isSimulating = $state(false);

	function simulate() {
		if (attackerUnits.length === 0 || defenderUnits.length === 0) return;

		isSimulating = true;

		// Use setTimeout to allow the UI to update with loading state
		setTimeout(() => {
			const attackerDict: Record<string, number> = {};
			for (const u of attackerUnits) {
				attackerDict[u.name] = (attackerDict[u.name] || 0) + u.count;
			}

			const defenderDict: Record<string, number> = {};
			for (const u of defenderUnits) {
				defenderDict[u.name] = (defenderDict[u.name] || 0) + u.count;
			}

			results = simulateBattles(attackerDict, defenderDict, rounds);
			isSimulating = false;
		}, 10);
	}

	function reset() {
		attackerUnits = [];
		defenderUnits = [];
		results = null;
	}
</script>

<svelte:head>
	<title>TIBS - Twilight Imperium Battle Simulator</title>
</svelte:head>

<div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
	<!-- Header -->
	<div class="text-center mb-8">
		<h1 class="text-4xl sm:text-5xl font-extrabold tracking-tight mb-2" style="font-family: 'Orbitron', sans-serif;">
			<span class="text-[var(--color-accent)]">Battle</span>
			<span class="text-[var(--color-text)]"> Simulator</span>
		</h1>
		<p class="text-[var(--color-text-muted)]">Monte Carlo simulation for Twilight Imperium 4 combat</p>
	</div>

	<!-- Controls Bar -->
	<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-4 mb-6">
		<div class="flex flex-wrap items-center gap-4 justify-between">
			<div class="flex items-center gap-4">
				<label class="flex items-center gap-2 text-sm cursor-pointer">
					<input
						type="checkbox"
						bind:checked={showFactionUnits}
						class="w-4 h-4 rounded bg-[var(--color-bg)] border-[var(--color-border)] text-[var(--color-accent)] focus:ring-[var(--color-accent)] cursor-pointer accent-[var(--color-accent)]"
					/>
					<span class="text-[var(--color-text-muted)]">Show Faction Units</span>
				</label>

				<div class="flex items-center gap-2">
					<label for="rounds" class="text-sm text-[var(--color-text-muted)]">Rounds:</label>
					<input
						id="rounds"
						type="range"
						bind:value={rounds}
						min="1000"
						max="100000"
						step="1000"
						class="w-32 accent-[var(--color-accent)] cursor-pointer"
					/>
					<span class="text-sm font-mono text-[var(--color-text)] w-16">{rounds.toLocaleString()}</span>
				</div>
			</div>

			<div class="flex gap-2">
				<button
					onclick={reset}
					class="px-4 py-2 rounded-lg text-sm font-medium border border-[var(--color-border)] bg-[var(--color-surface-hover)] hover:bg-[var(--color-border)] text-[var(--color-text)] transition-colors cursor-pointer"
				>
					Reset
				</button>
				<button
					onclick={simulate}
					disabled={attackerUnits.length === 0 || defenderUnits.length === 0 || isSimulating}
					class="px-6 py-2 rounded-lg text-sm font-bold text-[var(--color-bg)] transition-all cursor-pointer disabled:opacity-40 disabled:cursor-not-allowed"
					style="background: linear-gradient(135deg, var(--color-accent), #d97706); box-shadow: 0 0 20px rgba(245, 158, 11, 0.3);"
				>
					{#if isSimulating}
						Simulating...
					{:else}
						Simulate Battle
					{/if}
				</button>
			</div>
		</div>
	</div>

	<!-- Unit Panels -->
	<div class="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-8">
		<UnitPanel
			label="Attacker"
			color="#ef4444"
			units={attackerUnits}
			onUpdate={(u) => attackerUnits = u}
			{showFactionUnits}
		/>
		<UnitPanel
			label="Defender"
			color="#3b82f6"
			units={defenderUnits}
			onUpdate={(u) => defenderUnits = u}
			{showFactionUnits}
		/>
	</div>

	<!-- Results -->
	{#if results}
		<ResultsDisplay {results} />
	{:else}
		<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-12 text-center">
			<p class="text-[var(--color-text-muted)] text-lg">Add units to both sides and click <strong class="text-[var(--color-accent)]">Simulate Battle</strong> to see results.</p>
		</div>
	{/if}
</div>
