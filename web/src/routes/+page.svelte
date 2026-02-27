<script lang="ts">
	import UnitPanel from '$lib/components/UnitPanel.svelte';
	import ResultsDisplay from '$lib/components/ResultsDisplay.svelte';
	import type { SimulationResults } from '$lib/simulation/simulate';

	let attackerUnits = $state<{ name: string; count: number }[]>([
		{ name: 'Dreadnought II', count: 1 },
		{ name: 'Carrier', count: 1 },
		{ name: 'Fighter', count: 1 },
		{ name: 'Mech', count: 1 },
		{ name: 'Infantry', count: 1 },
	]);
	let defenderUnits = $state<{ name: string; count: number }[]>([
		{ name: 'Cruiser', count: 1 },
		{ name: 'Destroyer II', count: 1 },
		{ name: 'Fighter', count: 1 },
		{ name: 'Mech', count: 1 },
		{ name: 'Infantry', count: 1 },
	]);
	let showFactionUnits = $state(false);
	let rounds = $state(50000);
	let results = $state<SimulationResults | null>(null);
	let isSimulating = $state(false);
	let simulationError = $state<string | null>(null);
	let worker = $state<Worker | null>(null);

	function simulate() {
		if (attackerUnits.length === 0 || defenderUnits.length === 0) return;

		isSimulating = true;
		simulationError = null;

		const attackerDict: Record<string, number> = {};
		for (const u of attackerUnits) {
			attackerDict[u.name] = (attackerDict[u.name] || 0) + u.count;
		}

		const defenderDict: Record<string, number> = {};
		for (const u of defenderUnits) {
			defenderDict[u.name] = (defenderDict[u.name] || 0) + u.count;
		}

		const w = new Worker(new URL('../lib/simulation/simulate.worker.ts', import.meta.url), { type: 'module' });
		worker = w;

		const warningTimer = setTimeout(() => {
			if (isSimulating) simulationError = 'Simulation is taking a while. Try cancelling and reducing the number of rounds or units.';
		}, 10000);

		w.onmessage = (e) => {
			clearTimeout(warningTimer);
			worker = null;
			isSimulating = false;
			if (e.data.type === 'result') {
				results = e.data.data;
				simulationError = null;
			} else {
				simulationError = e.data.message;
			}
		};

		w.onerror = () => {
			clearTimeout(warningTimer);
			worker = null;
			isSimulating = false;
			simulationError = 'Simulation failed.';
		};

		w.postMessage({ attackerUnits: attackerDict, defenderUnits: defenderDict, rounds });
	}

	function cancelSimulation() {
		worker?.terminate();
		worker = null;
		isSimulating = false;
		simulationError = null;
	}

	function reset() {
		window.location.reload();
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
		<p class="text-[var(--color-text-muted)]">Simulate Twilight Imperium Fourth Edition Combat</p>
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
				{#if isSimulating}
					<button
						onclick={cancelSimulation}
						class="w-40 py-2 rounded-lg text-sm font-bold text-white bg-red-700 hover:bg-red-600 transition-colors cursor-pointer"
					>
						Cancel
					</button>
				{:else}
					<button
						onclick={simulate}
						disabled={attackerUnits.length === 0 || defenderUnits.length === 0}
						class="w-40 py-2 rounded-lg text-sm font-bold text-[var(--color-bg)] transition-all cursor-pointer disabled:opacity-40 disabled:cursor-not-allowed"
						style="background: linear-gradient(135deg, var(--color-accent), #d97706); box-shadow: 0 0 20px rgba(245, 158, 11, 0.3);"
					>
						Simulate Battles
					</button>
				{/if}
			</div>
		</div>
	</div>

	<!-- Simulation Warning -->
	{#if simulationError}
		<div class="rounded-xl border border-amber-500/40 bg-amber-500/10 px-4 py-3 mb-6 text-sm text-amber-400">
			{simulationError}
		</div>
	{/if}

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
	{#if isSimulating}
		<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-12 text-center animate-pulse">
			<p class="text-[var(--color-text-muted)] text-lg">Running simulation…</p>
		</div>
	{:else if results}
		<ResultsDisplay {results} />
	{:else}
		<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-12 text-center">
			<p class="text-[var(--color-text-muted)] text-lg">Add units to both sides and click <strong class="text-[var(--color-accent)]">Simulate Battle</strong> to see results.</p>
		</div>
	{/if}
</div>
