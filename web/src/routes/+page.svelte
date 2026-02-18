<script lang="ts">
	import UnitPanel from '$lib/components/UnitPanel.svelte';
	import ResultsDisplay from '$lib/components/ResultsDisplay.svelte';
	import { simulateBattles, type SimulationResults } from '$lib/simulation/simulate';

	let attackerUnits = $state<{ name: string; count: number }[]>([]);
	let defenderUnits = $state<{ name: string; count: number }[]>([]);
	let showFactionUnits = $state(false);
	let rounds = $state(500);
	let results = $state<SimulationResults | null>(null);
	let isSimulating = $state(false);

	function simulate() {
		if (attackerUnits.length === 0 || defenderUnits.length === 0) return;

		isSimulating = true;

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

	let canSimulate = $derived(attackerUnits.length > 0 && defenderUnits.length > 0 && !isSimulating);
</script>

<svelte:head>
	<title>TIBS - Twilight Imperium Battle Simulator</title>
</svelte:head>

<div class="relative z-10 max-w-[1600px] mx-auto px-4 sm:px-6 lg:px-8 py-6">

	<!-- Hero Header -->
	<div class="text-center mb-8">
		<h1 class="text-5xl sm:text-6xl font-extrabold tracking-tight font-orbitron mb-2">
			<span style="
				background: linear-gradient(135deg, #f59e0b 0%, #ef4444 50%, #a855f7 100%);
				-webkit-background-clip: text;
				-webkit-text-fill-color: transparent;
				background-clip: text;
			">BATTLE SIMULATOR</span>
		</h1>
		<p class="text-[var(--color-text-muted)] text-sm tracking-widest uppercase">
			Twilight Imperium IV &mdash; Monte Carlo Combat Engine
		</p>
	</div>

	<!-- Controls Bar -->
	<div class="panel mb-6 px-5 py-4">
		<div class="flex flex-wrap items-center gap-4 justify-between">
			<div class="flex items-center gap-5">
				<label class="flex items-center gap-2 text-sm cursor-pointer group">
					<div class="relative">
						<input
							type="checkbox"
							bind:checked={showFactionUnits}
							class="sr-only"
						/>
						<div class="w-9 h-5 rounded-full transition-colors"
							 style="background: {showFactionUnits ? 'var(--color-accent)' : 'var(--color-border)'};">
						</div>
						<div class="absolute top-0.5 left-0.5 w-4 h-4 rounded-full bg-white transition-transform"
							 style="transform: translateX({showFactionUnits ? '16px' : '0'});">
						</div>
					</div>
					<span class="text-[var(--color-text-muted)] group-hover:text-[var(--color-text)] transition-colors select-none"
						  onclick={() => showFactionUnits = !showFactionUnits}
						  role="button" tabindex="0"
						  onkeydown={(e) => e.key === 'Enter' && (showFactionUnits = !showFactionUnits)}>
						Faction Units
					</span>
				</label>

				<div class="flex items-center gap-3">
					<span class="text-xs text-[var(--color-text-muted)] uppercase tracking-wider">Simulations:</span>
					<input
						type="range"
						bind:value={rounds}
						min="100"
						max="2000"
						step="100"
						class="w-28 accent-[var(--color-accent)] cursor-pointer"
					/>
					<span class="text-sm font-mono text-[var(--color-accent)] w-12">{rounds}</span>
				</div>
			</div>

			<div class="flex gap-2">
				<button
					onclick={reset}
					class="px-4 py-2 rounded-lg text-sm font-medium border border-[var(--color-border)] text-[var(--color-text-muted)] hover:text-[var(--color-text)] hover:border-[var(--color-border-bright)] transition-all cursor-pointer"
				>
					Reset
				</button>
				<button
					onclick={simulate}
					disabled={!canSimulate}
					class="relative px-8 py-2 rounded-lg text-sm font-bold tracking-wider font-orbitron uppercase transition-all cursor-pointer disabled:opacity-40 disabled:cursor-not-allowed overflow-hidden"
					style="
						background: linear-gradient(135deg, #f59e0b, #d97706);
						color: #0f0a00;
						box-shadow: {canSimulate ? '0 0 20px rgba(245,158,11,0.4), 0 0 40px rgba(245,158,11,0.15)' : 'none'};
					"
				>
					{#if isSimulating}
						<span class="flex items-center gap-2">
							<svg class="animate-spin w-4 h-4" fill="none" viewBox="0 0 24 24">
								<circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
								<path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
							</svg>
							Simulating...
						</span>
					{:else}
						⚔ Simulate
					{/if}
				</button>
			</div>
		</div>
	</div>

	<!-- Battle Arena: Attacker | VS | Defender -->
	<div class="grid grid-cols-1 lg:grid-cols-[1fr_auto_1fr] gap-4 mb-8 items-start">
		<!-- Attacker Panel -->
		<div class="min-h-[500px] max-h-[75vh] flex flex-col">
			<UnitPanel
				label="Attacker"
				color="#ef4444"
				units={attackerUnits}
				onUpdate={(u) => attackerUnits = u}
				{showFactionUnits}
			/>
		</div>

		<!-- VS Divider -->
		<div class="hidden lg:flex flex-col items-center justify-center py-8 gap-3 self-center">
			<div class="w-px h-16 bg-gradient-to-b from-transparent via-[var(--color-border-bright)] to-transparent"></div>
			<span class="text-2xl font-extrabold font-orbitron vs-text text-[var(--color-accent)]">VS</span>
			<div class="w-px h-16 bg-gradient-to-b from-transparent via-[var(--color-border-bright)] to-transparent"></div>
		</div>
		<div class="flex lg:hidden items-center gap-3 my-1">
			<div class="flex-1 h-px bg-[var(--color-border)]"></div>
			<span class="text-lg font-extrabold font-orbitron vs-text text-[var(--color-accent)]">VS</span>
			<div class="flex-1 h-px bg-[var(--color-border)]"></div>
		</div>

		<!-- Defender Panel -->
		<div class="min-h-[500px] max-h-[75vh] flex flex-col">
			<UnitPanel
				label="Defender"
				color="#3b82f6"
				units={defenderUnits}
				onUpdate={(u) => defenderUnits = u}
				{showFactionUnits}
			/>
		</div>
	</div>

	<!-- Results -->
	{#if results}
		<ResultsDisplay {results} />
	{:else}
		<div class="panel p-12 text-center">
			<div class="text-4xl mb-4">🌌</div>
			<p class="text-[var(--color-text-muted)] text-base">
				Build your fleets above, then click
				<strong class="text-[var(--color-accent)] font-orbitron">⚔ SIMULATE</strong>
				to run the battle
			</p>
		</div>
	{/if}
</div>
