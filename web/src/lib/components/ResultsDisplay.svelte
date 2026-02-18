<script lang="ts">
	import DonutChart from './DonutChart.svelte';
	import type { SimulationResults } from '$lib/simulation/simulate';

	interface Props {
		results: SimulationResults;
	}

	let { results }: Props = $props();

	const { winPercentages: wp, metadata: md } = $derived(results);

	// Determine overall winner for hero display
	let winner = $derived(
		wp.overall.attacker > wp.overall.defender
			? { label: 'ATTACKER WINS', color: '#ef4444', pct: wp.overall.attacker }
			: wp.overall.defender > wp.overall.attacker
			? { label: 'DEFENDER WINS', color: '#3b82f6', pct: wp.overall.defender }
			: { label: 'CONTESTED', color: '#a855f7', pct: wp.overall.draw }
	);
</script>

<div class="space-y-5">

	<!-- Victory Banner -->
	<div class="panel px-6 py-5 text-center relative overflow-hidden">
		<div class="absolute inset-0 opacity-10"
			 style="background: radial-gradient(ellipse at center, {winner.color} 0%, transparent 70%);">
		</div>
		<div class="relative">
			<div class="text-xs uppercase tracking-widest text-[var(--color-text-muted)] mb-1 font-orbitron">
				Simulation Result
			</div>
			<div class="text-3xl sm:text-4xl font-extrabold font-orbitron mb-1"
				 style="color: {winner.color}; text-shadow: 0 0 30px {winner.color}60;">
				{winner.label}
			</div>
			<div class="text-[var(--color-text-muted)] text-sm">
				<span class="font-mono text-lg font-bold" style="color: {winner.color};">{winner.pct}%</span>
				overall win rate across {md.space.combatsSimulated + md.ground.combatsSimulated} simulations
			</div>
		</div>
	</div>

	<!-- Win % Charts -->
	<div class="panel p-6">
		<h3 class="text-sm font-bold uppercase tracking-widest text-[var(--color-text-muted)] text-center mb-6 font-orbitron">
			Win Probabilities
		</h3>
		<div class="grid grid-cols-1 sm:grid-cols-3 gap-8">
			<DonutChart
				title="Space Combat"
				attacker={wp.space.attacker}
				defender={wp.space.defender}
				draw={wp.space.draw}
			/>
			<DonutChart
				title="Ground Combat"
				attacker={wp.ground.attacker}
				defender={wp.ground.defender}
				draw={wp.ground.draw}
			/>
			<DonutChart
				title="Overall"
				attacker={wp.overall.attacker}
				defender={wp.overall.defender}
				draw={wp.overall.draw}
			/>
		</div>

		<!-- Legend -->
		<div class="flex justify-center gap-6 mt-6 text-xs text-[var(--color-text-muted)]">
			<span class="flex items-center gap-2">
				<span class="w-3 h-3 rounded-full bg-red-500" style="box-shadow: 0 0 6px #ef4444;"></span>
				Attacker
			</span>
			<span class="flex items-center gap-2">
				<span class="w-3 h-3 rounded-full bg-blue-500" style="box-shadow: 0 0 6px #3b82f6;"></span>
				Defender
			</span>
			<span class="flex items-center gap-2">
				<span class="w-3 h-3 rounded-full bg-purple-500" style="box-shadow: 0 0 6px #a855f7;"></span>
				Draw
			</span>
		</div>
	</div>

	<!-- Combat Stats -->
	<div class="panel overflow-hidden">
		<div class="px-5 py-3 border-b border-[var(--color-border)]">
			<h3 class="text-sm font-bold uppercase tracking-widest text-[var(--color-text-muted)] font-orbitron">Combat Statistics</h3>
		</div>
		<div class="grid grid-cols-1 sm:grid-cols-2 divide-y sm:divide-y-0 sm:divide-x divide-[var(--color-border)]">
			<!-- Space -->
			<div class="p-5">
				<div class="text-xs uppercase tracking-wider text-[var(--color-text-muted)] mb-3 font-semibold">Space Combat</div>
				<div class="space-y-2">
					<div class="flex justify-between items-center text-sm">
						<span class="text-red-400">Attacker wins</span>
						<span class="font-mono font-bold text-red-400">{wp.space.attacker}%</span>
					</div>
					<div class="w-full h-1.5 rounded-full bg-[var(--color-border)] overflow-hidden">
						<div class="h-full rounded-full bg-red-500" style="width: {wp.space.attacker}%; box-shadow: 0 0 8px #ef4444;"></div>
					</div>
					<div class="flex justify-between items-center text-sm">
						<span class="text-blue-400">Defender wins</span>
						<span class="font-mono font-bold text-blue-400">{wp.space.defender}%</span>
					</div>
					<div class="w-full h-1.5 rounded-full bg-[var(--color-border)] overflow-hidden">
						<div class="h-full rounded-full bg-blue-500" style="width: {wp.space.defender}%; box-shadow: 0 0 8px #3b82f6;"></div>
					</div>
					<div class="flex justify-between text-xs text-[var(--color-text-muted)] pt-1">
						<span>Avg rounds: <span class="font-mono text-[var(--color-text)]">{md.space.averageRounds}</span></span>
						<span>Simulated: <span class="font-mono text-[var(--color-text)]">{md.space.combatsSimulated}</span></span>
					</div>
				</div>
			</div>
			<!-- Ground -->
			<div class="p-5">
				<div class="text-xs uppercase tracking-wider text-[var(--color-text-muted)] mb-3 font-semibold">Ground Combat</div>
				<div class="space-y-2">
					<div class="flex justify-between items-center text-sm">
						<span class="text-red-400">Attacker wins</span>
						<span class="font-mono font-bold text-red-400">{wp.ground.attacker}%</span>
					</div>
					<div class="w-full h-1.5 rounded-full bg-[var(--color-border)] overflow-hidden">
						<div class="h-full rounded-full bg-red-500" style="width: {wp.ground.attacker}%; box-shadow: 0 0 8px #ef4444;"></div>
					</div>
					<div class="flex justify-between items-center text-sm">
						<span class="text-blue-400">Defender wins</span>
						<span class="font-mono font-bold text-blue-400">{wp.ground.defender}%</span>
					</div>
					<div class="w-full h-1.5 rounded-full bg-[var(--color-border)] overflow-hidden">
						<div class="h-full rounded-full bg-blue-500" style="width: {wp.ground.defender}%; box-shadow: 0 0 8px #3b82f6;"></div>
					</div>
					<div class="flex justify-between text-xs text-[var(--color-text-muted)] pt-1">
						<span>Avg rounds: <span class="font-mono text-[var(--color-text)]">{md.ground.averageRounds}</span></span>
						<span>Simulated: <span class="font-mono text-[var(--color-text)]">{md.ground.combatsSimulated}</span></span>
					</div>
				</div>
			</div>
		</div>
	</div>

	<!-- Unit Stats -->
	{#if results.attackerUnitStats.length > 0 || results.defenderUnitStats.length > 0}
		<div class="grid grid-cols-1 lg:grid-cols-2 gap-4">
			{#if results.attackerUnitStats.length > 0}
				<div class="panel overflow-hidden">
					<div class="px-4 py-3 border-b border-[var(--color-border)]"
						 style="background: linear-gradient(135deg, rgba(239,68,68,0.1), transparent);">
						<h3 class="text-sm font-bold text-red-400 font-orbitron uppercase tracking-wider">Attacker Units</h3>
					</div>
					<div class="overflow-x-auto">
						<table class="w-full text-xs">
							<thead>
								<tr class="border-b border-[var(--color-border)]">
									<th class="px-4 py-2.5 text-left text-[var(--color-text-muted)] font-medium">Unit</th>
									<th class="px-4 py-2.5 text-right text-[var(--color-text-muted)] font-medium">Combat</th>
									<th class="px-4 py-2.5 text-right text-[var(--color-text-muted)] font-medium">Shots</th>
									<th class="px-4 py-2.5 text-center text-[var(--color-text-muted)] font-medium">Sustain</th>
								</tr>
							</thead>
							<tbody>
								{#each results.attackerUnitStats as unit}
									<tr class="border-b border-[var(--color-border)]/30 hover:bg-red-500/5 transition-colors">
										<td class="px-4 py-2.5 font-medium">{unit.Name}</td>
										<td class="px-4 py-2.5 text-right font-mono text-amber-400">{unit.Unit_Combat_Value}</td>
										<td class="px-4 py-2.5 text-right font-mono">{unit.Shots}</td>
										<td class="px-4 py-2.5 text-center">
											{#if unit.Has_Sustain_Damage}
												<span class="inline-block w-4 h-4 rounded-full bg-yellow-500/80 text-[8px] font-bold text-black flex items-center justify-center">S</span>
											{:else}
												<span class="text-[var(--color-text-muted)]">—</span>
											{/if}
										</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>
				</div>
			{/if}

			{#if results.defenderUnitStats.length > 0}
				<div class="panel overflow-hidden">
					<div class="px-4 py-3 border-b border-[var(--color-border)]"
						 style="background: linear-gradient(135deg, rgba(59,130,246,0.1), transparent);">
						<h3 class="text-sm font-bold text-blue-400 font-orbitron uppercase tracking-wider">Defender Units</h3>
					</div>
					<div class="overflow-x-auto">
						<table class="w-full text-xs">
							<thead>
								<tr class="border-b border-[var(--color-border)]">
									<th class="px-4 py-2.5 text-left text-[var(--color-text-muted)] font-medium">Unit</th>
									<th class="px-4 py-2.5 text-right text-[var(--color-text-muted)] font-medium">Combat</th>
									<th class="px-4 py-2.5 text-right text-[var(--color-text-muted)] font-medium">Shots</th>
									<th class="px-4 py-2.5 text-center text-[var(--color-text-muted)] font-medium">Sustain</th>
								</tr>
							</thead>
							<tbody>
								{#each results.defenderUnitStats as unit}
									<tr class="border-b border-[var(--color-border)]/30 hover:bg-blue-500/5 transition-colors">
										<td class="px-4 py-2.5 font-medium">{unit.Name}</td>
										<td class="px-4 py-2.5 text-right font-mono text-amber-400">{unit.Unit_Combat_Value}</td>
										<td class="px-4 py-2.5 text-right font-mono">{unit.Shots}</td>
										<td class="px-4 py-2.5 text-center">
											{#if unit.Has_Sustain_Damage}
												<span class="inline-block w-4 h-4 rounded-full bg-yellow-500/80 text-[8px] font-bold text-black flex items-center justify-center">S</span>
											{:else}
												<span class="text-[var(--color-text-muted)]">—</span>
											{/if}
										</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>
				</div>
			{/if}
		</div>
	{/if}
</div>
