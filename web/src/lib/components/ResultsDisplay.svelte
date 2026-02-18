<script lang="ts">
	import DonutChart from './DonutChart.svelte';
	import type { SimulationResults } from '$lib/simulation/simulate';

	interface Props {
		results: SimulationResults;
	}

	let { results }: Props = $props();

	const { winPercentages: wp, metadata: md } = $derived(results);
</script>

<div class="space-y-6">
	<!-- Donut Charts -->
	<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] p-6">
		<h3 class="text-lg font-bold text-[var(--color-text)] mb-4 text-center">Win Percentages</h3>
		<div class="grid grid-cols-1 sm:grid-cols-3 gap-6">
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
		<div class="flex justify-center gap-6 mt-4 text-sm">
			<span class="flex items-center gap-2"><span class="w-3 h-3 rounded-full bg-red-500"></span> Attacker</span>
			<span class="flex items-center gap-2"><span class="w-3 h-3 rounded-full bg-blue-500"></span> Defender</span>
			<span class="flex items-center gap-2"><span class="w-3 h-3 rounded-full bg-purple-500"></span> Draw</span>
		</div>
	</div>

	<!-- Metadata Table -->
	<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] overflow-hidden">
		<h3 class="text-lg font-bold text-[var(--color-text)] px-6 py-3 border-b border-[var(--color-border)]">Combat Statistics</h3>
		<div class="overflow-x-auto">
			<table class="w-full text-sm">
				<thead>
					<tr class="border-b border-[var(--color-border)]">
						<th class="px-4 py-3 text-left text-[var(--color-text-muted)] font-medium">Phase</th>
						<th class="px-4 py-3 text-right text-red-400 font-medium">Attacker Wins</th>
						<th class="px-4 py-3 text-right text-blue-400 font-medium">Defender Wins</th>
						<th class="px-4 py-3 text-right text-purple-400 font-medium">Draws</th>
						<th class="px-4 py-3 text-right text-[var(--color-text-muted)] font-medium">Avg Rounds</th>
						<th class="px-4 py-3 text-right text-[var(--color-text-muted)] font-medium">Simulated</th>
					</tr>
				</thead>
				<tbody>
					<tr class="border-b border-[var(--color-border)]/50">
						<td class="px-4 py-3 font-medium">Space</td>
						<td class="px-4 py-3 text-right text-red-400">{md.space.attackerWins}</td>
						<td class="px-4 py-3 text-right text-blue-400">{md.space.defenderWins}</td>
						<td class="px-4 py-3 text-right text-purple-400">{md.space.draws}</td>
						<td class="px-4 py-3 text-right">{md.space.averageRounds}</td>
						<td class="px-4 py-3 text-right">{md.space.combatsSimulated}</td>
					</tr>
					<tr>
						<td class="px-4 py-3 font-medium">Ground</td>
						<td class="px-4 py-3 text-right text-red-400">{md.ground.attackerWins}</td>
						<td class="px-4 py-3 text-right text-blue-400">{md.ground.defenderWins}</td>
						<td class="px-4 py-3 text-right text-purple-400">{md.ground.draws}</td>
						<td class="px-4 py-3 text-right">{md.ground.averageRounds}</td>
						<td class="px-4 py-3 text-right">{md.ground.combatsSimulated}</td>
					</tr>
				</tbody>
			</table>
		</div>
	</div>

	<!-- Unit Stats -->
	{#if results.attackerUnitStats.length > 0 || results.defenderUnitStats.length > 0}
		<div class="grid grid-cols-1 lg:grid-cols-2 gap-4">
			{#if results.attackerUnitStats.length > 0}
				<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] overflow-hidden">
					<h3 class="text-base font-bold text-red-400 px-4 py-3 border-b border-[var(--color-border)]">Attacker Units</h3>
					<div class="overflow-x-auto">
						<table class="w-full text-xs">
							<thead>
								<tr class="border-b border-[var(--color-border)]">
									<th class="px-3 py-2 text-left text-[var(--color-text-muted)]">Unit</th>
									<th class="px-3 py-2 text-left text-[var(--color-text-muted)]">Type</th>
									<th class="px-3 py-2 text-right text-[var(--color-text-muted)]">Combat</th>
									<th class="px-3 py-2 text-right text-[var(--color-text-muted)]">Shots</th>
									<th class="px-3 py-2 text-center text-[var(--color-text-muted)]">Sustain</th>
								</tr>
							</thead>
							<tbody>
								{#each results.attackerUnitStats as unit}
									<tr class="border-b border-[var(--color-border)]/30">
										<td class="px-3 py-2">{unit.Name}</td>
										<td class="px-3 py-2 text-[var(--color-text-muted)]">{unit.Unit_Type}</td>
										<td class="px-3 py-2 text-right">{unit.Unit_Combat_Value}</td>
										<td class="px-3 py-2 text-right">{unit.Shots}</td>
										<td class="px-3 py-2 text-center">{unit.Has_Sustain_Damage ? 'Yes' : '-'}</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>
				</div>
			{/if}

			{#if results.defenderUnitStats.length > 0}
				<div class="rounded-xl border border-[var(--color-border)] bg-[var(--color-surface)] overflow-hidden">
					<h3 class="text-base font-bold text-blue-400 px-4 py-3 border-b border-[var(--color-border)]">Defender Units</h3>
					<div class="overflow-x-auto">
						<table class="w-full text-xs">
							<thead>
								<tr class="border-b border-[var(--color-border)]">
									<th class="px-3 py-2 text-left text-[var(--color-text-muted)]">Unit</th>
									<th class="px-3 py-2 text-left text-[var(--color-text-muted)]">Type</th>
									<th class="px-3 py-2 text-right text-[var(--color-text-muted)]">Combat</th>
									<th class="px-3 py-2 text-right text-[var(--color-text-muted)]">Shots</th>
									<th class="px-3 py-2 text-center text-[var(--color-text-muted)]">Sustain</th>
								</tr>
							</thead>
							<tbody>
								{#each results.defenderUnitStats as unit}
									<tr class="border-b border-[var(--color-border)]/30">
										<td class="px-3 py-2">{unit.Name}</td>
										<td class="px-3 py-2 text-[var(--color-text-muted)]">{unit.Unit_Type}</td>
										<td class="px-3 py-2 text-right">{unit.Unit_Combat_Value}</td>
										<td class="px-3 py-2 text-right">{unit.Shots}</td>
										<td class="px-3 py-2 text-center">{unit.Has_Sustain_Damage ? 'Yes' : '-'}</td>
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
