<script lang="ts">
	import { onMount } from 'svelte';
	import { Chart, DoughnutController, ArcElement, Tooltip, Legend } from 'chart.js';

	Chart.register(DoughnutController, ArcElement, Tooltip, Legend);

	interface Props {
		title: string;
		attacker: number;
		defender: number;
		draw: number;
	}

	let { title, attacker, defender, draw }: Props = $props();

	let canvas: HTMLCanvasElement;
	let chart: Chart | null = null;

	onMount(() => {
		createChart();
		return () => chart?.destroy();
	});

	function createChart() {
		if (chart) chart.destroy();

		chart = new Chart(canvas, {
			type: 'doughnut',
			data: {
				labels: ['Attacker', 'Defender', 'Draw'],
				datasets: [{
					data: [attacker, defender, draw],
					backgroundColor: ['#ef4444', '#3b82f6', '#a855f7'],
					borderColor: '#1e293b',
					borderWidth: 3,
					hoverBorderColor: '#f1f5f9'
				}]
			},
			options: {
				responsive: true,
				maintainAspectRatio: true,
				cutout: '60%',
				plugins: {
					legend: { display: false },
					tooltip: {
						callbacks: {
							label: (ctx) => `${ctx.label}: ${ctx.parsed}%`
						}
					}
				}
			}
		});
	}

	$effect(() => {
		// Reactively update when data changes
		if (chart) {
			chart.data.datasets[0].data = [attacker, defender, draw];
			chart.update();
		}
	});
</script>

<div class="flex flex-col items-center gap-2">
	<h4 class="text-sm font-semibold text-[var(--color-text-muted)] uppercase tracking-wide">{title}</h4>
	<div class="w-40 h-40 relative">
		<canvas bind:this={canvas}></canvas>
	</div>
	<div class="flex gap-4 text-xs">
		<span class="flex items-center gap-1">
			<span class="w-2.5 h-2.5 rounded-full bg-red-500"></span>
			{attacker}%
		</span>
		<span class="flex items-center gap-1">
			<span class="w-2.5 h-2.5 rounded-full bg-blue-500"></span>
			{defender}%
		</span>
		<span class="flex items-center gap-1">
			<span class="w-2.5 h-2.5 rounded-full bg-purple-500"></span>
			{draw}%
		</span>
	</div>
</div>
