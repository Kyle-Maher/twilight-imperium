<script lang="ts">
	import '../app.css';
	import { base } from '$app/paths';

	let { children } = $props();
</script>

<div class="min-h-screen flex flex-col relative">
	<!-- Nav -->
	<nav class="relative z-20 border-b border-[var(--color-border)] backdrop-blur-sm"
		 style="background: rgba(6, 8, 15, 0.85);">
		<div class="max-w-[1600px] mx-auto px-4 sm:px-6 lg:px-8">
			<div class="flex items-center justify-between h-14">
				<a href="{base}/" class="flex items-center gap-3 no-underline group">
					<div class="flex items-center gap-2">
						<!-- Logo mark -->
						<div class="w-7 h-7 rounded-lg flex items-center justify-center"
							 style="background: linear-gradient(135deg, #f59e0b, #d97706); box-shadow: 0 0 10px rgba(245,158,11,0.4);">
							<span class="text-xs font-black text-[#0f0a00]">TI</span>
						</div>
						<span class="text-base font-extrabold tracking-widest font-orbitron"
							  style="color: var(--color-accent);">TIBS</span>
					</div>
					<span class="hidden sm:inline text-xs text-[var(--color-text-muted)] tracking-wider border-l border-[var(--color-border)] pl-3">
						Twilight Imperium Battle Simulator
					</span>
				</a>
				<div class="flex gap-1">
					<a href="{base}/"
					   class="px-3 py-1.5 rounded-lg text-xs font-medium text-[var(--color-text-muted)] hover:text-[var(--color-text)] hover:bg-[var(--color-surface-hover)] transition-all no-underline tracking-wider uppercase">
						Simulator
					</a>
					<a href="{base}/wiki"
					   class="px-3 py-1.5 rounded-lg text-xs font-medium text-[var(--color-text-muted)] hover:text-[var(--color-text)] hover:bg-[var(--color-surface-hover)] transition-all no-underline tracking-wider uppercase">
						Wiki
					</a>
				</div>
			</div>
		</div>
	</nav>

	<main class="flex-1 relative z-10">
		{@render children()}
	</main>

	<footer class="relative z-10 border-t border-[var(--color-border)] py-3 text-center text-xs text-[var(--color-text-muted)]"
			style="background: rgba(6, 8, 15, 0.85);">
		Created by Kyle Maher &middot; Twilight Imperium 4 &middot; Not affiliated with Fantasy Flight Games
	</footer>
</div>
