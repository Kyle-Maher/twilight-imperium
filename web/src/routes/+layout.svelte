<script lang="ts">
	import '../app.css';

	let { children } = $props();
</script>

<div class="min-h-screen flex flex-col">
	<nav class="border-b border-[var(--color-border)] bg-[var(--color-surface)]">
		<div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
			<div class="flex items-center justify-between h-16">
				<a href="/" class="flex items-center gap-3 no-underline">
					<span class="text-2xl font-extrabold tracking-wider text-[var(--color-accent)]" style="font-family: 'Orbitron', sans-serif;">TIBS</span>
					<span class="hidden sm:inline text-sm text-[var(--color-text-muted)]">Twilight Imperium Battle Simulator</span>
				</a>
				<div class="flex gap-6">
					<a href="/" class="text-sm font-medium text-[var(--color-text-muted)] hover:text-[var(--color-text)] transition-colors no-underline">Simulator</a>
					<a href="/wiki" class="text-sm font-medium text-[var(--color-text-muted)] hover:text-[var(--color-text)] transition-colors no-underline">Wiki</a>
				</div>
			</div>
		</div>
	</nav>

	<main class="flex-1">
		{@render children()}
	</main>

	<footer class="border-t border-[var(--color-border)] py-4 text-center text-xs text-[var(--color-text-muted)]">
		Created by Kyle Maher &middot; Twilight Imperium 4
	</footer>
</div>
