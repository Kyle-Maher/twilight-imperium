# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Twilight Imperium Battle Simulator (TIBS) — a Monte Carlo battle simulator for the board game Twilight Imperium 4. The active codebase is the **web app** in `web/`. Legacy Python/R code still exists in `src/`, `TwilightImperiumBattleSimulator/`, and `testing/` but is no longer the primary focus.

## Web App Commands

All commands run from the `web/` directory:

```bash
cd web
npm install        # install dependencies
npm run dev        # start dev server (http://localhost:5173)
npm run build      # production build → web/build/
npm run preview    # preview production build
npm run check      # TypeScript + Svelte type checking
npm run check:watch
```

## Deployment

The app is deployed to GitHub Pages at `/twilight-imperium`. The `svelte.config.js` sets `paths.base` to `/twilight-imperium` in production and `''` in dev. All internal links must use the `$app/paths` `base` import (already done in `+layout.svelte`). CI/CD via `.github/workflows/deploy.yml` builds and deploys on every push to `main`.

## Architecture

**SvelteKit static site** (adapter-static) + **Tailwind CSS v4** + **TypeScript** + **Chart.js**. Svelte 5 runes syntax (`$state`, `$props`, `$derived`) is used throughout — not the legacy Svelte 4 store/reactive syntax.

### Key files

- [web/src/lib/simulation/simulate.ts](web/src/lib/simulation/simulate.ts) — core simulation engine. `simulateBattles(attackerUnits, defenderUnits, rounds)` runs Monte Carlo combat returning `SimulationResults`. Combat sequence: Anti-Fighter Barrage → Space Combat → (if attacker wins) Bombardment → Ground Combat.
- [web/src/lib/data/units.ts](web/src/lib/data/units.ts) — all unit data as a large typed `Record<string, UnitData>`. Helper exports: `getCommonUnits()`, `getFactionUnits()`, `getAllUnitNames()`, `getFactions()`.
- [web/src/lib/components/UnitPanel.svelte](web/src/lib/components/UnitPanel.svelte) — unit selection UI for attacker/defender.
- [web/src/lib/components/ResultsDisplay.svelte](web/src/lib/components/ResultsDisplay.svelte) — displays win percentages and metadata.
- [web/src/lib/components/DonutChart.svelte](web/src/lib/components/DonutChart.svelte) — Chart.js donut chart wrapper.
- [web/src/routes/+page.svelte](web/src/routes/+page.svelte) — main simulator page; orchestrates state and calls `simulateBattles`.
- [web/src/routes/wiki/+page.svelte](web/src/routes/wiki/+page.svelte) — unit reference/wiki page.

### Simulation logic

Units are classified as ships (`Is_Ship`) or ground forces (`Is_Ground_Force`). Combat rolls use d10 — a roll ≥ `Combat_Value` is a hit. Hits are assigned prioritizing units without `Has_Sustain_Damage` first (sustain units absorb one hit before being destroyed). Ground combat only occurs if the attacker wins space combat.

### Styling

CSS custom properties define the color theme in [web/src/app.css](web/src/app.css): `--color-bg`, `--color-surface`, `--color-accent`, `--color-text`, `--color-text-muted`, `--color-border`, `--color-surface-hover`. Use these variables rather than hardcoded colors.
