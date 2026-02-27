import { simulateBattles } from './simulate';

self.onmessage = (e: MessageEvent) => {
	const { attackerUnits, defenderUnits, rounds } = e.data;
	try {
		const data = simulateBattles(attackerUnits, defenderUnits, rounds);
		self.postMessage({ type: 'result', data });
	} catch (err) {
		self.postMessage({ type: 'error', message: err instanceof Error ? err.message : 'Simulation failed.' });
	}
};
