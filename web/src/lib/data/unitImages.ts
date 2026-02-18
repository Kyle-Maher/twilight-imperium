// Unit type artwork from the Twilight Imperium 4 wiki
// Images sourced from: https://twilight-imperium.fandom.com/wiki/
export const unitTypeImages: Record<string, string> = {
	Fighter:     'https://static.wikia.nocookie.net/twilight-imperium-4/images/9/97/Fighter2.jpg/revision/latest?cb=20201012105924',
	Destroyer:   'https://static.wikia.nocookie.net/twilight-imperium-4/images/5/58/Destroyer2.jpg/revision/latest?cb=20201012105912',
	Cruiser:     'https://static.wikia.nocookie.net/twilight-imperium-4/images/2/29/Cruiser2.jpg/revision/latest?cb=20201012105906',
	Carrier:     'https://static.wikia.nocookie.net/twilight-imperium-4/images/b/b5/Carrier2.jpg/revision/latest?cb=20201012105900',
	Dreadnought: 'https://static.wikia.nocookie.net/twilight-imperium-4/images/1/10/Dreadnought2.jpg/revision/latest?cb=20201012105918',
	War_Sun:     'https://static.wikia.nocookie.net/twilight-imperium-4/images/e/ed/Warsun2.jpg/revision/latest?cb=20201012105957',
	Flagship:    'https://static.wikia.nocookie.net/twilight-imperium-4/images/8/8e/Flagship_Plastic.png/revision/latest?cb=20210622184307',
	Infantry:    'https://static.wikia.nocookie.net/twilight-imperium-4/images/1/16/Infantry2.jpg/revision/latest?cb=20201012105933',
	Mechs:       'https://static.wikia.nocookie.net/twilight-imperium-4/images/1/1c/Leader_&_Mech_Component.png/revision/latest?cb=20201107031020',
};

export function getUnitImage(unitType: string): string {
	return unitTypeImages[unitType] ?? unitTypeImages['Fighter'];
}
