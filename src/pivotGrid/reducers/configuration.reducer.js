import { SET_CONFIG_PROPERTY, ZOOM } from "../constants";
export default (
	state = {
		totalsFirst: true,
		object: "dataset",
		features: {
			dimensions: "enabled",
			measures: "enabled",
			resize: "enabled",
			expandCollapse: "enabled",
			totals: "enabled",
			filters: "enabled",
			sorting: "enabled",
			configuration: "enabled"
		},
		edition: { editable: false },
		callbacks: {},
		height: 800,
		width: 1000,
		cellHeight: 25,
		cellWidth: 100,
		zoom: 1
	},
	action
) => {
	const { type, property, value, zoomValue } = action;
	switch (type) {
		case SET_CONFIG_PROPERTY:
			return { ...state, [property]: value };
		case ZOOM:
			return { ...state, zoom: zoomValue };
		default:
			return state;
	}
};
