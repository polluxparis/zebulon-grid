const newAxis = (e, type, axis, caller) => {
	// console.log("Column menu 0", e, caller);
	const table = caller[type].table;
	const index = table.state.selectedRange.end.rows;
	let row, tabIndex;
	if (type === "dimensions") {
		tabIndex = 4;
		row = {
			id: e[axis].id,
			keyAccessor: e[axis].accessor || "row." + e[axis].id,
			format: e[axis].format,
			index_: table.getDataLength(),
			isLocal: true
		};
	} else if (type === "measures") {
		tabIndex = 3;
		row = {
			id: e[axis].id,
			aggregation: "sum",
			valueAccessor: e[axis].accessor || "row." + e[axis].id,
			format: e[axis].format,
			index_: table.getDataLength(),
			isLocal: true
		};
	}
	caller.setState({ selectedTab: tabIndex });
	table.newRow_(row, index);
};
export const configurationMenus = {
	dataset: caller => () => {
		return {
			"column-header-menu": [
				{
					code: "dimension",
					caption: "New dimension",
					type: "MenuItem",
					function: e => newAxis(e, "dimensions", "column", caller)
				},
				{
					code: "measure",
					caption: "New measure",
					type: "MenuItem",
					function: e => newAxis(e, "measures", "column", caller)
				}
			]
		};
	},
	properties: caller => (state, props) => {
		return {
			"row-header-menu": [
				{
					code: "dimension",
					caption: "New dimension",
					type: "MenuItem",
					function: e => newAxis(e, "dimensions", "row", caller)
				},
				{
					code: "measure",
					caption: "New measure",
					type: "MenuItem",
					function: e => newAxis(e, "measures", "row", caller)
				}
			]
		};
	}
};
