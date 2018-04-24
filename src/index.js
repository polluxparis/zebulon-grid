import React from "react";
import ReactDOM from "react-dom";
import ZebulonGridDemo from "./demo/ZebulonGrid.demo";

ReactDOM.render(<ZebulonGridDemo />, document.getElementById("root"));

// import React, { Component } from "react";
// import ReactDOM from "react-dom";
// import ZebulonGrid from "./pivotGrid/ZebulonGrid";

// const buildData = (n0, n1, n2) => {
// 	let data = [];
// 	for (let i0 = 0; i0 < n0; i0++) {
// 		for (let i1 = 0; i1 < n1; i1++) {
// 			for (let i2 = 0; i2 < n2; i2++) {
// 				data.push({
// 					totoId: i0,
// 					totoLabel: `toto${i0}`,
// 					titi: i1,
// 					tutu: `tutu${i2}`,
// 					qty: Math.round(100 * Math.random()),
// 					amt: Math.round(1000 * Math.random())
// 				});
// 			}
// 		}
// 	}
// 	return data;
// };
// const buildConfiguration = () => ({
// 	measureHeadersAxis: "columns",
// 	width: 1000,
// 	height: 800,
// 	cellHeight: 30,
// 	cellWidth: 100,
// 	dimensions: [
// 		{
// 			id: "toto",
// 			caption: "Toto",
// 			keyAccessor: "totoId",
// 			labelAccessor: "totoLabel",
// 			sort: {
// 				keyAccessor: "totoLabel"
// 			}
// 		},
// 		{
// 			id: "titi",
// 			caption: "Titi",
// 			keyAccessor: "titi"
// 		},
// 		{
// 			id: "tutu",
// 			caption: "Tutu",
// 			keyAccessor: "tutu"
// 		}
// 	],
// 	measures: [
// 		{
// 			valueAccessor: "qty",
// 			id: "qty",
// 			caption: "Quantity",
// 			aggregation: "sum"
// 		},
// 		{
// 			valueAccessor: "amt",
// 			id: "amt",
// 			caption: "Amount",
// 			aggregation: "sum"
// 		}
// 	],
// 	columns: ["tutu"],
// 	rows: ["toto", "titi"],
// 	activeMeasures: ["qty", "amt"]
// });
// class MyPivotGrid extends Component {
// 	constructor(props) {
// 		super(props);
// 		this.state = {};
// 	}
// 	data = buildData(30, 12, 7);
// 	configuration = buildConfiguration();
// 	render() {
// 		return (
// 			<ZebulonGrid
// 				data={this.data}
// 				configuration={this.configuration}
// 				height={800}
// 				width={1000}
// 			/>
// 		);
// 	}
// }

// ReactDOM.render(<MyPivotGrid />, document.getElementById("root"));
