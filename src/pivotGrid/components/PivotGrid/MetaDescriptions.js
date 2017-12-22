import React, { Component } from "react";
import * as aggregations from "../../utils/aggregation";
import { Input } from "../controls/Input";
class Property extends Component {
	constructor(props) {
		super(props);
		const object = this.props.row;
		const indexes = [8, 9, 10, 11, 12, 13];
		// const items = [],
		this.title = "Functions for analytic property";
		this.inputs = [];
		const style = {
			border: "solid lightgrey thin",
			boxSizing: "border-box",
			padding: 0,
			backgroundColor: "inherit",
			width: 200,
			textAlign: "left"
		};
		indexes.forEach(index => {
			const column = this.props.meta[index];
			// items.push(this.props.meta[index]);
			this.inputs.push(
				<Input
					style={style}
					label={column.caption}
					select={column.select}
					className="zebulon-input-label"
					row={props.row}
					id={column.id}
					key={column.id}
					value={this.props.row[column.id]}
					onChange={e => props.onChange(e, this.props.row, column)}
				/>
			);
		});
	}
	render() {
		// onst select = ["", "a", "b", "c"];
		// const style = {
		// 	border: "solid lightgrey thin",
		// 	boxSizing: "border-box",
		// 	padding: 0,
		// 	backgroundColor: "inherit",
		// 	width: 200,
		// 	textAlign: "left"

		// };
		return (
			<div
				style={{
					position: "absolute",
					border: "solid 0.1em rgba(0, 0, 0, 0.5)",
					backgroundColor: "white",
					top: this.props.top,
					left: 25,
					zIndex: 3,
					opacity: 1,
					width: 400,
					height: "fit-content",
					autoFocus: true
				}}
			>
				<div
					style={{
						textAlign: "Center",
						fontWeight: "bold",
						margin: 3,
						marginBottom: 7,
						display: "flex",
						justifyContent: "space-between"
					}}
				>
					{this.title}
					<button onClick={this.props.close}>X</button>
				</div>
				{this.inputs}
			</div>
		);
	}
}

const functionToString = f => {
	let fs = String(f);
	// fs = fs.slice(0, fs.length - 1);
	return fs.slice(fs.indexOf("{"));
};
export const metaFunctions = configurationFunctions => {
	const accessors = configurationFunctions.accessors;
	const sorts = configurationFunctions.sorts;
	const formats = configurationFunctions.formats;
	const aggs = { ...aggregations, ...configurationFunctions.aggregations };
	return Object.keys(accessors)
		.map(key => ({
			id: key,
			caption: key,
			tp: "accessor",
			codeJS: functionToString(accessors[key])
		}))
		.concat(
			Object.keys(aggs).map(key => ({
				id: key,
				caption: key,
				tp: "aggregation",
				codeJS: functionToString(aggs[key])
			}))
		)
		.concat(
			Object.keys(sorts).map(key => ({
				id: key,
				caption: key,
				tp: "sort",
				codeJS: functionToString(sorts[key])
			}))
		)
		.concat(
			Object.keys(formats).map(key => ({
				id: key,
				caption: key,
				tp: "format",
				codeJS: functionToString(formats[key])
			}))
		)
		.map((f, index) => {
			f.index_ = index;
			return f;
		});
	// const formats = props.configurationFunctions.formats.map(f=>format => {functionToString(});
	// const aggregations = props.configurationFunctions.formats.map(
	// 	aggregation => {}
	// );
};

export const metaDescriptions = (functions, properties, props) => {
	const accessors = props.configurationFunctions.accessors;

	const getFunctions = type =>
		[""].concat(
			functions.filter(f => f.tp === type).map(f => f.caption || f.id)
		);
	const getAccessors = () => getFunctions("accessor");
	const getAccessorsAndProperties = () =>
		getAccessors().concat(
			properties.map(property => property.caption || property.id)
		);
	const getAggregations = () => getFunctions("aggregation");
	const getFormats = () => getFunctions("format");
	const getSorts = () => getFunctions("sort");
	const getWindowFunctions = () => getFunctions("window");
	// const availableAccessors = [""].concat(
	// 	Object.keys(accessors).concat(props.meta.map(column => column.id))
	// );
	return {
		properties: [
			{
				id: "id",
				caption: "Column",
				width: 100,
				dataType: "string",
				editable: row => "Initial" !== row.tp
			},
			{
				id: "tp",
				caption: "Type",
				width: 100,
				dataType: "string",
				// accessor: (row, status) => (status.new_ ? row.tp : row.tp),
				editable: row => "Initial" !== row.tp,
				select: ["", "Computed", "Analytic"]
			},
			{
				id: "dataType",
				caption: "Data type",
				width: 100,
				dataType: "string",
				// accessor: (row, status) => (status.new_ ? row.tp : row.tp),
				editable: true, //(row, status) => status.new_,
				select: ["", "number", "string", "text", "date", "boolean"]
			},
			{
				id: "caption",
				caption: "Caption",
				width: 150,
				dataType: "string",
				editable: true
			},
			{
				id: "width",
				caption: "Width",
				width: 60,
				dataType: "number",
				editable: true
				// ,
				// onChange: row => this.computeMetaProperties()
			},
			{
				id: "format",
				caption: "Format",
				width: 100,
				dataType: "string",
				editable: true,
				select: getFormats
			},
			{
				id: "filterType",
				caption: "Filter",
				width: 100,
				dataType: "string",
				editable: true,
				select: ["", "starts", "=", ">=", "<=", "between", "values"]
			},
			{
				id: "valueAccessor",
				caption: "Value accessor",
				width: 150,
				dataType: "string",
				editable: row => "Initial" !== row.tp,
				select: getAccessors
			},
			{
				id: "groupByAccessor",
				caption: "Group by accessor",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getAccessorsAndProperties
			},
			{
				id: "comparisonAccessor",
				caption: "Comparison accessor",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getAccessorsAndProperties
			},
			{
				id: "sortAccessor",
				caption: "Sort accessor",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getAccessorsAndProperties
			},
			{
				id: "aggregation",
				caption: "Aggregation",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getAggregations
			},
			{
				id: "startFunction",
				caption: "Start function",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getWindowFunctions
			},
			{
				id: "endFunction",
				caption: "End function",
				width: 200,
				dataType: "string",
				editable: true,
				hidden: true,
				select: getWindowFunctions
			}
		],
		measures: [
			{
				id: "id",
				caption: "Id",
				width: 100,
				dataType: "string",
				editable: (row, status) => status.new_ && "Initial" !== row.tp
			},
			{
				id: "caption",
				caption: "Caption",
				width: 160,
				dataType: "string",
				editable: true
			},
			{
				id: "valueAccessor",
				caption: "Value accessor",
				width: 160,
				dataType: "string",
				editable: true,
				select: getAccessorsAndProperties
			},
			{
				id: "aggregation",
				caption: "Aggregation function",
				width: 160,
				dataType: "string",
				editable: true,
				select: getAggregations
			},
			{
				id: "format",
				caption: "Format",
				width: 160,
				dataType: "string",
				editable: true,
				select: getFormats
			}
		],
		dimensions: [
			{
				id: "id",
				caption: "Code",
				width: 100,
				dataType: "string",
				editable: (row, status) => status.new_ && "Initial" !== row.tp
			},
			{
				id: "caption",
				caption: "Caption",
				width: 150,
				dataType: "string",
				editable: true
			},
			{
				id: "keyAccessor",
				caption: "Key accessor",
				width: 150,
				accessor: row => accessors[row.keyAccessor] || row.keyAccessor,
				dataType: "string",
				editable: true,
				select: getAccessorsAndProperties
			},
			{
				id: "Label Accessor",
				caption: "Label accessor",
				width: 150,
				accessor: row =>
					accessors[row.labelAccessor] ||
					row.labelAccessor ||
					accessors[row.keyAccessor] ||
					row.keyAccessor,
				dataType: "string",
				editable: true,
				select: getAccessorsAndProperties
			},
			{
				id: "sortAccessor",
				caption: "Sorting accessor",
				accessor: row =>
					(row.sort
						? accessors[row.sort.keyAccessor] ||
							row.sort.keyAccessor
						: null) ||
					accessors[row.labelAccessor] ||
					row.labelAccessor ||
					accessors[row.keyAccessor] ||
					row.keyAccessor,
				width: 150,
				dataType: "string",
				editable: true,
				select: getAccessorsAndProperties
				// ,
				// onChange: row => this.computeMetaProperties()
			},
			{
				id: "sort",
				caption: "Sort function",
				width: 150,
				dataType: "string",
				editable: true,
				accessor: row => (row.sort ? row.sort.custom : undefined),
				select: getSorts
			},
			{
				id: "format",
				caption: "Format",
				width: 160,
				dataType: "string",
				editable: true,
				select: getFormats
			}
		],
		functions: [
			{
				id: "id",
				caption: "Code",
				width: 100,
				dataType: "string",
				editable: (row, status) => status.new_ && "Initial" !== row.tp
			},
			{
				id: "caption",
				caption: "Caption",
				width: 150,
				dataType: "string",
				editable: true
			},
			{
				id: "tp",
				caption: "Type",
				width: 150,
				dataType: "string",
				editable: true,
				select: [
					"",
					"accessor",
					"aggregation",
					"format",
					"sort",
					"window"
				]
			},
			{
				id: "codeJS",
				caption: "Function",
				width: 500,
				dataType: "text",
				editable: true
			}
		]
	};
};
export const actionDescriptions = (object, callbacks) => {
	if (object === "dataset") {
		return [];
	} else if (object === "properties") {
		return [
			{ caption: "New", type: "new" },
			{
				caption: "Delete",
				type: "delete",
				disable: row => row.index_ === undefined || "Initial" === row.tp
			},
			{
				caption: "Compute",
				action: e => callbacks.computeData()
			},
			{ caption: "Save", type: "save" },
			{
				caption: "Analytic",
				type: "detail",
				disable: row => "Analytic" !== row.tp,
				content: (row, data, meta, status, params, top, left) => {
					return (
						<Property
							row={row}
							data={data}
							meta={meta}
							params={params}
							status={status}
							onChange={callbacks.onChange}
							close={callbacks.close}
							top={top}
						/>
					);
				}
			}
		];
	} else {
		return [
			{ caption: "New", type: "new" },
			{
				caption: "Delete",
				type: "delete",
				disable: row => row.index_ === undefined || "Initial" === row.tp
			}
		];
	}
};
