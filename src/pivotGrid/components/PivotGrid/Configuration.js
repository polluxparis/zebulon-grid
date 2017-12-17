import React, { Component } from "react";
import cx from "classnames";
import { Table } from "../controls/Table";

export default class Configuration extends Component {
	constructor(props) {
		super(props);
		this.state = { selectedTab: 0, data: props.data, meta: props.meta };
		this.metaProperties = [
			{
				id: "id",
				caption: "Column",
				width: 100,
				dataType: "string",
				editable: (row, status) => status.new_ && "Initial" !== row.tp
			},
			{
				id: "tp",
				caption: "Type",
				width: 100,
				dataType: "string",
				// accessor: (row, status) => (status.new_ ? row.tp : row.tp),
				editable: (row, status) => status.new_,
				select: ["", "Computed", "Analytics"]
			},
			{
				id: "dataType",
				caption: "Data type",
				width: 100,
				dataType: "string",
				// accessor: (row, status) => (status.new_ ? row.tp : row.tp),
				editable: (row, status) => status.new_,
				select: ["", "number", "string", "date", "boolean"]
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
			},
			{
				id: "format",
				caption: "Format",
				width: 200,
				dataType: "string",
				editable: true
			},
			{
				id: "filterType",
				caption: "Filter",
				width: 100,
				dataType: "string",
				editable: (row, status) => status.new_,
				select: ["", "dafault", "interval", "list", "values"]
			},
			{
				id: "formula",
				caption: "Formula",
				width: 500,
				dataType: "string",
				editable: row => "Initial" !== row.tp
			}
		];
		this.actionsProperties = [
			{ caption: "Save", type: "save" },
			{
				caption: "Compute",
				action: e => this.computeData(this.props.data, e.data)
			},
			{ caption: "New", type: "new" },
			{
				caption: "Delete",
				type: "delete",
				disable: row => row.index_ === undefined || "Initial" === row.tp
			}
		];
		// this.init(props);
	}
	componentWillReceiveProps(nextProps) {
		if (
			nextProps.data !== this.props.data ||
			nextProps.meta !== this.props.meta
		) {
			this.setState({
				data: nextProps.data,
				meta: nextProps.meta
			});
		}
	}
	// componentWillReceiveProps(newProps) {
	// 	this.init(newProps);
	// }
	handleKeyDown = e => {
		const tab = this.tabs[this.state.selectedTab].caption;
		if (this[tab] && this[tab].handleNavigationKeys) {
			return this[tab].handleNavigationKeys(e);
		}
	};
	computeData = (data, meta) => {
		const metaFormula = meta.filter(column => column.tp === "Computed");
		metaFormula.forEach(column => {
			column.f = eval("(row)=>" + column.formula);
			column.dataType = typeof column.f(data[0]);
		});
		data.forEach(row =>
			metaFormula.forEach(column => (row[column.id] = column.f(row)))
		);
		this.metaPositions(meta);
		this.setState({ data, meta });
	};
	metaPositions = meta => {
		let position = 0;
		meta.forEach((column, index) => {
			column.position = position;
			column.index_ = index;
			position += column.width;
		});
	};
	init = props => {
		let position = 0;
		this.metaPositions(this.metaProperties);
		// this.metaProperties.forEach(column => {
		// 	column.position = position;
		// 	position += column.width;
		// });
		this.tabs = [
			{
				caption: "Dataset",
				content: (
					<Table
						key="dataset"
						visible={this.state.selectedTab === 0}
						data={this.state.data}
						meta={this.state.meta}
						width={props.width}
						height={props.height - 30}
						ref={ref => (this.Dataset = ref)}
					/>
				)
			},
			{
				caption: "Properties",
				content: (
					<Table
						key="properties"
						visible={this.state.selectedTab === 1}
						data={this.state.meta}
						meta={this.metaProperties}
						width={props.width}
						height={props.height - 30}
						onChange={this.onChangeProperties}
						actions={this.actionsProperties}
						ref={ref => (this.Properties = ref)}
					/>
				)
			},
			{ caption: "Measures", content: <div key="measures">tata</div> },
			{
				caption: "Dimensions",
				content: <div key="dimensions">titi</div>
			},
			{ caption: "Graph", content: <div key="graph">tyty</div> }
		];
	};
	componentDidMount() {
		this.props.getRef(this);
	}
	onChangeProperties = (e, row, column) => {
		if (column.id === "width") {
			this.metaPositions(this.props.meta);
		}
	};
	render() {
		if (this.props.status.loading || this.props.status.loadingConfig) {
			return <div>Loading data...</div>;
		} else if (this.props.status.error) {
			if (this.props.status.error.message === "No rows retrieved") {
				return (
					<div style={{ width: "max-content" }}>
						No rows retrieved
					</div>
				);
			} else {
				return (
					<div style={{ color: "red", width: "max-content" }}>
						<p>{this.props.status.error.type}</p>
						<p>{this.props.status.error.message}</p>
					</div>
				);
			}
		}
		this.init(this.props);
		return (
			<div>
				<div
					style={{ display: "flex", height: 25 }}
					className="zebulon-tabs-list"
				>
					{this.tabs.map((tab, index) => (
						<div
							key={index}
							className={cx({
								"zebulon-tabs-tab": true,
								"zebulon-tabs-tab-selected":
									index === this.state.selectedTab
							})}
							onClick={() =>
								this.setState({ selectedTab: index })}
						>
							{tab.caption}
						</div>
					))}
				</div>
				<div style={{ height: this.props.height - 30 }}>
					{this.tabs.map(tab => tab.content)}
				</div>
			</div>
		);
	}
}
