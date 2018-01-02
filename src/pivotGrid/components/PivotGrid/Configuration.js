import React, { Component } from "react";
import cx from "classnames";
import ZebulonTable from "zebulon-table";
import {
	metaDescriptions,
	metaFunctions,
	actionDescriptions
} from "./MetaDescriptions";
import { isNavigationKey } from "../../utils/generic";

export default class Configuration extends Component {
	constructor(props) {
		super(props);
		const functions = metaFunctions(props.configurationFunctions);
		this.state = {
			selectedTab: 0,
			data: props.data,
			meta: props.meta,
			functions,
			dimensions: props.configuration.dimensions.map(
				(dimension, index) => {
					dimension.index_ = index;
					return dimension;
				}
			),
			measures: props.configuration.measures.map((measure, index) => {
				measure.index_ = index;
				return measure;
			})
		};
		const meta = metaDescriptions(
			this.state.functions,
			this.state.meta,
			props
		);
		this.state.metaProperties = this.computeMeta(meta.properties);
		this.state.metaMeasures = this.computeMeta(meta.measures);
		this.state.metaDimensions = this.computeMeta(meta.dimensions);
		this.state.metaFunctions = this.computeMeta(meta.functions);
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
	handleKeyDown = e => {
		if (isNavigationKey(e)) {
			const tab = this.tabs[this.state.selectedTab].caption;
			if (this[tab] && this[tab].handleNavigationKeys) {
				return this[tab].handleNavigationKeys(e);
			}
		}
	};
	computeData = () => {
		let { data, meta, functions } = this.state;
		const params = this.props.params || {};
		functions = this.state.functions
			.filter(f => f.tp === "accessor")
			.reduce((acc, f) => {
				acc[f.id] = f.codeJS;
				return acc;
			}, {});
		const metaFormula = meta.filter(column => column.tp === "Computed");
		metaFormula.forEach(column => {
			column.f = eval("(row,params)=>" + functions[column.valueAccessor]);
			column.dataType = typeof column.f(data[0], params);
		});
		data.forEach(row =>
			metaFormula.forEach(
				column => (row[column.id] = column.f(row, params))
			)
		);
		this.setState({
			data,
			meta: this.computeMeta(meta)
		});
	};
	computeMeta = meta => {
		let position = 0;
		meta.forEach((column, index) => {
			const width = column.hidden ? 0 : column.width;
			column.position = position;
			column.index_ = index;
			position += width || 0;
			column.formatFunction =
				this.props.configurationFunctions.formats[column.format] ||
				(x => x);
		});
		return meta;
	};

	initTabs = props => {
		return [
			{
				caption: "Dataset",
				content: (
					<ZebulonTable
						key="dataset"
						id="dataset"
						visible={this.state.selectedTab === 0}
						data={this.state.data}
						meta={this.state.meta.filter(
							column => column.width || 0 !== 0
						)}
						sizes={{
							width: props.width,
							height: props.height - 30
						}}
						getActions={actionDescriptions}
						rowHeight={25}
						ref={ref => (this.Dataset = ref)}
					/>
				)
			},
			{
				caption: "Properties",
				content: (
					<ZebulonTable
						key="properties"
						id="properties"
						visible={this.state.selectedTab === 1}
						data={this.state.meta}
						meta={this.state.metaProperties}
						sizes={{
							width: props.width,
							height: props.height - 30
						}}
						rowHeight={25}
						onChange={this.onChangeProperties}
						getActions={actionDescriptions}
						callbacks={{ computeData: this.computeData }}
						ref={ref => (this.Properties = ref)}
					/>
				)
			},
			{
				caption: "Measures",
				content: (
					<ZebulonTable
						key="measures"
						id="measures"
						visible={this.state.selectedTab === 2}
						data={this.state.measures}
						meta={this.state.metaMeasures}
						sizes={{
							width: props.width,
							height: props.height - 30
						}}
						rowHeight={25}
						getActions={actionDescriptions}
						ref={ref => (this.Measures = ref)}
					/>
				)
			},
			{
				caption: "Dimensions",
				content: (
					<ZebulonTable
						key="dimensions"
						id="dimensions"
						visible={this.state.selectedTab === 3}
						data={this.state.dimensions}
						meta={this.state.metaDimensions}
						sizes={{
							width: props.width,
							height: props.height - 30
						}}
						rowHeight={25}
						getActions={actionDescriptions}
						ref={ref => (this.Dimensions = ref)}
					/>
				)
			},
			{
				caption: "Functions",
				content: (
					<ZebulonTable
						key="functions"
						id="functions"
						visible={this.state.selectedTab === 4}
						data={this.state.functions}
						meta={this.state.metaFunctions}
						sizes={{
							width: props.width,
							height: props.height - 30
						}}
						rowHeight={25}
						getActions={actionDescriptions}
						ref={ref => (this.Functions = ref)}
					/>
				)
			},
			{ caption: "Graph", content: null }
		];
	};

	componentDidMount() {
		this.props.getRef(this);
	}
	onChangeProperties = (e, row, column) => {
		if (column.id === "width" || column.id === "format") {
			this.setState({ meta: this.computeMeta(this.state.meta) });
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
		this.tabs = this.initTabs(this.props);
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
