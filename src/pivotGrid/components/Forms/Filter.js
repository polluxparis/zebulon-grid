import React, { Component } from 'react';
import List from 'react-virtualized/dist/commonjs/List';
// import AutoSizer from 'react-virtualized/dist/commonjs/AutoSizer';
// import { ResizableBox } from 'react-resizable';
import { isNull } from '../../utils/generic';
// const CheckboxFilter = ({ value, onChange, style }) =>
// 	<div style={{ ...style, textAlign: 'left' }}>
// 		<input
// 			type="text"
// 			id="filter"
// 			placeholder="Filter boxes"
// 			value={value.text}
// 		/>
// 	</div>;
const Checkbox = ({ value, checked, onChange, style }) =>
	<div style={{ ...style, textAlign: 'left' }}>
		<label>
			<input
				type="checkbox"
				checked={checked}
				onChange={onChange}
				valueKey={value.key}
				rowIndex={value.index}
			/>
			{value.label}
		</label>
	</div>;
export class Filter extends Component {
	constructor(props) {
		super(props);
		this.state = {
			...props.dimensionValues(props.dimensionId),
			filterLabel: '',
			filteredMap: null
		};
	}
	componentWillUpdate = nextProps => {
		if (nextProps.dimensionValues !== this.props.dimensionValues) {
			this.setState(nextProps.dimensionValues(nextProps.dimensionId));
		}
	};
	handleChange = value => {
		console.log(this.props);
		const newValue = {
			...value,
			isNotFiltered: !value.checked,
			checked: !value.checked
		};
		const newValues = this.state.values.map(
			(val, index) => (index === value.index ? newValue : val)
		);
		this.setState({ values: newValues });
		// return newValue;
		// this.values[value.index].isNotFiltered = !value.isNotFiltered;
		// this.handleChange(value, index);
	};
	handleChangeAll = () => {
		const noFilter = !this.state.noFilter;
		const values = this.state.values.map(value => ({
			...value,
			isNotFiltered: noFilter,
			checked: noFilter
		}));
		this.setState({ values, noFilter });
	};
	filterOnLabel = e => {
		const label = e.target.value;
		const filteredMap = label ? [] : null;
		if (label) {
			this.state.values.map((value, index) => {
				if (value.label.toLowerCase().startsWith(label.toLowerCase())) {
					filteredMap.push(index);
				}
			});
		}
		this.setState({
			...this.state,
			filteredMap: filteredMap,
			filterLabel: label
		});
	};
	applyFilter = () => {
		const filterKeys = this.state.values
			.filter(value => value.isNotFiltered)
			.map(value => value.key);
		if (filterKeys.length === this.state.values.length) {
			this.props.deleteFilter(this.props.dimensionId);
		} else {
			this.props.setFilter(this.props.dimensionId, 'in', filterKeys);
		}
	};

	rowRenderer = props => {
		const style = { width: 200, height: 20 };
		const index = isNull(this.state.filteredMap)
			? props.index
			: this.state.filteredMap[props.index];
		let value = this.state.values[index];
		value = {
			...value,
			index: index,
			checked: value.isNotFiltered || false
		};

		return (
			<Checkbox
				value={value}
				style={style}
				checked={value.checked}
				onChange={() => this.handleChange(value)}
				width="inherit"
			/>
		);
	};
	render() {
		// console.log(this.checkedValues);
		const rowCount = isNull(this.state.filteredMap)
			? this.state.values.length
			: this.state.filteredMap.length;
		// <ResizableBox
		// 	style={{ border: 'solid .05em', width: 210, height: 310 }}
		// >
		// <AutoSizer>
		// {({ width, height }) =>
		//
		// 			/AutoSizer>
		// </ResizableBox>
		const width = 200;
		const rowHeight = 20;
		const height = Math.min((3 + rowCount) * rowHeight, 250);

		return (
			<div height={height} width={width}>
				<input
					type="text"
					id="filter"
					placeholder="Filter boxes"
					style={{ width: width - 5, height: rowHeight - 2 }}
					onChange={this.filterOnLabel}
				/>
				<Checkbox
					style={{
						border: 'solid .05em',
						width: width - 2,
						height: rowHeight
					}}
					value={{ key: null, label: 'All' }}
					checked={this.state.noFilter}
					onChange={this.handleChangeAll}
				/>
				<List
					height={height - 3 * rowHeight}
					width={width - 5}
					rowCount={rowCount}
					rowHeight={rowHeight}
					rowRenderer={this.rowRenderer}
					overscanRowCount={10}
					values={this.state.values}
					{...this.props}
				/>
				<button
					style={{
						textAlign: 'center',
						width: width - 5,
						height: rowHeight
					}}
					onClick={this.applyFilter}
				>
					Apply filter
				</button>
			</div>
		);
	}
}
