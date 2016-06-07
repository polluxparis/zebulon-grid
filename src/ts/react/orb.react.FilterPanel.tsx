import * as React from 'react';
import * as ReactDOM from 'react-dom';
import {VirtualScroll, AutoSizer} from 'react-virtualized';
import Dropdown from 'react-dropdown';
import {ResizableBox} from 'react-resizable';

import * as utils from '../orb.utils';
import * as filtering from '../orb.filtering';
import {FilterManager} from './FilterManager';

import {PivotTableComponent} from './orb.react.PivotTable';

export interface FilterPanelProps{
	field: string,
	pivotTableComp: PivotTableComponent
}

export default class FilterPanelComponent extends React.Component<FilterPanelProps,any>{

	public _pgridwidgetstore;
	public values = null;
	public filterManager: FilterManager;

	constructor(props){
		super(props);
		this._pgridwidgetstore = this.props.pivotTableComp.pgridwidgetstore;
		this.filterManager = new FilterManager(this, null);

		this.onFilter = this.onFilter.bind(this);
		this.onMouseDown = this.onMouseDown.bind(this);
		this.onMouseWheel = this.onMouseWheel.bind(this);
		this.filterManager.onOperatorChanged = this.filterManager.onOperatorChanged.bind(this.filterManager);
	}

	destroy() {
		const container = ReactDOM.findDOMNode(this).parentNode;
		ReactDOM.unmountComponentAtNode(container as Element);
		container.parentNode.removeChild(container);
	}

	onFilter(operator, term, staticValue, excludeStatic) {
		this.props.pivotTableComp.applyFilter(this.props.field, operator, term, staticValue, excludeStatic);
		this.destroy();
	}

	onMouseDown(e) {
    const container = ReactDOM.findDOMNode(this).parentNode;
		let target = e.target || e.srcElement;
		while(target != null) {
			if(target == container) {
				return true;
			}
			target = target.parentNode;
		}

		this.destroy();
	}

	onMouseWheel(e) {
		const valuesTable = this.refs['valuesTable'];
		let target = e.target || e.srcElement;
		while(target != null) {
			if(target == valuesTable) {
				if(valuesTable['scrollHeight'] <= valuesTable['clientHeight']) {
					utils.stopPropagation(e);
					utils.preventDefault(e);
				}
				return;
			}
			target = target.parentNode;
		}
		this.destroy();
	}

	componentWillMount() {
		utils.addEventListener(document, 'mousedown', this.onMouseDown);
		// utils.addEventListener(document, 'wheel', this.onMouseWheel);
		utils.addEventListener(window, 'resize', this.destroy);
	}

	componentDidMount() {
	    // this.filterManager.init(ReactDOM.findDOMNode(this));
	}

	componentWillUnmount() {
		utils.removeEventListener(document, 'mousedown', this.onMouseDown);
		// utils.removeEventListener(document, 'wheel', this.onMouseWheel);
		utils.removeEventListener(window, 'resize', this.destroy);
	}

	render() {
		this.filterManager.reactComp = this;
		this.filterManager.initialFilterObject = this._pgridwidgetstore.pgrid.getFieldFilter(this.props.field);
		this.values = this._pgridwidgetstore.pgrid.getFieldValues(this.props.field);
		const withAll = [filtering.ALL, ...this.values];

		const getCheckbox = (value, text?) => <label><input type="checkbox" value={value} defaultChecked="checked"/>{text || value}</label>

		const checkboxes =
			<AutoSizer>
				{({width, height})=>(
					<VirtualScroll
						width={width}
						height={height}
						rowCount={withAll.length}
						rowHeight={20}
						rowRenderer={
							({index, isScrolling}) => {
								if(withAll[index] != null) {
									if(index === 0){
										return getCheckbox(filtering.ALL, '(Show All)');
									}
									return getCheckbox(withAll[index]);
								} else {
									return getCheckbox(filtering.BLANK, '(Blank)');
								}
							}
						}
					/>)
				}
			</AutoSizer>;

		const buttonClass = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().orbButton;

		const currentFilter = this._pgridwidgetstore.pgrid.getFieldFilter(this.props.field);

		const divStyle = {
			backgroundColor:'white',
			fontSize:'90%',
			padding:'3px',
			border:'solid 1px',
			boxShadow:'0 5px 15px #9d9d9d',
			display: 'flex',
			flexDirection: 'column',
			justifyContent: 'space-between',
			height: '100%',
			width: '100%',
		}

		const inputStyle = {
			backgroundColor: 'white',
			border: '1px solid #ccc',
			borderRadius: '2px',
			boxSizing: 'border-box',
			color: '#333',
			cursor: 'default',
			outline: 'none',
			padding: '8px 10px 8px 10px',
		}


		return(
			<ResizableBox width={301} height={223} minConstraints={[301, 223]}>
				<div style={divStyle}>
							<div style={{display: 'flex', justifyContent: 'flex-start'}}>
									<Dropdown
										options={[
												filtering.Operators.MATCH.name,
												filtering.Operators.NOTMATCH.name,
												filtering.Operators.EQ.name,
												filtering.Operators.NEQ.name,
												filtering.Operators.GT.name,
												filtering.Operators.GTE.name,
												filtering.Operators.LT.name,
												filtering.Operators.LTE.name
											]}
											value={currentFilter && currentFilter.operator ? currentFilter.operator.name : filtering.Operators.MATCH.name}
											onChange={ this.filterManager.onOperatorChanged }>
									</Dropdown>
									<div className="srchtyp-col" title="Enable/disable Regular expressions" style={inputStyle}>.*</div>
									<div className="srchbox-col">
										<input type="text" placeholder="filter"/>
									</div>
							</div>
							<div colSpan="3" className="fltr-vals-col" style={{paddingBottom: '3px', flex: '1'}}>
									{checkboxes}
							</div>
							<div className="bottom-row">
								<div className="cnfrm-btn-col" colSpan="2" style={{paddingTop: '5px'}}>
									<input type="button" className={buttonClass} value="Ok" style={{ float: 'left' }}/>
									<input type="button" className={buttonClass} value="Cancel" style={{ float: 'left' }}/>
								</div>
							</div>
						</div>
			</ResizableBox>
		);
	}
};
