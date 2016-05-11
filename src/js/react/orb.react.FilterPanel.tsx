import * as React from 'react';
import * as ReactDOM from 'react-dom';
// import {ResizableBox} from 'react-resizable';
import Dropdown from './orb.react.Dropdown';
import * as utils from '../orb.utils';
import * as filtering from '../orb.filtering';
import {FilterManager} from './FilterManager';

import {PivotTableComponent} from './orb.react.PivotTable';

interface Props{
	field: string,
	pivotTableComp: PivotTableComponent
}

export default class FilterPanelComponent extends React.Component<Props,any>{

	public pgridwidgetstore = null;
	public values = null;
	public filterManager = null;

	constructor(props){
		super(props);
		this.pgridwidgetstore = this.props.pivotTableComp.pgridwidgetstore;
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
		console.log('onMouseDown in FilterPanel');
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
		console.log('onMouseWheel in FilterPanel');
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
		console.log('componentWillMount');
		utils.addEventListener(document, 'mousedown', this.onMouseDown);
		// utils.addEventListener(document, 'wheel', this.onMouseWheel);
		utils.addEventListener(window, 'resize', this.destroy);
	}
	componentDidMount() {
		console.log('componentDidMount');
	    this.filterManager.init(ReactDOM.findDOMNode(this));
	}
	componentWillUnmount() {
		console.log('componentWillUnmount');
		utils.removeEventListener(document, 'mousedown', this.onMouseDown);
		// utils.removeEventListener(document, 'wheel', this.onMouseWheel);
		utils.removeEventListener(window, 'resize', this.destroy);
	}
	render() {
		console.log('render');
		const checkboxes = [];

		this.filterManager.reactComp = this;
		this.filterManager.initialFilterObject = this.pgridwidgetstore.pgrid.getFieldFilter(this.props.field);
		this.values = this.pgridwidgetstore.pgrid.getFieldValues(this.props.field);

		function addCheckboxRow(value, text?) {
			const style = {
			  whiteSpace: 'nowrap',
			  textOverflow: 'ellipsis',
			  overflow: 'hidden'
			}
			return checkboxes.push(<tr key={value}>
				<td className="fltr-chkbox" style={{width: 16}}>
					<input type="checkbox" value={value} defaultChecked="checked"/>
				</td>
				<td className="fltr-val" title={text || value} style={style}>{text || value}</td>
				</tr>);
		}

		addCheckboxRow(filtering.ALL, '(Show All)');

		for(let i = 0; i < this.values.length; i++) {
			if(this.values[i] != null) {
				addCheckboxRow(this.values[i]);
			} else {
				addCheckboxRow(filtering.BLANK, '(Blank)');
			}
		}

		const buttonClass = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().orbButton;
		// const style = this.props.pivotTableComp.fontStyle;

		const style={
		  width: '100%',
		  tableLayout: 'fixed',
		  borderCollapse: 'separate',
		  borderSpacing: 2
		}

		const currentFilter = this.pgridwidgetstore.pgrid.getFieldFilter(this.props.field);

		return(
		// <ResizableBox width={301} heigth={200}>
			<table className="fltr-scntnr" style={style}>
				<tbody>
					<tr>
						<td className="srchop-col" style={{width: '105px', verticalAlign: 'middle'}}>
							<Dropdown values={[
										filtering.Operators.MATCH.name,
										filtering.Operators.NOTMATCH.name,
										filtering.Operators.EQ.name,
										filtering.Operators.NEQ.name,
										filtering.Operators.GT.name,
										filtering.Operators.GTE.name,
										filtering.Operators.LT.name,
										filtering.Operators.LTE.name
								]} selectedValue={currentFilter && currentFilter.operator ? currentFilter.operator.name : filtering.Operators.MATCH.name} onValueChanged={ this.filterManager.onOperatorChanged }>
							</Dropdown>
						</td>
						<td className="srchtyp-col" title="Enable/disable Regular expressions"
							style={{width: '18px', textAlign: 'center', fontWeight: 'bold', cursor: 'pointer'}}>.*</td>
						<td className="srchbox-col">
							<table style={{width: '100%'}}>
								<tbody>
									<tr>
										<td><input type="text" placeholder="search" style={{  width: '100%', border: 'none'}}/></td>
										<td><div className="srchclear-btn"
															style={{width: '14px', textAlign: 'center', fontWeight: 'bold', cursor: 'pointer', float: 'right'}}>x</div></td>
									</tr>
								</tbody>
							</table>
						</td>
					</tr>
					<tr>
						<td colSpan="3" className="fltr-vals-col" style={{  verticalAlign: 'top', paddingBottom: '3px'}}>
							<table className="fltr-vals-tbl" ref="valuesTable" style={{  tableLayout: 'fixed', width: '100%', display: 'block'}}>
							<tbody style={{  float: 'left', overflow: 'auto', width: '100%', height: '154px' }}>
								{checkboxes}
							</tbody>
							</table>
						</td>
					</tr>
					<tr className="bottom-row">
						<td className="cnfrm-btn-col" colSpan="2" style={{paddingTop: '5px'}}>
							<input type="button" className={buttonClass} value="Ok" style={{ float: 'left' }}/>
							<input type="button" className={buttonClass} value="Cancel" style={{ float: 'left' }}/>
						</td>
						<td className="resize-col" style={{verticalAlign: 'bottom'}}>
							<div style={{float: 'right',
						  width: '16px',
						  height: '16px',
						  marginBottom: '0',
						  background: 'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAQklEQVQ4jWNgGJngxo0b/2GYIgMOHz5MvgGHDx8m3wD6AmwBRlIgYgswkgIRW4AN4kAkNsBwBiKxAYYzEIkNMGQxAOs9ug3E3qdjAAAAAElFTkSuQmCC) no-repeat 0px 0px',
						  cursor: 'se-resize'}}></div>
						</td>
					</tr>
				</tbody>
			</table>
			// </ResizableBox>
		);
	}
};
