import * as React from 'react';
import * as ReactDOM from 'react-dom';
// import {ResizableBox} from 'react-resizable';
import Dropdown from './orb.react.Dropdown';
import * as utils from '../orb.utils';
import * as filtering from '../orb.filtering';
import {FilterManager} from './FilterManager';

export default class FilterPanelComponent extends React.Component<any,any>{

	public pgridwidget = null;
	public values = null;
	public filterManager = null;

	constructor(props){
		super(props);
		this.pgridwidget = this.props.pivotTableComp.pgridwidget;
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
	    this.filterManager.init(ReactDOM.findDOMNode(this));
	}
	componentWillUnmount() {
		utils.removeEventListener(document, 'mousedown', this.onMouseDown);
		// utils.removeEventListener(document, 'wheel', this.onMouseWheel);
		utils.removeEventListener(window, 'resize', this.destroy);
	}
	render() {
		const checkboxes = [];

		this.filterManager.reactComp = this;
		this.filterManager.initialFilterObject = this.pgridwidget.pgrid.getFieldFilter(this.props.field);
		this.values = this.pgridwidget.pgrid.getFieldValues(this.props.field);

		function addCheckboxRow(value, text?) {
			return checkboxes.push(<tr key={value}>
				<td className="fltr-chkbox">
					<input type="checkbox" value={value} defaultChecked="checked"/>
				</td>
				<td className="fltr-val" title={text || value}>{text || value}</td>
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
		const style = this.props.pivotTableComp.fontStyle;

		const currentFilter = this.pgridwidget.pgrid.getFieldFilter(this.props.field);

		return(
		// <ResizableBox width={301} heigth={200}>
			<table className="fltr-scntnr" style={style}>
				<tbody>
					<tr>
						<td className="srchop-col">
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
						<td className="srchtyp-col" title="Enable/disable Regular expressions">.*</td>
						<td className="srchbox-col">
							<table style={{width: '100%'}}>
								<tbody>
									<tr>
										<td><input type="text" placeholder="search"/></td>
										<td><div className="srchclear-btn">x</div></td>
									</tr>
								</tbody>
							</table>
						</td>
					</tr>
					<tr>
						<td colSpan="3" className="fltr-vals-col">
							<table className="fltr-vals-tbl" ref="valuesTable">
							<tbody>
								{checkboxes}
							</tbody>
							</table>
						</td>
					</tr>
					<tr className="bottom-row">
						<td className="cnfrm-btn-col" colSpan="2">
							<input type="button" className={buttonClass} value="Ok" style={{ float: 'left' }}/>
							<input type="button" className={buttonClass} value="Cancel" style={{ float: 'left' }}/>
						</td>
						<td className="resize-col">
							<div></div>
						</td>
					</tr>
				</tbody>
			</table>
			// </ResizableBox>
		);
	}
};
