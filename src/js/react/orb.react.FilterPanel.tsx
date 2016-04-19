import * as React from 'react';
import * as ReactDOM from 'react-dom';
import Dropdown from './orb.react.Dropdown';
import * as utils from '../orb.utils';
import * as filtering from '../orb.filtering';
import * as domUtils from '../orb.utils.dom';

export default class FilterPanelComponenent extends React.Component<any,any>{

	public pgridwidget = null;
	public values = null;
	public filterManager = null;

	constructor(props){
		super(props);
		this.pgridwidget = this.props.pivotTableComp.pgridwidget;
		this.filterManager = new FilterManager(this, null)
	}

	destroy() {
	    const container = ReactDOM.findDOMNode(this).parentNode;
	    ReactDOM.unmountComponentAtNode(container);
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
		const valuesTable = this.refs.valuesTable;
		let target = e.target || e.srcElement;
		while(target != null) {
			if(target == valuesTable) {
				if(valuesTable.scrollHeight <= valuesTable.clientHeight) {
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
		utils.addEventListener(document, 'wheel', this.onMouseWheel);
		utils.addEventListener(window, 'resize', this.destroy);
	}
	componentDidMount() {
	    this.filterManager.init(ReactDOM.findDOMNode(this));
	}
	componentWillUnmount() {
		utils.removeEventListener(document, 'mousedown', this.onMouseDown);
		utils.removeEventListener(document, 'wheel', this.onMouseWheel);
		utils.removeEventListener(window, 'resize', this.destroy);
	}
	render() {
		const checkboxes = [];

		this.filterManager = new FilterManager(this, this.pgridwidget.pgrid.getFieldFilter(this.props.field));
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

		return <table className="fltr-scntnr" style={style}>
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
								<td><div className="srchclear-btn" onClick={this.clearFilter}>x</div></td>
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
		</table>;
	}
});

class FilterManager{

	public INDETERMINATE = 'indeterminate';

	public savedCheckedValues;
	public isSearchMode = false;
	public isRegexMode = false;
	public operator = filtering.Operators.MATCH;
	public lastSearchTerm = '';

	public elems = {
		filterContainer: null,
		checkboxes: {},
		searchBox: null,
		operatorBox: null,
		allCheckbox: null,
		addCheckbox: null,
		enableRegexButton: null,
		clearSearchButton: null,
		okButton: null,
		cancelButton: null,
		resizeGrip: null,
		blankCheckbox: null
	};

	public resizeManager;
	public initialFilterObject;

	public reactComp;

	constructor(reactComp, initialFilterObject) {
		this.initialFilterObject = initialFilterObject;
		this.reactComp = this.reactComp;
	}

	init (filterContainerElement){

		this.elems.filterContainer = filterContainerElement;
		this.elems.checkboxes = {};
		this.elems.searchBox = this.elems.filterContainer.rows[0].cells[2].children[0].rows[0].cells[0].children[0];
		this.elems.clearSearchButton = this.elems.filterContainer.rows[0].cells[2].children[0].rows[0].cells[1].children[0];
		this.elems.operatorBox = this.elems.filterContainer.rows[0].cells[0].children[0];
		this.elems.okButton = this.elems.filterContainer.rows[2].cells[0].children[0];
		this.elems.cancelButton = this.elems.filterContainer.rows[2].cells[0].children[1];
		this.elems.resizeGrip = this.elems.filterContainer.rows[2].cells[1].children[0];

		const rows = this.elems.filterContainer.rows[1].cells[0].children[0].rows;
		for(let i = 0; i < rows.length; i++) {
			const checkbox = rows[i].cells[0].children[0];
			this.elems.checkboxes[checkbox.value] = checkbox;
		}

		this.elems.allCheckbox = this.elems.checkboxes[filtering.ALL];
		this.elems.blankCheckbox = this.elems.checkboxes[filtering.BLANK];
		this.elems.addCheckbox = null;
		this.elems.enableRegexButton = this.elems.filterContainer.rows[0].cells[1];

		this.resizeManager = new ResizeManager(this.elems.filterContainer.parentNode, this.elems.filterContainer.rows[1].cells[0].children[0], this.elems.resizeGrip);

		this.applyInitialFilterObject();
		this.addEventListeners();
	};

	onOperatorChanged (newOperator){
		if(this.operator.name !== newOperator) {
			this.operator = filtering.Operators.get(newOperator);
			this.toggleRegexpButtonVisibility();
			this.searchChanged('operatorChanged');
		}
	};

	checkboxVisible(checkbox, isVisible?) {
		if(isVisible != null) {
			checkbox.parentNode.parentNode.style.display = isVisible ? '' : 'none';
		} else {
			return checkbox.parentNode.parentNode.style.display != 'none';
		}
	}

	applyInitialFilterObject() {
		if(this.initialFilterObject) {
			const staticInfos = {
				values: this.initialFilterObject.staticValue,
				toExclude: this.initialFilterObject.excludeStatic
			};

			if(this.initialFilterObject.term) {
				this.isSearchMode = true;

				this.operator = this.initialFilterObject.operator;
				this.toggleRegexpButtonVisibility();

				if(this.initialFilterObject.regexpMode) {
					this.isRegexMode = true;
					this.toggleRegexpButtonState();
					this.lastSearchTerm = this.initialFilterObject.term.source;
				} else {
					this.lastSearchTerm = this.initialFilterObject.term;
				}

				this.elems.searchBox.value = this.lastSearchTerm;

				this.applyFilterTerm(this.initialFilterObject.operator, this.initialFilterObject.term);
			} else {
				this.savedCheckedValues = staticInfos;
			}

			this.updateCheckboxes(staticInfos);
			this.updateAllCheckbox();
		}
	}

	addEventListeners() {
		this.toggleRegexpButtonVisibility();

		utils.addEventListener(this.elems.filterContainer, 'click', this.valueChecked);
		utils.addEventListener(this.elems.searchBox, 'keyup', this.searchChanged);

		utils.addEventListener(this.elems.clearSearchButton, 'click', this.clearSearchBox);

		utils.addEventListener(this.elems.okButton, 'click', () => {
			const checkedObj = this.getCheckedValues();
			this.reactComp.onFilter(this.operator.name, this.operator.regexpSupported && this.isSearchMode && this.isRegexMode ? new RegExp(this.lastSearchTerm, 'i') : this.lastSearchTerm, checkedObj.values, checkedObj.toExclude);
		});
		utils.addEventListener(this.elems.cancelButton, 'click', () => { this.reactComp.destroy(); });
	}

	clearSearchBox () {
		this.elems.searchBox.value = '';
		this.searchChanged();
	};
	toggleRegexpButtonVisibility() {
		if(this.operator.regexpSupported) {
			utils.addEventListener(this.elems.enableRegexButton, 'click', this.regexpActiveChanged);
			domUtils.removeClass(this.elems.enableRegexButton, 'srchtyp-col-hidden');

		} else {
			utils.removeEventListener(this.elems.enableRegexButton, 'click', this.regexpActiveChanged);
			domUtils.addClass(this.elems.enableRegexButton, 'srchtyp-col-hidden');
		}
	};

	toggleRegexpButtonState() {
		this.elems.enableRegexButton.className = this.elems.enableRegexButton.className.replace('srchtyp-col-active', '');
		if(this.isRegexMode) {
			domUtils.addClass(this.elems.enableRegexButton, 'srchtyp-col-active');
		} else {
			domUtils.removeClass(this.elems.enableRegexButton, 'srchtyp-col-active');
		}
	};

	regexpActiveChanged() {
		this.isRegexMode = !this.isRegexMode;
		this.toggleRegexpButtonState();
		this.searchChanged('regexModeChanged');
	};

	valueChecked (e) {
		const target = e.target || e.srcElement;
		if(target && target.type && target.type === 'checkbox') {
			if(target == this.elems.allCheckbox) {
				this.updateCheckboxes({ values: this.elems.allCheckbox.checked });
			} else {
				this.updateAllCheckbox();
			}
		}
	};

	applyFilterTerm (operator, term) {
		const defaultVisible = term ? false : true;
		const opterm = operator.regexpSupported && this.isSearchMode ? (this.isRegexMode ? term : utils.escapeRegex(term)) : term;
		this.checkboxVisible(this.elems.allCheckbox, defaultVisible);
		for(let i = 0; i < this.reactComp.values.length; i++) {
			const val = this.reactComp.values[i];
			const checkbox = val != null ? this.elems.checkboxes[val] : this.elems.blankCheckbox;
			const visible = !this.isSearchMode || operator.func(val, opterm);
			this.checkboxVisible(checkbox, visible);
			checkbox.checked = visible;
		}
	};

	searchChanged(e?) {
		const search = (this.elems.searchBox.value || '').trim();
		if(e === 'operatorChanged' || (e === 'regexModeChanged' && search) || search != this.lastSearchTerm) {
			this.lastSearchTerm = search;

			const previousIsSearchMode = this.isSearchMode;
			this.isSearchMode = search !== '';

			if(this.isSearchMode && !previousIsSearchMode) {
				this.savedCheckedValues = this.getCheckedValues();
			}

			var searchTerm = this.operator.regexpSupported && this.isSearchMode ? new RegExp(this.isRegexMode ? search : utils.escapeRegex(search), 'i') : search;
			if(e !== 'operatorChanged' || this.isSearchMode) {
				this.applyFilterTerm(this.operator, search);
			}

			if(!this.isSearchMode && previousIsSearchMode) {
				this.updateCheckboxes(this.savedCheckedValues);
			}

			this.updateAllCheckbox();
		}
	};

	getCheckedValues() {
		if(!this.isSearchMode && !this.elems.allCheckbox.indeterminate) {
			return {
				values: this.elems.allCheckbox.checked ? filtering.ALL : filtering.NONE,
				toExclude: false
			};
		} else {
			let staticValue;
			let i, val, checkbox;
			let valuesCount = 0, checkedCount = 0;

			for(i = 0; i < this.reactComp.values.length; i++) {
				val = this.reactComp.values[i];
				checkbox = val != null ? this.elems.checkboxes[val] : this.elems.blankCheckbox;
				if(this.checkboxVisible(checkbox)) {
					valuesCount++;
					if(checkbox.checked) {
						checkedCount++;
					}
				}
			}

			let excludeUnchecked = false;

			if(checkedCount === 0) {
				staticValue = filtering.NONE;
			} else if(checkedCount == valuesCount) {
				staticValue = filtering.ALL;
			} else {
				staticValue = [];
				excludeUnchecked = checkedCount > (valuesCount/2 + 1);

				for(i = 0; i < this.reactComp.values.length; i++) {
					val = this.reactComp.values[i];
					checkbox = val != null ? this.elems.checkboxes[val] : this.elems.blankCheckbox;
					if(this.checkboxVisible(checkbox)) {
						if((!excludeUnchecked && checkbox.checked) || (excludeUnchecked && !checkbox.checked))  {
							staticValue.push(val);
						}
					}
				}
			}
			return {
				values: staticValue,
				toExclude: excludeUnchecked
			};
		}
	};

	updateCheckboxes(checkedList) {
		const values = checkedList ? checkedList.values : null;
		const allchecked = utils.isArray(values) ?
			null :
			(values == null || values === filtering.ALL ?
				true :
				(values === filtering.NONE ?
					false :
					!!values
				)
			);
		for(let i = 0; i < this.reactComp.values.length; i++) {
			const val = this.reactComp.values[i];
			const checkbox = val != null ? this.elems.checkboxes[val] : this.elems.blankCheckbox;
			if(this.checkboxVisible(checkbox)) {
				if(allchecked != null) {
					checkbox.checked = allchecked;
				} else {
					const valInList = values.indexOf(val) >= 0;
					checkbox.checked =  checkedList.toExclude ? !valInList : valInList;
				}
			}
		}
	};

	updateAllCheckbox() {
		if(!this.isSearchMode) {
			let allchecked = null;
			for(let i = 0; i < this.reactComp.values.length; i++) {
				const val = this.reactComp.values[i];
				const checkbox = val != null ? this.elems.checkboxes[val] : this.elems.blankCheckbox;
				if(allchecked == null) {
					allchecked = checkbox.checked;
				} else {
					if(allchecked !== checkbox.checked) {
						allchecked = this.INDETERMINATE;
						break;
					}
				}
			}

			if(allchecked === this.INDETERMINATE) {
				this.elems.allCheckbox.indeterminate = true;
				this.elems.allCheckbox.checked = false;
			} else {
				this.elems.allCheckbox.indeterminate = false;
				this.elems.allCheckbox.checked = allchecked;
			}
		}
	};
}

class ResizeManager {
	public minContainerWidth = 301;
	public minContainerHeight = 223;

	public mousedownpos = {
		x: 0, y: 0
	};
	public isMouseDown = false;

	public outerContainerElem;
	public  valuesTableElem;
	public resizeGripElem;


	constructor(outerContainerElem, valuesTableElem, resizeGripElem){
		this.outerContainerElem = outerContainerElem;
		this.valuesTableElem = valuesTableElem;
		this.resizeGripElem = resizeGripElem;

		utils.addEventListener(this.resizeGripElem, 'mousedown', this.resizeMouseDown);
		utils.addEventListener(document, 'mouseup', this.resizeMouseUp);
		utils.addEventListener(document, 'mousemove', this.resizeMouseMove);
	}


	resizeMouseDown(e) {
		// drag/sort with left mouse button
		if (utils.getEventButton(e) !== 0) return;

		const mousePageXY = utils.getMousePageXY(e);

		this.isMouseDown = true;
		document.body.style.cursor = 'se-resize';

		this.mousedownpos.x = mousePageXY.pageX;
		this.mousedownpos.y = mousePageXY.pageY;

		// prevent event bubbling (to prevent text selection while dragging for example)
		utils.stopPropagation(e);
		utils.preventDefault(e);
	};

	resizeMouseUp() {
		this.isMouseDown = false;
		document.body.style.cursor = 'auto';
		return true;
	};

	resizeMouseMove(e) {
		// if the mouse is not down while moving, return (no drag)
		if (!this.isMouseDown) return;

		const mousePageXY = utils.getMousePageXY(e);

		const resizeGripSize = this.resizeGripElem.getBoundingClientRect();
		const outerContainerSize = this.outerContainerElem.getBoundingClientRect();
		const valuesTableSize = this.valuesTableElem.tBodies[0].getBoundingClientRect();

		const outerContainerWidth = outerContainerSize.right - outerContainerSize.left;
		const outerContainerHeight = outerContainerSize.bottom - outerContainerSize.top;

		const offset = {
			x: outerContainerWidth <= this.minContainerWidth && mousePageXY.pageX < resizeGripSize.left ? 0 : mousePageXY.pageX - this.mousedownpos.x,
			y: outerContainerHeight <= this.minContainerHeight && mousePageXY.pageY < resizeGripSize.top ? 0 : mousePageXY.pageY - this.mousedownpos.y
		};

		const newContainerWidth = outerContainerWidth  + offset.x;
		const newContainerHeight = outerContainerHeight  + offset.y;

		this.mousedownpos.x = mousePageXY.pageX;
		this.mousedownpos.y = mousePageXY.pageY;

		if(newContainerWidth >= this.minContainerWidth) {
			this.outerContainerElem.style.width = `${newContainerWidth}px`;
		}

		if(newContainerHeight >= this.minContainerHeight) {
			this.outerContainerElem.style.height = `${newContainerHeight}px`;
			this.valuesTableElem.tBodies[0].style.height = `${valuesTableSize.bottom - valuesTableSize.top}${offset.y}px`;
		}

		utils.stopPropagation(e);
		utils.preventDefault(e);
	};


}
