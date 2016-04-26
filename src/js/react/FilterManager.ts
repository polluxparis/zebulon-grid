import * as utils from '../orb.utils';
import * as filtering from '../orb.filtering';
import * as domUtils from '../orb.utils.dom';
import {ResizeManager} from './ResizeManager';

export class FilterManager{

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
		console.log('onOperatorChanged');
		console.log(newOperator);
		console.log(this);
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

		utils.addEventListener(this.elems.filterContainer, 'click', this.valueChecked.bind(this));
		utils.addEventListener(this.elems.searchBox, 'keyup', this.searchChanged.bind(this));

		utils.addEventListener(this.elems.clearSearchButton, 'click', this.clearSearchBox.bind(this));

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
			utils.addEventListener(this.elems.enableRegexButton, 'click', this.regexpActiveChanged.bind(this));
			domUtils.removeClass(this.elems.enableRegexButton, 'srchtyp-col-hidden');

		} else {
			utils.removeEventListener(this.elems.enableRegexButton, 'click', this.regexpActiveChanged.bind(this));
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
		console.log('valueChecked');
		console.log(this);
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
