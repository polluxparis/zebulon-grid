import React from 'react';
import axe from '../orb.axe';
import domUtils from '../orb.utils.dom';

export default React.createClass({
  _toInit: [],
  componentDidMount() {
    for(let i = 0; i < this._toInit.length; i++){
      const btn = this._toInit[i];
      btn.init(this.props.pivotTableComp, this.refs[btn.ref]);
    }
  },
  componentDidUpdate() {
    for(let i = 0; i < this._toInit.length; i++){
      const btn = this._toInit[i];
      btn.init(this.props.pivotTableComp, this.refs[btn.ref]);
    }
  },
  createCallback(action) {
    if(action != null) {
      const pgridComponent = this.props.pivotTableComp;
      return e => {
        action(pgridComponent, e.target || e.srcElement);
      };
    }
    return null;
  },
  render() {

    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    if(config.toolbar && config.toolbar.visible) {

      const configButtons = config.toolbar.buttons ?
        defaultToolbarConfig.buttons.concat(config.toolbar.buttons) :
        defaultToolbarConfig.buttons;

      const buttons = [];
      for(let i = 0; i < configButtons.length; i++) {
        const btnConfig = configButtons[i];
        const refName = `btn${i}`;

        if(btnConfig.type == 'separator') {
          buttons.push(<div key={i} className="orb-tlbr-sep"></div>);
        } else if(btnConfig.type == 'label') {
          buttons.push(<div key={i} className="orb-tlbr-lbl">{btnConfig.text}</div>);
        } else {
          buttons.push(<div key={i} className={'orb-tlbr-btn ' + btnConfig.cssClass} title={btnConfig.tooltip} ref={refName} onClick={ this.createCallback(btnConfig.action) }></div>);
        }
        if(btnConfig.init) {
          this._toInit.push({
            ref: refName,
            init: btnConfig.init
          });
        }
      }

      return <div>
        { buttons }
        </div>;
    }

    return <div></div>;
  }
});

import excelExport from '../orb.export.excel';

var defaultToolbarConfig = {
  exportToExcel(pgridComponent, button) {
    const a = document.createElement('a');
    a.download = "orbpivotgrid.xls";
    a.href =  excelExport(pgridComponent.props.pgridwidget);
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
  },
  expandAllRows(pgridComponent, button) {
      pgridComponent.pgridwidget.toggleFieldExpansion(axe.Type.ROWS, null, true);
  },
  collapseAllRows(pgridComponent, button) {
      pgridComponent.pgridwidget.toggleFieldExpansion(axe.Type.ROWS, null, false);
  },
  expandAllColumns(pgridComponent, button) {
      pgridComponent.pgridwidget.toggleFieldExpansion(axe.Type.COLUMNS, null, true);
  },
  collapseAllColumns(pgridComponent, button) {
      pgridComponent.pgridwidget.toggleFieldExpansion(axe.Type.COLUMNS, null, false);
  },
  updateSubtotalsButton(axetype, pgridComponent, button) {
    const subTotalsState = pgridComponent.pgridwidget.areSubtotalsVisible(axetype);
    button.style.display = subTotalsState === null ? 'none' : '';

    let classToAdd = '';
    let classToRemove = '';
    if(subTotalsState) {
      classToAdd = 'subtotals-visible';
      classToRemove = 'subtotals-hidden';
    } else {
      classToAdd = 'subtotals-hidden';
      classToRemove = 'subtotals-visible';
    }

    domUtils.removeClass(button, classToRemove);
    domUtils.addClass(button, classToAdd);
  },
  initSubtotals(axetype) {
    const self = this;
    return (pgridComponent, button) => {
      self.updateSubtotalsButton(axetype, pgridComponent, button);
    };
  },
  toggleSubtotals(axetype) {
    const self = this;
    return (pgridComponent, button) => {
      pgridComponent.toggleSubtotals(axetype);
      self.updateSubtotalsButton(axetype, pgridComponent, button);
    };
  },
  updateGrandtotalButton(axetype, pgridComponent, button) {
    const subTotalsState = pgridComponent.pgridwidget.isGrandtotalVisible(axetype);
    button.style.display = subTotalsState === null ? 'none' : '';

    let classToAdd = '';
    let classToRemove = '';
    if(subTotalsState) {
      classToAdd = 'grndtotal-visible';
      classToRemove = 'grndtotal-hidden';
    } else {
      classToAdd = 'grndtotal-hidden';
      classToRemove = 'grndtotal-visible';
    }

    domUtils.removeClass(button, classToRemove);
    domUtils.addClass(button, classToAdd);
  },
  initGrandtotal(axetype) {
    const self = this;
    return (pgridComponent, button) => {
      self.updateGrandtotalButton(axetype, pgridComponent, button);
    };
  },
  toggleGrandtotal(axetype) {
    const self = this;
    return (pgridComponent, button) => {
      pgridComponent.toggleGrandtotal(axetype);
      self.updateGrandtotalButton(axetype, pgridComponent, button);
    };
  }
};

defaultToolbarConfig.buttons = [
  { type: 'label', text: 'Rows:'},
  { type: 'button', tooltip: 'Expand all rows', cssClass: 'expand-all', action: defaultToolbarConfig.expandAllRows},
  { type: 'button', tooltip: 'Collapse all rows', cssClass: 'collapse-all', action: defaultToolbarConfig.collapseAllRows},
  { type: 'button', tooltip: 'Toggle rows sub totals', init: defaultToolbarConfig.initSubtotals(axe.Type.ROWS),
                                                       action: defaultToolbarConfig.toggleSubtotals(axe.Type.ROWS)},
  { type: 'button', tooltip: 'Toggle rows grand total', init: defaultToolbarConfig.initGrandtotal(axe.Type.ROWS),
                                                        action: defaultToolbarConfig.toggleGrandtotal(axe.Type.ROWS)},
  { type: 'separator'},
  { type: 'label', text: 'Columns:'},
  { type: 'button', tooltip: 'Expand all columns', cssClass: 'expand-all', action: defaultToolbarConfig.expandAllColumns},
  { type: 'button', tooltip: 'Collapse all columns', cssClass: 'collapse-all', action: defaultToolbarConfig.collapseAllColumns},
  { type: 'button', tooltip: 'Toggle columns sub totals', init: defaultToolbarConfig.initSubtotals(axe.Type.COLUMNS),
                                                          action: defaultToolbarConfig.toggleSubtotals(axe.Type.COLUMNS)},
  { type: 'button', tooltip: 'Toggle columns grand total', init: defaultToolbarConfig.initGrandtotal(axe.Type.COLUMNS),
                                                           action: defaultToolbarConfig.toggleGrandtotal(axe.Type.COLUMNS)},
  { type: 'separator'},
  { type: 'label', text: 'Export:'},
  { type: 'button', tooltip: 'Export to Excel', cssClass: 'export-xls', action: defaultToolbarConfig.exportToExcel}
];