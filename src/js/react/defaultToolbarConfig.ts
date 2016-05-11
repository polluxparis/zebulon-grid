import * as React from 'react';
import excelExport from '../orb.export.excel';
import {removeClass, addClass} from '../orb.utils.dom';
import {AxeType} from '../orb.axe';

export function exportToExcel(pgridComponent, button) {
  const a = document.createElement('a');
  a['download'] = "orbpivotgrid.xls";
  a.href =  excelExport(pgridComponent.props.pgridwidgetstore);
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}
export function expandAllRows(pgridComponent, button) {
    pgridComponent.pgridwidgetstore.toggleFieldExpansion(AxeType.ROWS, null, true);
}
export function collapseAllRows(pgridComponent, button) {
    pgridComponent.pgridwidgetstore.toggleFieldExpansion(AxeType.ROWS, null, false);
}
export function expandAllColumns(pgridComponent, button) {
    pgridComponent.pgridwidgetstore.toggleFieldExpansion(AxeType.COLUMNS, null, true);
}
export function collapseAllColumns(pgridComponent, button) {
    pgridComponent.pgridwidgetstore.toggleFieldExpansion(AxeType.COLUMNS, null, false);
}
export function updateSubtotalsButton(axetype, pgridComponent, button) {
  const subTotalsState = pgridComponent.pgridwidgetstore.areSubtotalsVisible(axetype);
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

  removeClass(button, classToRemove);
  addClass(button, classToAdd);
}
export function initSubtotals(axetype) {
  return (pgridComponent, button) => {
    updateSubtotalsButton(axetype, pgridComponent, button);
  };
}
export function toggleSubtotals(axetype) {
  return (pgridComponent, button) => {
    pgridComponent.toggleSubtotals(axetype);
    updateSubtotalsButton(axetype, pgridComponent, button);
  };
}
export function updateGrandtotalButton(axetype, pgridComponent, button) {
  const subTotalsState = pgridComponent.pgridwidgetstore.isGrandtotalVisible(axetype);
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

  removeClass(button, classToRemove);
  addClass(button, classToAdd);
}
export function initGrandtotal(axetype) {
  return (pgridComponent, button) => {
    updateGrandtotalButton(axetype, pgridComponent, button);
  };
}
export function toggleGrandtotal(axetype) {
  return (pgridComponent, button) => {
    pgridComponent.toggleGrandtotal(axetype);
    updateGrandtotalButton(axetype, pgridComponent, button);
  };
}

interface Button{
  type: string;
  text?:string;
  tooltip?:string;
  cssClass?: string;
  init?:{(pgridComponent: any, btn:Button): void};
  action?:{(pgridComponent: any, btn:Button): void};
}
export const buttons: Button[]= [
  { type: 'label', text: 'Rows:'},
  { type: 'button', tooltip: 'Expand all rows', cssClass: 'expand-all', action: expandAllRows},
  { type: 'button', tooltip: 'Collapse all rows', cssClass: 'collapse-all', action: collapseAllRows},
  { type: 'button', tooltip: 'Toggle rows sub totals', init: initSubtotals(AxeType.ROWS),
                                                       action: toggleSubtotals(AxeType.ROWS)},
  { type: 'button', tooltip: 'Toggle rows grand total', init: initGrandtotal(AxeType.ROWS),
                                                        action: toggleGrandtotal(AxeType.ROWS)},
  { type: 'separator'},
  { type: 'label', text: 'Columns:'},
  { type: 'button', tooltip: 'Expand all columns', cssClass: 'expand-all', action: expandAllColumns},
  { type: 'button', tooltip: 'Collapse all columns', cssClass: 'collapse-all', action: collapseAllColumns},
  { type: 'button', tooltip: 'Toggle columns sub totals', init: initSubtotals(AxeType.COLUMNS),
                                                          action: toggleSubtotals(AxeType.COLUMNS)},
  { type: 'button', tooltip: 'Toggle columns grand total', init: initGrandtotal(AxeType.COLUMNS),
                                                           action: toggleGrandtotal(AxeType.COLUMNS)},
  { type: 'separator'},
  { type: 'label', text: 'Export:'},
  { type: 'button', tooltip: 'Export to Excel', cssClass: 'export-xls', action: exportToExcel}
];
