import React from 'react';
import ReactDOM from 'react-dom';
import DragManager from './orb.react.DragManager.jsx';
import SizingManager from './orb.react.PivotTable.SizingManager.jsx';
import Toolbar from './orb.react.Toolbar.jsx';
import UpperButtons from './orb.react.PivotTable.UpperButtons.jsx';
import ColumnButtons from './orb.react.PivotTable.ColumnButtons.jsx';
import RowButtons from './orb.react.PivotTable.RowButtons.jsx';
import RowHeaders from './orb.react.PivotTable.RowHeaders.jsx';
import ColumnHeaders from './orb.react.PivotTable.ColumnHeaders.jsx';
import DataCells from './orb.react.PivotTable.DataCells.jsx';
import ScrollBars from './orb.react.ScrollBars.jsx';
const HorizontalScrollBar = ScrollBars.HorizontalScrollBar;
const VerticalScrollBar = ScrollBars.VerticalScrollBar;
import utils from '../orb.utils';
import domUtils from '../orb.utils.dom';
let pivotId = 1;
const themeChangeCallbacks = {};

export default React.createClass({
  id: pivotId++,
  pgrid: null,
  pgridwidget: null,
  fontStyle: null,
  getInitialState() {
    DragManager.init(this);

    themeChangeCallbacks[this.id] = [];
    this.registerThemeChanged(this.updateClasses);

    this.pgridwidget = this.props.pgridwidget;
    this.pgrid = this.pgridwidget.pgrid;
    return {};
  },
  sort(axetype, field) {
    this.pgridwidget.sort(axetype, field);
  },
  moveButton(button, newAxeType, position) {
    this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
  },
  toggleSubtotals(axetype) {
    this.pgridwidget.toggleSubtotals(axetype);
  },
  toggleGrandtotal(axetype) {
    this.pgridwidget.toggleGrandtotal(axetype);
  },
  applyFilter(fieldname, operator, term, staticValue, excludeStatic) {
    this.pgridwidget.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
  },
  registerThemeChanged(compCallback) {
    if(compCallback) {
      themeChangeCallbacks[this.id].push(compCallback);
    }
  },
  unregisterThemeChanged(compCallback) {
    let i;
    if(compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
      themeChangeCallbacks[this.id].splice(i, 1);
    }
  },
  changeTheme(newTheme) {
    if(this.pgridwidget.pgrid.config.setTheme(newTheme)) {
      // notify self/sub-components of the theme change
      for(let i = 0; i < themeChangeCallbacks[this.id].length; i++) {
        themeChangeCallbacks[this.id][i]();
      }
    }
  },
  updateClasses() {
      const thisnode = ReactDOM.findDOMNode(this);
      const classes = this.pgridwidget.pgrid.config.theme.getPivotClasses();
      thisnode.className = classes.container;
      thisnode.children[1].className = classes.table;
  },
  componentDidUpdate() {
    this.synchronizeWidths();
  },
  componentDidMount() {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true);
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    };

    const dataCellsNode = ReactDOM.findDOMNode(this.refs.dataCells);
    const dataCellsTableNode = dataCellsNode.children[0];
    const colHeadersNode = ReactDOM.findDOMNode(this.refs.colHeaders);
    const rowHeadersNode = ReactDOM.findDOMNode(this.refs.rowHeaders);

    this.refs.horizontalScrollBar.setScrollClient(dataCellsNode, scrollPercent => {
      const scrollAmount = Math.ceil(
        scrollPercent * (
          domUtils.getSize(dataCellsTableNode).width -
          domUtils.getSize(dataCellsNode).width
        )
      );
      colHeadersNode.scrollLeft = scrollAmount;
      dataCellsNode.scrollLeft = scrollAmount;
    });

    this.refs.verticalScrollBar.setScrollClient(dataCellsNode, scrollPercent => {
      const scrollAmount = Math.ceil(
        scrollPercent * (
          domUtils.getSize(dataCellsTableNode).height -
          domUtils.getSize(dataCellsNode).height
        )
      );
      rowHeadersNode.scrollTop = scrollAmount;
      dataCellsNode.scrollTop = scrollAmount;
    });

    this.synchronizeWidths();
  },
  onWheel(e) {
    let elem;
    let scrollbar;
    let amount;

    if(e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.colHeaders))) {
      scrollbar = this.refs.horizontalScrollBar;
      amount = e.deltaX || e.deltaY;
    } else if ((e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.rowHeaders))) ||
              (e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.dataCells))) ) {
      scrollbar = this.refs.verticalScrollBar;
      amount = e.deltaY;
    }

    if(scrollbar && scrollbar.scroll(amount, e.deltaMode)) {
      utils.stopPropagation(e);
      utils.preventDefault(e);
    }
  },
  synchronizeWidths() {
    SizingManager.synchronizeWidths(this);
    this.refs.horizontalScrollBar.refresh();
    this.refs.verticalScrollBar.refresh();
  },
  render() {

    const self = this;

    const config = this.pgridwidget.pgrid.config;
    const classes = config.theme.getPivotClasses();

    const tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return 'toto';
    return (
    <div className={classes.container} style={tblStyle} ref="pivot">
      <table id={'tbl-' + self.id} ref="pivotWrapperTable" className={classes.table} style={{tableLayout: 'fixed'}}>
        <tbody>
          <tr ref="upperButtons">
            <td colSpan="4">
              <UpperButtons pivotTableComp={self}></UpperButtons>
            </td>
          </tr>
          <tr ref="colButtons">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <ColumnButtons pivotTableComp={self}></ColumnButtons>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <RowButtons pivotTableComp={self} ref="rowButtons"></RowButtons>
            </td>
            <td>
              <ColumnHeaders pivotTableComp={self} ref="colHeaders"></ColumnHeaders>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td>
              <RowHeaders pivotTableComp={self} ref="rowHeaders"></RowHeaders>
            </td>
            <td>
              <DataCells pivotTableComp={self} ref="dataCells"></DataCells>
            </td>
            <td>
              <VerticalScrollBar pivotTableComp={self} ref="verticalScrollBar"></VerticalScrollBar>
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td>
              <HorizontalScrollBar pivotTableComp={self} ref="horizontalScrollBar"></HorizontalScrollBar>
            </td>
            <td colSpan="2"></td>
          </tr>
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
    );
    return (
    <div className={classes.container} style={tblStyle} ref="pivot">
      {config.toolbar && config.toolbar.visible ? <div ref="toolbar" className="orb-toolbar">
        <Toolbar pivotTableComp={self}></Toolbar>
      </div> : null}
      <table id={'tbl-' + self.id} ref="pivotWrapperTable" className={classes.table} style={{tableLayout: 'fixed'}}>
        <colgroup>
          <col ref="column1"></col>
          <col ref="column2"></col>
          <col ref="column3"></col>
          <col ref="column4"></col>
        </colgroup>
        <tbody>
          <tr ref="upperButtons">
            <td colSpan="4">
              <UpperButtons pivotTableComp={self}></UpperButtons>
            </td>
          </tr>
          <tr ref="colButtons">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <ColumnButtons pivotTableComp={self}></ColumnButtons>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <RowButtons pivotTableComp={self} ref="rowButtons"></RowButtons>
            </td>
            <td>
              <ColumnHeaders pivotTableComp={self} ref="colHeaders"></ColumnHeaders>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td>
              <RowHeaders pivotTableComp={self} ref="rowHeaders"></RowHeaders>
            </td>
            <td>
              <DataCells pivotTableComp={self} ref="dataCells"></DataCells>
            </td>
            <td>
              <VerticalScrollBar pivotTableComp={self} ref="verticalScrollBar"></VerticalScrollBar>
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td>
              <HorizontalScrollBar pivotTableComp={self} ref="horizontalScrollBar"></HorizontalScrollBar>
            </td>
            <td colSpan="2"></td>
          </tr>
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
    );
  }
});