import * as React from 'react';
import * as ReactDOM from 'react-dom';
import DragManager from './orb.react.DragManager';
import {synchronizeWidths} from './orb.react.PivotTable.SizingManager';
import Toolbar from './orb.react.Toolbar';
import UpperButtons from './orb.react.PivotTable.UpperButtons';
import ColumnButtons from './orb.react.PivotTable.ColumnButtons';
import RowButtons from './orb.react.PivotTable.RowButtons';
import Chart from './orb.react.Chart';
import * as domUtils from '../orb.utils.dom';
let pivotId = 1;
const themeChangeCallbacks = {};

export default class PivotChartComponent extends React.Component<any,any>{

  id;
  pgrid;
  pgridwidgetstore;
  fontStyle;

  constructor(){
    super();
    this.id = pivotId++;
    DragManager.init(this);

    themeChangeCallbacks[this.id] = [];
    this.registerThemeChanged(this.updateClasses);

    this.pgridwidgetstore = this.props.pgridwidgetstore;
    this.pgrid = this.pgridwidgetstore.pgrid;
  }
  sort(axetype, field) {
    this.pgridwidgetstore.sort(axetype, field);
  }
  moveButton(button, newAxeType, position) {
    this.pgridwidgetstore.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
  }
  applyFilter(fieldname, operator, term, staticValue, excludeStatic) {
    this.pgridwidgetstore.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
  }
  registerThemeChanged(compCallback) {
    if(compCallback) {
      themeChangeCallbacks[this.id].push(compCallback);
    }
  }
  unregisterThemeChanged(compCallback) {
    let i;
    if(compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
      themeChangeCallbacks[this.id].splice(i, 1);
    }
  }
  changeTheme(newTheme) {
    if(this.pgridwidgetstore.pgrid.config.setTheme(newTheme)) {
      // notify this/sub-components of the theme change
      for(let i = 0; i < themeChangeCallbacks[this.id].length; i++) {
        themeChangeCallbacks[this.id][i]();
      }
    }
  }
  updateClasses() {
      const thisnode = ReactDOM.findDOMNode(this);
      const classes = this.pgridwidgetstore.pgrid.config.theme.getPivotClasses();
      thisnode.className = classes.container;
      thisnode['children'][1].className = classes.table;
  }
  componentDidUpdate() {
    this.synchronizeWidths();
  }
  componentDidMount() {
      const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true);
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    };

    this.synchronizeWidths();
  }
  synchronizeWidths() {
    const chartStyle = synchronizeWidths(this);
    chartStyle['fontFamily'] = this.fontStyle.fontFamily;
    chartStyle['fontSize'] = this.fontStyle.fontSize;

    (this.refs['chart'] as Chart).setState({
      canRender: true,
      chartStyle
    });
  }
  render() {

    const config = this.pgridwidgetstore.pgrid.config;
    const classes = config.theme.getPivotClasses();

    const tblStyle = {width: undefined, height: undefined};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (<div className={classes.container} style={tblStyle} ref="pivot">
      <table id={'tbl-' + this.id} ref="pivotWrapperTable" className={classes.table}>
        <colgroup>
          <col ref="column1"></col>
          <col ref="column2"></col>
        </colgroup>
        <tbody>
          <tr ref="upperButtons">
            <td colSpan="2">
              <UpperButtons pivotTableComp={this}></UpperButtons>
            </td>
          </tr>
          <tr ref="colButtons">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <ColumnButtons pivotTableComp={this}></ColumnButtons>
            </td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <RowButtons pivotTableComp={this} ref="rowButtons"></RowButtons>
            </td>
            <td>
              <Chart pivotTableComp={this} chartMode={config.chartMode} ref="chart"></Chart>
            </td>
          </tr>
        </tbody>
      </table>
    </div>);
  }
};
