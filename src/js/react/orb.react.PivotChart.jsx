import React from 'react';
import ReactDOM from 'react-dom';
import DragManager from './orb.react.DragManager.jsx';
import SizingManager from './orb.react.PivotTable.SizingManager.jsx';
import Toolbar from './orb.react.Toolbar.jsx';
import UpperButtons from './orb.react.PivotTable.UpperButtons.jsx';
import ColumnButtons from './orb.react.PivotTable.ColumnButtons.jsx';
import RowButtons from './orb.react.PivotTable.RowButtons.jsx';
import Chart from './orb.react.Chart.jsx';
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

    this.synchronizeWidths();
  },
  synchronizeWidths() {
    const chartStyle = SizingManager.synchronizeWidths(this);
    chartStyle.fontFamily = this.fontStyle.fontFamily;
    chartStyle.fontSize = this.fontStyle.fontSize;

    this.refs.chart.setState({
      canRender: true,
      chartStyle
    });
  },
  render() {

    const self = this;

    const config = this.pgridwidget.pgrid.config;
    const classes = config.theme.getPivotClasses();

    const tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    // return (<div className={classes.container} style={tblStyle} ref="pivot">
    //   <table id={'tbl-' + self.id} ref="pivotWrapperTable" className={classes.table}>
    //     <colgroup>
    //       <col ref="column1"></col>
    //       <col ref="column2"></col>
    //     </colgroup>
    //     <tbody>
    //       <tr ref="upperButtons">
    //         <td colSpan="2">
    //           <UpperButtons pivotTableComp={self}></UpperButtons>
    //         </td>
    //       </tr>
    //       <tr ref="colButtons">
    //         <td></td>
    //         <td style={{padding: '11px 4px !important'}}>
    //           <ColumnButtons pivotTableComp={self}></ColumnButtons>
    //         </td>
    //       </tr>
    //       <tr>
    //         <td style={{ position: 'relative'}}>
    //           <RowButtons pivotTableComp={self} ref="rowButtons"></RowButtons>
    //         </td>
    //         <td>
    //           <Chart pivotTableComp={self} chartMode={config.chartMode} ref="chart"></Chart>
    //         </td>
    //       </tr>
    //     </tbody>
    //   </table>
    // </div>);
  }
});