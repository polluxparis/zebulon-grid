import * as React from 'react';
import * as ReactDOM from 'react-dom';
import DragManager from './orb.react.DragManager';
import * as SizingManager from './orb.react.PivotTable.SizingManager';
import Toolbar from './orb.react.Toolbar';
import UpperButtons from './orb.react.PivotTable.UpperButtons';
import ColumnButtons from './orb.react.PivotTable.ColumnButtons';
import RowButtons from './orb.react.PivotTable.RowButtons';
import RowHeaders from './orb.react.PivotTable.RowHeaders';
import ColumnHeaders from './orb.react.PivotTable.ColumnHeaders';
import DataCells from './orb.react.PivotTable.DataCells';
import {ScrollBar} from './orb.react.ScrollBars';
import * as utils from '../orb.utils';
import * as domUtils from '../orb.utils.dom';

import {Grid, ScrollSync} from 'react-virtualized';

import {observer} from 'mobx-react';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';
import {PGrid} from '../orb.pgrid';


let pivotId = 1;
const themeChangeCallbacks = {};

export interface PivotTableProps{
  pgridwidgetstore: PGridWidgetStore
}

// CSS files
import 'react-virtualized/styles.css';
// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css');
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css');

@observer
export class PivotTableComponent extends React.Component<PivotTableProps,{}>{

  id:number = pivotId++;
  pgrid: PGrid = null;
  pgridwidgetstore: PGridWidgetStore = null;
  fontStyle = null;

  constructor(props){
    super(props);
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
  toggleSubtotals(axetype) {
    this.pgridwidgetstore.toggleSubtotals(axetype);
  }
  toggleGrandtotal(axetype) {
    this.pgridwidgetstore.toggleGrandtotal(axetype);
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
      // notify self/sub-components of the theme change
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

  componentDidMount() {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true);
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    };
  }

  shouldComponentUpdate(nextProps, nextState){
    console.log(`shouldComponentUpdate`);
    // return shallowCompare(this, nextProps, nextState);
  }

  render() {

    const cellHeight = this.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.pgridwidgetstore.layout.cell.width;

    const rowVerticalCount = this.pgridwidgetstore.layout.rowHeaders.height;
    const rowHorizontalCount = this.pgridwidgetstore.layout.rowHeaders.width;
    const columnVerticalCount = this.pgridwidgetstore.layout.columnHeaders.height;
    const columnHorizontalCount = this.pgridwidgetstore.layout.columnHeaders.width;

    const config = this.pgridwidgetstore.pgrid.config;
    const classes = config.theme.getPivotClasses();

    const tblStyle = {width:undefined, height:undefined};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
      <div className={classes.container}>
        <div className={'orb'}>
          <UpperButtons pivotTableComp={this}></UpperButtons>
          <div style={{
            position: 'relative',
            left: cellWidth*rowHorizontalCount,
          }}>
            <ColumnButtons pivotTableComp={this}/>
          </div>

          <div style={{position: 'relative'}}>
          <ScrollSync>
          {({ clientHeight, clientWidth, onScroll, scrollHeight, scrollLeft, scrollTop, scrollWidth }) => {
            return <div style={tblStyle} ref="pivot">
            <div
            style={{
              position: 'absolute',
              left: 0,
              top: 0
            }}>
            <RowButtons pivotTableComp={this} ref="rowButtons"/>
            <div
            style={{
              backgroundColor: '#eef8fb',
              margin: '2px',
              position: 'absolute',
              left: 0,
              top: cellHeight*columnVerticalCount+20,
              height: Math.min(config.height-cellHeight*columnVerticalCount,cellHeight*rowVerticalCount),
              width: rowHorizontalCount*cellWidth

            }}>
            <RowHeaders pgridwidgetstore={this.props.pgridwidgetstore} onScroll={onScroll} scrollTop={scrollTop} ref="rowHeaders"/>
            </div>
            <div
            style={{
              margin: '2px',
              position: 'absolute',
              left: cellWidth*rowHorizontalCount,
              top: 20,
              width: cellWidth*columnHorizontalCount
            }}>
            <div style={{
              backgroundColor: '#eef8fb',
              width: Math.min(config.width-cellWidth*rowHorizontalCount,cellWidth*columnHorizontalCount),
              height: columnVerticalCount*cellHeight
            }} >
              <ColumnHeaders pgridwidgetstore={this.props.pgridwidgetstore} onScroll={onScroll} scrollLeft={scrollLeft}ref="colHeaders"></ColumnHeaders>
            </div>
            <div style={{
              width: Math.min(config.width-cellWidth*rowHorizontalCount,cellWidth*columnHorizontalCount),
              height: Math.min(config.height-cellHeight*columnVerticalCount,cellHeight*rowVerticalCount)
            }}>
              <DataCells pgridwidgetstore={this.props.pgridwidgetstore} onScroll={onScroll} scrollTop={scrollTop} scrollLeft={scrollLeft} ref="dataCells"/>
            </div>
            </div>
            </div>
            </div>
          }}
          </ScrollSync>
          <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + this.id}></div>
          </div>
        </div>
      </div>
    );

    // return (
    // <div className={classes.container} style={tblStyle} ref="pivot">
    //   {config.toolbar && config.toolbar.visible ? <div ref="toolbar" className="orb-toolbar">
    //     <Toolbar pgridwidgetstore={this.props.pgridwidgetstore}></Toolbar>
    //   </div> : null}
    //   <table id={'tbl-' + this.id} ref="pivotWrapperTable" className={classes.table} style={{tableLayout: 'fixed'}}>
    //     <colgroup>
    //       <col ref="column1"></col>
    //       <col ref="column2"></col>
    //       <col ref="column3"></col>
    //       <col ref="column4"></col>
    //     </colgroup>
    //     <tbody>
    //       <tr ref="upperButtons">
    //         <td colSpan="4">
    //           <UpperButtons pgridwidgetstore={this.props.pgridwidgetstore}></UpperButtons>
    //         </td>
    //       </tr>
    //       <tr ref="colButtons">
    //         <td></td>
    //         <td style={{padding: '11px 4px !important'}}>
    //           <ColumnButtons pgridwidgetstore={this.props.pgridwidgetstore}></ColumnButtons>
    //         </td>
    //         <td colSpan="2"></td>
    //       </tr>
    //       <tr>
    //         <td style={{ position: 'relative'}}>
    //           <RowButtons pgridwidgetstore={this.props.pgridwidgetstore} ref="rowButtons"></RowButtons>
    //         </td>
    //         <td>
    //           <ColumnHeaders pgridwidgetstore={this.props.pgridwidgetstore} ref="colHeaders"></ColumnHeaders>
    //         </td>
    //         <td colSpan="2"></td>
    //       </tr>
    //       <tr>
    //         <td>
    //           <RowHeaders pgridwidgetstore={this.props.pgridwidgetstore} ref="rowHeaders"></RowHeaders>
    //         </td>
    //         <td>
    //           <DataCells pgridwidgetstore={this.props.pgridwidgetstore} ref="dataCells"></DataCells>
    //         </td>
    //         <td>
    //           <ScrollBar pgridwidgetstore={this.props.pgridwidgetstore} axis='vertical' ref="verticalScrollBar"></ScrollBar>
    //         </td>
    //         <td></td>
    //       </tr>
    //       <tr>
    //         <td></td>
    //         <td>
    //           <ScrollBar pgridwidgetstore={this.props.pgridwidgetstore} axis='horizontal' ref="horizontalScrollBar"></ScrollBar>
    //         </td>
    //         <td colSpan="2"></td>
    //       </tr>
    //     </tbody>
    //   </table>
    //   <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + this.id}></div>
    // </div>
    // );

  }
}
