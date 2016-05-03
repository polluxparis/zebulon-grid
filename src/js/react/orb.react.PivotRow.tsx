import * as React from 'react';
import PivotCell from './orb.react.PivotCell';
import {AxeType} from '../orb.axe';

import {Grid} from 'react-virtualized';

import PivotTableComponent from './orb.react.PivotTable';
import {Header, DataHeader, DataCell, ButtonCell, EmptyCell} from '../orb.ui.header';

interface Props{
key:number,
row:(Header|DataHeader|DataCell|ButtonCell|EmptyCell)[],
axetype:number,
layoutInfos:{
  lastLeftMostCellVSpan: number,
  topMostCells: {}
},
pivotTableComp:PivotTableComponent,
onScroll?: any,
scrollLeft?: any
}

export default class PivotRowComponent extends React.Component<Props,{}>{
  private lastCellIndex
  private cell0
  private leftmostCellFound = false;
  private layoutInfos
  private istopmost=false;
  constructor(props){
    super(props);
    this.lastCellIndex = this.props.row.length - 1;
    this.cell0 = this.props.row[0];
    this.layoutInfos = this.props.layoutInfos;
    this.renderCell = this.renderCell.bind(this);
  }

  renderCell({columnIndex, rowIndex}) {

    let cell = this.props.row[columnIndex]
    let isleftmost = false;

    // If current cells are column/data headers and left most cell is not found yet
    // and last row left most cell does not span vertically over the current one and current one is visible
    // then mark IT as the left most cell
    if(cell.visible() && this.layoutInfos) {
      if((cell as Header).dim) {
        if(
          ((cell as Header).dim.isRoot && this.layoutInfos.topMostCells[(cell as Header).dim.depth - 1] === undefined) ||
          (
           !(cell as Header).dim.isRoot && this.layoutInfos.topMostCells[(cell as Header).dim.depth] === undefined &&
            ((cell as Header).dim.parent.isRoot || this.layoutInfos.topMostCells[(cell as Header).dim.depth + 1] === (cell as Header).dim.parent))
          ) {
          this.istopmost = true;
          this.layoutInfos.topMostCells[(cell as Header).dim.depth] = (cell as Header).dim;
        }
      } else if(!this.layoutInfos.topMostCells['0']) {
        this.istopmost = this.layoutInfos.topMostCells['0'] = true;
      }

      if(!this.leftmostCellFound && (this.props.axetype === AxeType.DATA || this.props.axetype === AxeType.COLUMNS) &&
          this.layoutInfos.lastLeftMostCellVSpan === 0) {

        isleftmost = this.leftmostCellFound = true;
        this.layoutInfos.lastLeftMostCellVSpan = cell.vspan() - 1;
      }
    }

    return <PivotCell key={columnIndex}
                      cell={cell}
                      leftmost={isleftmost}
                      topmost={this.istopmost}
                      pivotTableComp={this.props.pivotTableComp}>
           </PivotCell>;
  }

  _render() {

    // decrement lastLeftMostCellVSpan
    // if(this.layoutInfos && this.layoutInfos.lastLeftMostCellVSpan > 0 && !this.leftmostCellFound) {
    //   this.layoutInfos.lastLeftMostCellVSpan--;
    // }

    const pgridwidget = this.props.pivotTableComp.pgridwidget;

    const config = pgridwidget.pgrid.config;

    const rowHeight = 30;
    // const hspan = this.props.row[0].hspan(false);
    const hspan = 1;

    return (
      <Grid
        onScroll={this.props.onScroll}
        scrollLeft={this.props.scrollLeft}
        height={rowHeight}
        width={config.width}
        rowHeight={rowHeight}
        columnWidth={100*hspan}
        columnsCount={this.props.row.length/hspan}
        rowsCount={1}
        renderCell={this.renderCell}
      />
    );
  }

  render() {
    const lastCellIndex = this.props.row.length - 1;
    const cell0 = this.props.row[0];
    let leftmostCellFound = false;
    const layoutInfos = this.props.layoutInfos;
    let cells;

    const rowstyle = {};

    let istopmost = false;

    cells = this.props.row.map((cell, index) => {

      let isleftmost = false;

      // If current cells are column/data headers and left most cell is not found yet
      // and last row left most cell does not span vertically over the current one and current one is visible
      // then mark IT as the left most cell
      if(cell.visible() && layoutInfos) {
        if((cell as Header).dim) {
          if(((cell as Header).dim.isRoot && layoutInfos.topMostCells[(cell as Header).dim.depth - 1] === undefined) || (!(cell as Header).dim.isRoot && layoutInfos.topMostCells[(cell as Header).dim.depth] === undefined && ((cell as Header).dim.parent.isRoot || layoutInfos.topMostCells[(cell as Header).dim.depth + 1] === (cell as Header).dim.parent))) {
            istopmost = true;
            layoutInfos.topMostCells[(cell as Header).dim.depth] = (cell as Header).dim;
          }
        } else if(!layoutInfos.topMostCells['0']) {
          istopmost = layoutInfos.topMostCells['0'] = true;
        }

        if(!leftmostCellFound && (this.props.axetype === AxeType.DATA || this.props.axetype === AxeType.COLUMNS) &&
            layoutInfos.lastLeftMostCellVSpan === 0) {

          isleftmost = leftmostCellFound = true;
          layoutInfos.lastLeftMostCellVSpan = cell.vspan() - 1;
        }
      }

      return <PivotCell key={index}
                        cell={cell}
                        leftmost={isleftmost}
                        topmost={istopmost}
                        pivotTableComp={this.props.pivotTableComp}>
             </PivotCell>;
    });

    // decrement lastLeftMostCellVSpan
    if(layoutInfos && layoutInfos.lastLeftMostCellVSpan > 0 && !leftmostCellFound) {
      layoutInfos.lastLeftMostCellVSpan--;
    }

    return (
      <div style={rowstyle}>
        {cells}
      </div>
    );
  }
};
