import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';


import PivotTableComponent from './orb.react.PivotTable';

interface Props{
  pivotTableComp: PivotTableComponent,
  onScroll: any,
  scrollLeft: any
}

export default class ColumnHeadersComponent extends React.Component<Props,any>{
  _render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const columnHeaders = pgridwidget.columns.headers.map((headerRow, index) => {
      return <PivotRow
        onScroll={this.props.onScroll}
        scrollLeft={this.props.scrollLeft}
        key={index}
        row={headerRow}
        axetype={AxeType.COLUMNS}
        pivotTableComp={this.props.pivotTableComp}
        layoutInfos={layoutInfos}>
      </PivotRow>;
    });

    return  <div className={'inner-table-container' + cntrClass}>
          {columnHeaders}
    </div>;
  }
  render() {
    console.log('render columnHeaders');
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const config = pgridwidget.pgrid.config;
    const rowHeight = 30;
    const cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';
    // need to find how to represent the cells correctly using renderCell

    const leafsHeadersCount = pgridwidget.columns.leafsHeaders.length;
    const columnHeaders = pgridwidget.columns.headers.map((headerRow, index) =>{
      const columnsCount = headerRow.length;
      const columnWidth = (leafsHeadersCount/columnsCount)*100;
      return <Grid
            key={index}
            onScroll={this.props.onScroll}
            scrollLeft={this.props.scrollLeft}
            height={rowHeight}
            width={config.width - 100}
            rowHeight={rowHeight}
            columnWidth={columnWidth}
            columnsCount={columnsCount}
            rowsCount={1}
            renderCell={
              ({columnIndex, rowIndex}) => <PivotCell
                        key={columnIndex}
                        cell={pgridwidget.columns.headers[index][columnIndex]}
                        leftmost={false}
                        topmost={false}
                        pivotTableComp={this.props.pivotTableComp} />
                        }
            />
          });

    return (
        <div className={'inner-table-container' + cntrClass} >
        {columnHeaders}
        </div>
      )
  };
};
