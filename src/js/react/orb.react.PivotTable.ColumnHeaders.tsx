import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

export default class ColumnHeadersComponent extends React.Component<any,any>{
  _render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const columnHeaders = pgridwidget.columns.headers.map((headerRow, index) => {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={AxeType.COLUMNS}
                       pivotTableComp={this.props.pivotTableComp}
                       layoutInfos={layoutInfos}>
      </PivotRow>;
    });

    return  <div className={'inner-table-container' + cntrClass} onWheel={this.props.pivotTableComp.onWheel.bind(this.props.pivotTableComp)}>
      <table className="inner-table">
        <colgroup>
        </colgroup>
        <tbody>
          {columnHeaders}
        </tbody>
      </table>
    </div>;
  }
  render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';
    // need to find how to represent the cells correctly using renderCell
    return (
      <div className={'inner-table-container' + cntrClass} >
        <Grid
              scrollLeft={this.props.scrollLeft}
              width={500}
              height={40}
              columnWidth={100}
              rowHeight={30}
              columnsCount={pgridwidget.columns.leafsHeaders.length}
              rowsCount={2}
              renderCell={
                ({columnIndex, rowIndex}) => <PivotCell
                          key={rowIndex}
                          cell={pgridwidget.columns.headers[rowIndex][0]}
                          leftmost={false}
                          topmost={false}
                          pivotTableComp={this.props.pivotTableComp} />
                          }
              />
        </div>
      )
  };
};
