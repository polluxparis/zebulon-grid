import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

export default class ColumnHeadersComponent extends React.Component<any,any>{
  render() {
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

    return  <div className={'inner-table-container' + cntrClass} onWheel={this.props.pivotTableComp.onWheel}>
      <table className="inner-table">
        <colgroup>
        </colgroup>
        <tbody>
          {columnHeaders}
        </tbody>
      </table>
    </div>;
  }
};
