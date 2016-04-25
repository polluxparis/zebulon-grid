import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

export default class DataCellsComponent extends React.Component<any,any>{
  render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const dataCells = pgridwidget.dataRows.map((dataRow, index) => {
      return <PivotRow key={index}
                       row={dataRow}
                       axetype={AxeType.DATA}
                       layoutInfos={layoutInfos}
                       pivotTableComp={this.props.pivotTableComp}>
      </PivotRow>;
    });

    return <div className="inner-table-container data-cntr" onWheel={this.props.pivotTableComp.onWheel}>
        <table className="inner-table">
            <colgroup>
            </colgroup>
            <tbody>
              {dataCells}
            </tbody>
          </table>
      </div>;
  }
};
