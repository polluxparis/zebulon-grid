import React from 'react';
import axe from '../orb.axe';
import PivotRow from './orb.react.PivotRow.jsx';

export default React.createClass({
  render() {
    const self = this;
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const dataCells = pgridwidget.dataRows.map((dataRow, index) => {
      return <PivotRow key={index}
                       row={dataRow}
                       axetype={axe.Type.DATA}
                       layoutInfos={layoutInfos}
                       pivotTableComp={self.props.pivotTableComp}>
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
});