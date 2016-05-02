import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid} from 'react-virtualized';
import 'react-virtualized/styles.css';
import PivotCell from './orb.react.PivotCell';

import PivotTableComponent from './orb.react.PivotTable';

interface Props{
  pivotTableComp: PivotTableComponent,
  onScroll: any
}

export default class DataCellsComponent extends React.Component<any,{}>{


  render(){
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    return(
    <Grid
      onScroll={this.props.onScroll}
      width={500}
      height={300}
      columnWidth={100}
      rowHeight={30}
      columnsCount={pgridwidget.dataRows[0].length}
      rowsCount={pgridwidget.dataRows.length}
      renderCell={
        ({columnIndex, rowIndex}) => <PivotCell
                  key={columnIndex}
                  cell={pgridwidget.dataRows[rowIndex][columnIndex]}
                  leftmost={true}
                  topmost={true}
                  pivotTableComp={this.props.pivotTableComp} />
                  }
      />
    )
  }

  _render() {
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


    return <div className="inner-table-container data-cntr" onWheel={this.props.pivotTableComp.onWheel.bind(this.props.pivotTableComp)}>
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
