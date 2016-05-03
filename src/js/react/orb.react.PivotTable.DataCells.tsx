import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid} from 'react-virtualized';
import 'react-virtualized/styles.css';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';

interface Props{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollLeft: any,
  scrollTop: any
}

export default class DataCellsComponent extends React.Component<Props,{}>{


  render(){
    console.log('render dataCells');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const columnsCount = pgridwidgetstore.dataRows[0].length;

    return(
    <Grid
      onScroll={this.props.onScroll}
      scrollLeft={this.props.scrollLeft}
      scrollTop={this.props.scrollTop}
      width={config.width-100}
      height={config.height-60}
      columnWidth={100}
      rowHeight={30}
      columnsCount={columnsCount}
      rowsCount={pgridwidgetstore.dataRows.length}
      renderCell={
        ({columnIndex, rowIndex}) => <PivotCell
                  key={columnIndex}
                  cell={pgridwidgetstore.dataRows[rowIndex][columnIndex]}
                  leftmost={true}
                  topmost={true}
                  pgridwidgetstore={this.props.pgridwidgetstore} />
                  }
      />
    )
  }

  _render() {
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const dataCells = pgridwidgetstore.dataRows.map((dataRow, index) => {
      return <PivotRow key={index}
                       row={dataRow}
                       axetype={AxeType.DATA}
                       layoutInfos={layoutInfos}
                       pgridwidgetstore={this.props.pgridwidgetstore}>
      </PivotRow>;
    });


    return <div className="inner-table-container data-cntr" >
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
