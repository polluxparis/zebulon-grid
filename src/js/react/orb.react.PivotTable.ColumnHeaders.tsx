import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';

interface Props{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollLeft: any
}

export default class ColumnHeadersComponent extends React.Component<Props,any>{

  render() {
    // console.log('render columnHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const rowHeight = 30;
    const cntrClass = pgridwidgetstore.columns.headers.length === 0 ? '' : ' columns-cntr';
    // need to find how to represent the cells correctly using renderCell

    const leafsHeadersCount = pgridwidgetstore.columns.leafsHeaders.length;
    const columnHeaders = pgridwidgetstore.columns.headers.map((headerRow, index) =>{
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
            renderCell={this.renderCell(index)}
            />
          });

    return (
        <div className={'inner-table-container' + cntrClass} >
        {columnHeaders}
        </div>
      )
  };

  private renderCell(index: number){

    return ({columnIndex, rowIndex}) => <PivotCell
                  key={columnIndex}
                  cell={this.props.pgridwidgetstore.columns.headers[index][columnIndex]}
                  leftmost={false}
                  topmost={false}
                  pgridwidgetstore={this.props.pgridwidgetstore} />
  }


};
