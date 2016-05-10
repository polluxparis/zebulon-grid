import * as React from 'react';
import * as ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow';
import {AxeType} from '../orb.axe';

import {Grid} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';

interface Props{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollTop: any
}


export default class RowHeadersComponent extends React.Component<Props,any>{
  constructor(){
    super();
  }

  setColGroup(widths) {
    const myNode = ReactDOM.findDOMNode(this);
    const colGroupNode = this.refs['colgroup'];
    myNode['style'].tableLayout = 'auto';

    colGroupNode['innerHTML'] = '';
    for(let i = 0; i < widths.length; i++) {
      const col = document.createElement('col');
      col.style.width = `${widths[i]}${8}px`;
      colGroupNode['appendChild'](col);
    }
    myNode['style'].tableLayout = 'fixed';
  }
  render(){
    // console.log('render rowHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const columnWidth = 100;
    const cntrClass = pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';


    const leafsHeadersCount = pgridwidgetstore.rows.headers[pgridwidgetstore.rows.headers.length - 1].length;
    const rowHeaders = pgridwidgetstore.rows.headers.map((headerColumn, index)=>{
      const rowCount = headerColumn.length;
      const rowHeight = (leafsHeadersCount/rowCount)*30;

      return <Grid
              key={index}
              onScroll={this.props.onScroll}
              scrollTop={this.props.scrollTop}
              width={columnWidth}
              height={config.height - 60}
              columnWidth={columnWidth}
              rowHeight={rowHeight}
              columnCount={1}
              rowCount={rowCount}
              cellRenderer={
                ({columnIndex, rowIndex}) => <PivotCell
                          key={rowIndex}
                          cell={pgridwidgetstore.rows.headers[index][rowIndex]}
                          leftmost={false}
                          topmost={false}
                          pgridwidgetstore={this.props.pgridwidgetstore} />
                          }
              />
            })
      return <div className={'inner-table-container' + cntrClass} style={{display: 'flex'}}>
            {rowHeaders}
      </div>

  }

  _render(){
    console.log('render rowHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const columnWidth = 100;
    const cntrClass = pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    return <Grid
              onScroll={this.props.onScroll}
              scrollTop={this.props.scrollTop}
              width={columnWidth}
              height={config.height - 60}
              columnWidth={columnWidth}
              rowHeight={30}
              columnCount={1}
              rowCount={pgridwidgetstore.rows.headers.length}
              cellRenderer={
                ({columnIndex, rowIndex}) => <PivotRow
                          key={columnIndex}
                          row={pgridwidgetstore.rows.headers[rowIndex]}
                          layoutInfos={layoutInfos}
                          axetype={AxeType.ROWS}
                          pgridwidgetstore={this.props.pgridwidgetstore} />
                          }
              />
  }

  __render() {
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const cntrClass = pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const rowHeaders = pgridwidgetstore.rows.headers.map((headerRow, index) => {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={AxeType.ROWS}
                       layoutInfos={layoutInfos}
                       pgridwidgetstore={this.props.pgridwidgetstore}>
      </PivotRow>;
    });

    return  <div className={ 'inner-table-container' + cntrClass } >
      <table className="inner-table">
        <colgroup ref="colgroup">
        </colgroup>
        <tbody>
          {rowHeaders}
        </tbody>
      </table>
    </div>;
  }
};
