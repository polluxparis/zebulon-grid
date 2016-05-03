import * as React from 'react';
import * as ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow';
import {AxeType} from '../orb.axe';

import {Grid} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import PivotTableComponent from './orb.react.PivotTable';

interface Props{
  pivotTableComp: PivotTableComponent,
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
    console.log('render rowHeaders');
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const config = pgridwidget.pgrid.config;
    const columnWidth = 100;
    const cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';


    const leafsHeadersCount = pgridwidget.rows.headers[pgridwidget.rows.headers.length - 1].length;
    const rowHeaders = pgridwidget.rows.headers.map((headerColumn, index)=>{
      const rowsCount = headerColumn.length;
      const rowHeight = (leafsHeadersCount/rowsCount)*30;

      return <Grid
              key={index}
              onScroll={this.props.onScroll}
              scrollTop={this.props.scrollTop}
              width={columnWidth}
              height={config.height - 60}
              columnWidth={columnWidth}
              rowHeight={rowHeight}
              columnsCount={1}
              rowsCount={rowsCount}
              renderCell={
                ({columnIndex, rowIndex}) => <PivotCell
                          key={rowIndex}
                          cell={pgridwidget.rows.headers[index][rowIndex]}
                          leftmost={false}
                          topmost={false}
                          pivotTableComp={this.props.pivotTableComp} />
                          }
              />
            })
      return <div className={'inner-table-container' + cntrClass} style={{display: 'flex'}}>
            {rowHeaders}
      </div>

  }

  _render(){
    console.log('render rowHeaders');
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const config = pgridwidget.pgrid.config;
    const columnWidth = 100;
    const cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';

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
              columnsCount={1}
              rowsCount={pgridwidget.rows.headers.length}
              renderCell={
                ({columnIndex, rowIndex}) => <PivotRow
                          key={columnIndex}
                          row={pgridwidget.rows.headers[rowIndex]}
                          layoutInfos={layoutInfos}
                          axetype={AxeType.ROWS}
                          pivotTableComp={this.props.pivotTableComp} />
                          }
              />
  }

  __render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const rowHeaders = pgridwidget.rows.headers.map((headerRow, index) => {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={AxeType.ROWS}
                       layoutInfos={layoutInfos}
                       pivotTableComp={this.props.pivotTableComp}>
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
