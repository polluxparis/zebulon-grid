import * as React from 'react';
import * as ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow';
import {AxeType} from '../orb.axe';

import {Grid, Collection} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';
import{Header} from '../orb.ui.header';

interface Props{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollTop: any
}


export default class RowHeadersComponent extends React.Component<Props,any>{
  headersConcat: Header[];
  constructor(){
    super();
    this.layoutGetter = this.layoutGetter.bind(this);
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
    const cntrClass = pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';

    const colNb = pgridwidgetstore.rows.headers[0].length;
    pgridwidgetstore.rows.headers.map((headerRow,rowIndex)=>
      headerRow.map((header,colIndex) => Object.assign(header,{x:colNb - headerRow.length+colIndex, y: rowIndex})));
    this.headersConcat = [].concat(...pgridwidgetstore.rows.headers);
    const cellCount = this.headersConcat.length;

    const cellHeight = this.props.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.props.pgridwidgetstore.layout.cell.width;

    const colVerticalCount = this.props.pgridwidgetstore.layout.columnHeaders.height;
    const rowHorizontalCount = this.props.pgridwidgetstore.layout.rowHeaders.width;

    const rowHeaders =
      <Collection
          onScroll={this.props.onScroll}
          scrollTop={this.props.scrollTop}
          height={config.height - colVerticalCount*cellHeight}
          width={rowHorizontalCount*cellWidth}
          cellCount={cellCount}
          cellRenderer={({index})=>
            <PivotCell
                        key={index}
                        cell={this.headersConcat[index]}
                        leftmost={false}
                        topmost={false}
                        pgridwidgetstore={this.props.pgridwidgetstore} />
          }
          cellSizeAndPositionGetter={this.layoutGetter}
          />
        ;

      return <div className={'inner-table-container' + cntrClass} style={{display: 'flex'}}>
            {rowHeaders}
      </div>

  }

  layoutGetter({index}){
    return ({
            x:this.headersConcat[index]['x']*100,
            y:this.headersConcat[index]['y']*30,
            height:30*this.headersConcat[index].vspan(),
            width:100*this.headersConcat[index].hspan()
          })
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
