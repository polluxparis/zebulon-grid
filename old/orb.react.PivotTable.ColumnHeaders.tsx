import React, {Component} from 'react'
import {AxeType} from '../orb.axe';

import {Collection,AutoSizer} from 'react-virtualized';
import {PivotHeaderCell} from './orb.react.PivotCells';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';

export interface ColumnHeadersProps{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: number,
  scrollLeft: number
}

export default class ColumnHeadersComponent extends React.Component<ColumnHeadersProps,any>{

  private headersConcat;

  constructor(){
    super();
    this.layoutGetter = this.layoutGetter.bind(this);
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this);
  }

  render() {
    // console.log('render columnHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const cntrClass = pgridwidgetstore.columns.headers.length === 0 ? '' : ' columns-cntr';

    this.headersConcat = [].concat(...this.props.pgridwidgetstore.columns.headers);
    const rowNb = pgridwidgetstore.columns.headers.length;
    pgridwidgetstore.columns.headers.map((headerCol,colIndex)=>
    headerCol.map((header,rowIndex) => Object.assign(header,{x:rowIndex, y: colIndex})));

    const leafsHeadersCount = pgridwidgetstore.columns.leafsHeaders.length;
    const cellCount = this.headersConcat.length;

    const columnHeaders =
    <AutoSizer>
      {({height, width})=>(<Collection
        onScroll={this.props.onScroll}
        scrollLeft={this.props.scrollLeft}
        width={width}
        height={height}
        cellCount={cellCount}
        cellRenderer={this.columnHeaderRenderer}
        cellSizeAndPositionGetter={this.layoutGetter}
        />
      )}
      </AutoSizer>

    return (
        <div className={'inner-table-container' + cntrClass} style={{width:'100%', height:'100%'}}>
        {columnHeaders}
        </div>
      )
  };

  columnHeaderRenderer ({index}){
    return <PivotHeaderCell
                key={index}
                cell={this.headersConcat[index]}
                leftmost={false}
                topmost={false}
                pgridwidgetstore={this.props.pgridwidgetstore} />
  }

  mockColumnHeaderRenderer({index}){
    return `CH: ${index}`;
  }

  private layoutGetter({index}){
    const cellHeight = this.props.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.props.pgridwidgetstore.layout.cell.width;
    // multiplication by hspan is dirty
    // should be modifed later if need to use more complex patterns than just cells on a line having same widths
    return ({
            x:this.headersConcat[index]['x']*cellWidth*this.headersConcat[index].hspan(),
            y:this.headersConcat[index]['y']*cellHeight,
            height:cellHeight*this.headersConcat[index].vspan(),
            width:cellWidth*this.headersConcat[index].hspan()
          })
  }


};
