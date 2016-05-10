import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotRow from './orb.react.PivotRow';

import {Grid, Collection} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';

interface Props{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollLeft: any
}

export default class ColumnHeadersComponent extends React.Component<Props,any>{

  private headersConcat;

  constructor(){
    super();
    this.layoutGetter = this.layoutGetter.bind(this);
  }

  render() {
    // console.log('render columnHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const rowHeight = 30;
    const cntrClass = pgridwidgetstore.columns.headers.length === 0 ? '' : ' columns-cntr';
    // need to find how to represent the cells correctly using cellRenderer

    const leafsHeadersCount = pgridwidgetstore.columns.leafsHeaders.length;
    const rowNb = pgridwidgetstore.columns.headers.length;
    pgridwidgetstore.columns.headers.map((headerCol,colIndex)=>
      headerCol.map((header,rowIndex) => Object.assign(header,{x:rowIndex, y: colIndex})));
    this.headersConcat = [].concat(...pgridwidgetstore.columns.headers);
    const cellCount = this.headersConcat.length;

    const cellHeight = this.props.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.props.pgridwidgetstore.layout.cell.width;
    const colVerticalCount = this.props.pgridwidgetstore.layout.columnHeaders.height;
    const rowHorizontalCount = this.props.pgridwidgetstore.layout.rowHeaders.width;

    // const columnWidth = (leafsHeadersCount/columnCount)*100;
    const columnHeaders =
      <Collection
        onScroll={this.props.onScroll}
        scrollLeft={this.props.scrollLeft}
        height={colVerticalCount*cellHeight}
        width={config.width - rowHorizontalCount*cellWidth}
        cellCount={cellCount}
        cellRenderer={({index})=><PivotCell
                      key={index}
                      cell={this.headersConcat[index]}
                      leftmost={false}
                      topmost={false}
                      pgridwidgetstore={this.props.pgridwidgetstore} />}
        cellSizeAndPositionGetter={this.layoutGetter}
        />

    return (
        <div className={'inner-table-container' + cntrClass} >
        {columnHeaders}
        </div>
      )
  };

  private layoutGetter({index}){
    // multiplication by hspan is dirty
    // should be modifed later if need to use more complex patterns than just cells on a line having same widths
    return ({
            x:this.headersConcat[index]['x']*100*this.headersConcat[index].hspan(),
            y:this.headersConcat[index]['y']*30,
            height:30*this.headersConcat[index].vspan(),
            width:100*this.headersConcat[index].hspan()
          })
  }


};
