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

  render() {
    // console.log('render columnHeaders');
    const pgridwidgetstore = this.props.pgridwidgetstore;
    const config = pgridwidgetstore.pgrid.config;
    const rowHeight = 30;
    const cntrClass = pgridwidgetstore.columns.headers.length === 0 ? '' : ' columns-cntr';
    // need to find how to represent the cells correctly using cellRenderer

    const leafsHeadersCount = pgridwidgetstore.columns.leafsHeaders.length;
    // const columnHeaders = pgridwidgetstore.columns.headers.map((headerRow, index) =>{
      const headersConcat = [].concat(...pgridwidgetstore.columns.headers);
      const cellCount = headersConcat.length;
      // const columnWidth = (leafsHeadersCount/columnCount)*100;
      const columnHeaders = <Collection
            onScroll={this.props.onScroll}
            scrollLeft={this.props.scrollLeft}
            height={90}
            width={config.width - 100}
            cellCount={cellCount}
            cellRenderer={({index})=><PivotCell
                          key={index}
                          cell={headersConcat[index]}
                          leftmost={false}
                          topmost={false}
                          pgridwidgetstore={this.props.pgridwidgetstore} />}
                          cellSizeAndPositionGetter={({index})=>({x:index*100,y:0,height:30,width:100})}
            />
          // });

    return (
        <div className={'inner-table-container' + cntrClass} >
        {columnHeaders}
        </div>
      )
  };

  private cellRenderer(index){
    return
  }


};
