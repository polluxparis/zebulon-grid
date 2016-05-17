import * as React from 'react';
import * as ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow';
import {AxeType} from '../orb.axe';

import {Collection, AutoSizer} from 'react-virtualized';
import PivotCell from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';
import{Header} from '../orb.ui.header';

import {scrollbarSize} from '../orb.utils.dom';

export interface RowHeadersProps{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: any,
  scrollTop: any
}


export default class RowHeadersComponent extends React.Component<RowHeadersProps,any>{
  headersConcat: Header[];
  constructor(){
    super();
    this.layoutGetter = this.layoutGetter.bind(this);
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this);
  }

  componentWillMount(){
    console.log('componentWillMount');
    this.headersConcat = [].concat(...this.props.pgridwidgetstore.rows.headers);
    const colNb = this.props.pgridwidgetstore.rows.headers[0].length;
    this.props.pgridwidgetstore.rows.headers.map((headerRow,rowIndex)=>
      headerRow.map((header,colIndex) => Object.assign(header,{x:colNb - headerRow.length+colIndex, y: rowIndex}))
    );
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
    const config = this.props.pgridwidgetstore.pgrid.config;
    const cntrClass = this.props.pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';

    const cellCount = this.headersConcat.length;

    const rowHeaders =
      <AutoSizer>
        {({height, width})=>(
          <Collection
              onScroll={this.props.onScroll}
              scrollTop={this.props.scrollTop}
              height={height}
              width={width}
              cellCount={cellCount}
              cellRenderer={this.rowHeaderRenderer}
              cellSizeAndPositionGetter={this.layoutGetter}
              />
            )}
        </AutoSizer>
        ;

      return <div className={'inner-table-container' + cntrClass} style={{width:'100%', height:'100%'}}>
            {rowHeaders}
      </div>

  }

  mockRowHeaderRenderer({index}){
    return `RH: ${index}`;
  }
  rowHeaderRenderer({index}){
    return <PivotCell
                key={index}
                cell={this.headersConcat[index]}
                leftmost={false}
                topmost={false}
                pgridwidgetstore={this.props.pgridwidgetstore} />
    }

  layoutGetter({index}){
    const cellHeight = this.props.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.props.pgridwidgetstore.layout.cell.width;

    return ({
            x:this.headersConcat[index]['x']*cellWidth,
            y:this.headersConcat[index]['y']*cellHeight,
            height:cellHeight*this.headersConcat[index].vspan(),
            width:cellWidth*this.headersConcat[index].hspan()
          })
  }

};
