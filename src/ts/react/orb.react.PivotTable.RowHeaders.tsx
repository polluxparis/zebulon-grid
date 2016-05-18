import * as React from 'react';
import * as ReactDOM from 'react-dom';
import {AxeType} from '../orb.axe';

import {Collection, AutoSizer, Grid} from 'react-virtualized';
import CellSizeAndPositionManager from './utils/CellSizeAndPositionManager';
import {PivotHeaderCell} from './orb.react.PivotCell';

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';
import{Header} from '../orb.ui.header';

import {scrollbarSize} from '../orb.utils.dom';

export interface RowHeadersProps{
  pgridwidgetstore: PGridWidgetStore,
  onScroll: number,
  scrollTop: number
}

export default class RowHeadersComponent extends React.Component<RowHeadersProps,any>{

  constructor(){
    super();
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this);
    this.cellRangeRenderer = this.cellRangeRenderer.bind(this);
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
    const cntrClass = this.props.pgridwidgetstore.rows.headers.length === 0 ? '' : ' rows-cntr';

    const rowHeaders =
      <AutoSizer>
        {({height, width})=>(
          <Grid
              onScroll={this.props.onScroll}
              scrollTop={this.props.scrollTop}
              height={height}
              width={width}
              columnWidth={this.props.pgridwidgetstore.layout.cell.width}
              rowHeight={this.props.pgridwidgetstore.layout.cell.height}
              columnCount={this.props.pgridwidgetstore.rows.axe.dimensionsCount}
              rowCount={this.props.pgridwidgetstore.rows.headers.length}
              cellRangeRenderer={this.cellRangeRenderer}
              cellRenderer={this.rowHeaderRenderer}
              />
            )}
        </AutoSizer>
        ;

      return <div className={'inner-table-container' + cntrClass} style={{width:'100%', height:'100%'}}>
            {rowHeaders}
      </div>

  }

  cellRangeRenderer ({
      cellCache,
      cellRenderer,
      columnSizeAndPositionManager,
      columnStartIndex,
      columnStopIndex,
      isScrolling,
      rowSizeAndPositionManager,
      rowStartIndex,
      rowStopIndex
    }) {
    const renderedCells = []

    const headers = this.props.pgridwidgetstore.rows.headers;
    const cellHeight = this.props.pgridwidgetstore.layout.cell.height;
    const cellWidth = this.props.pgridwidgetstore.layout.cell.width;
    const colNb = headers[0].length;

    for (let rowIndex = rowStartIndex; rowIndex <= rowStopIndex; rowIndex++) {
      // let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)

      for (let columnIndex = columnStartIndex; columnIndex <= columnStopIndex; columnIndex++) {
        // let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
        let key = `${rowIndex}-${columnIndex}`
        let renderedCell

        // Avoid re-creating cells while scrolling.
        // This can lead to the same cell being created many times and can cause performance issues for "heavy" cells.
        // If a scroll is in progress- cache and reuse cells.
        // This cache will be thrown away once scrolling complets.
        if (isScrolling) {
          if (!cellCache[key]) {
            cellCache[key] = cellRenderer({
              columnIndex,
              isScrolling,
              rowIndex
            })
          }
          renderedCell = cellCache[key]
        // If the user is no longer scrolling, don't cache cells.
        // This makes dynamic cell content difficult for users and would also lead to a heavier memory footprint.
        } else {
          renderedCell = cellRenderer({
            columnIndex,
            isScrolling,
            rowIndex
          })
        }

        if (renderedCell == null || renderedCell === false) {
          continue
        }
        let child = (
          <div
            key={key}
            className='Grid__cell'
            style={{
              left:(colNb - headers[rowIndex].length+columnIndex)*cellWidth,
              top:rowIndex*cellHeight,
              height:cellHeight*headers[rowIndex][columnIndex].vspan(),
              width:cellWidth*headers[rowIndex][columnIndex].hspan()
            }}
          >
            {renderedCell}
          </div>
        )

        renderedCells.push(child)
      }
    }

    return renderedCells
  }

  mockRowHeaderRenderer({columnIndex, rowIndex}){
    return `C: ${columnIndex} R: ${rowIndex}`;
  }
  rowHeaderRenderer({columnIndex, rowIndex}){
    const cell = this.props.pgridwidgetstore.rows.headers[rowIndex][columnIndex];
    if (!cell){
      return null;
    }
    else {
      return <PivotHeaderCell
                key={rowIndex-columnIndex}
                cell={cell}
                leftmost={false}
                topmost={false}
                pgridwidgetstore={this.props.pgridwidgetstore} />
    }
  }

};
