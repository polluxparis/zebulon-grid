import * as React from 'react';
import * as ReactDOM from 'react-dom';
import {Grid, AutoSizer} from 'react-virtualized';

import CellSizeAndPositionManager from './utils/CellSizeAndPositionManager';

import{Header, DataCell, DataHeader} from '../orb.ui.header';
import {PivotHeaderCell, PivotDataCell} from './orb.react.PivotCells';

export class OrbGrid extends React.Component<any,{}>{

  private _grid;
  private _width;
  private _height;
  private _cellHeight;
  private _cellWidth;
  private _rowVerticalCount;
  private _rowHorizontalCount;
  private _columnVerticalCount;
  private _columnHorizontalCount;
  private _rowHeadersWidth;
  private _columnHeadersHeight;

  private _columnHeaders;
  private _rowHeaders;

  constructor(props){
    super(props);
    this.initLayoutInfos(props);

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this);
    this.dataCellRenderer = this.dataCellRenderer.bind(this);
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this);
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this);
  }

  componentWillUpdate(nextProps, nextState){
    this.initLayoutInfos(nextProps);
  }

  initLayoutInfos(props){
    const {pgridwidgetstore} = props;

    this._cellHeight = pgridwidgetstore.layout.cell.height;
    this._cellWidth = pgridwidgetstore.layout.cell.width;

    this._rowVerticalCount = pgridwidgetstore.layout.rowHeaders.height;
    this._rowHorizontalCount = pgridwidgetstore.layout.rowHeaders.width;
    this._columnVerticalCount = pgridwidgetstore.layout.columnHeaders.height;
    this._columnHorizontalCount = pgridwidgetstore.layout.columnHeaders.width;

    this._rowHeadersWidth = this._rowHorizontalCount*this._cellWidth;
    this._columnHeadersHeight = this._columnVerticalCount*this._cellHeight;

    this._width = Math.min(pgridwidgetstore.pgrid.config.width, this._rowHeadersWidth + this._columnHorizontalCount*this._cellWidth);
    this._height = Math.min(pgridwidgetstore.pgrid.config.height, this._columnHeadersHeight + this._rowVerticalCount*this._cellHeight);
  }

  render(){
    console.log('rendering grid');
    // if (this._grid) {this._grid.forceUpdate()};

    this._columnHeaders = this.props.pgridwidgetstore.columns.headers;
    this._rowHeaders = this.props.pgridwidgetstore.rows.headers;

    return (
          <Grid
            ref={ref => this._grid = ref}
            width={this._width}
            height={this._height}
            columnWidth={this._cellWidth}
            rowHeight={this._cellHeight}
            columnCount={this._columnHorizontalCount+this._rowHorizontalCount}
            rowCount={this._columnVerticalCount+this._rowVerticalCount}
            cellRenderer={()=>null}
            cellRangeRenderer={this.cellRangeRenderer}
            overscanRowCount={0}
            overscanColumnCount={0}
          />
    )
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
      rowStopIndex,
      scrollLeft,
      scrollTop
    }) {
    const renderedCells = [];

    // to avoid rendering empty cells
    // there is a difference between columnCount (the prop of the Grid object) and the column count except the row headers
    // the -1 is here because there are inferior or equal signs in the loops
    const _columnStopIndex = Math.min(columnStopIndex,this._columnHorizontalCount - 1)
    const _rowStopIndex = Math.min(rowStopIndex,this._rowVerticalCount - 1)

    // Top-left corner piece
    renderedCells.push(
      <div
        key='fixed-fixed'
        className={'Grid__cell'}
        style={{
          position: 'fixed',
          left: scrollLeft,
          top: scrollTop,
          width: this._rowHeadersWidth,
          height: this._columnHeadersHeight,
          zIndex: 2,
          backgroundColor: '#fff'
        }}
      >
        &nbsp;
      </div>
    )


    // Render fixed header rows
    for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
      for (let columnHeaderIndex = 0; columnHeaderIndex < this._columnHeaders[columnIndex].length; columnHeaderIndex++){
        let renderedCell = this.columnHeaderRenderer({
          rowIndex: columnIndex,
          columnIndex: columnHeaderIndex
        });
        let columnHeader = this._columnHeaders[columnIndex][columnHeaderIndex];
        renderedCells.push(
          <div
            key={`fixedrow-${columnHeaderIndex}-${columnIndex}`}
            className={'Grid__cell'}
            style={{
              position: 'fixed',
              left:columnIndex*this._cellWidth+this._rowHeadersWidth,
              top:(this._columnVerticalCount - this._columnHeaders[columnIndex].length + columnHeaderIndex)*this._cellHeight + scrollTop,
              height:this._cellHeight*columnHeader.vspan(),
              width:this._cellWidth*(columnHeader.subheaders.length || 1),
              zIndex: 1,
              backgroundColor: '#eef8fb'
            }}
          >
          {renderedCell}
          </div>
          )
        }
      }

    // Render fixed left columns
    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      for (let rowHeaderIndex = 0; rowHeaderIndex < this._rowHeaders[rowIndex].length; rowHeaderIndex++){
        let renderedCell = this.rowHeaderRenderer({
            columnIndex: rowHeaderIndex,
            rowIndex
          });
        let rowHeader = this._rowHeaders[rowIndex][rowHeaderIndex];
        renderedCells.push(
          <div
            key={`fixedcol-${rowHeaderIndex}-${rowIndex}`}
            className={'Grid__cell'}
            style={{
              position: 'fixed',
              left:(this._rowHorizontalCount - this._rowHeaders[rowIndex].length+rowHeaderIndex)*this._cellWidth + scrollLeft,
              top:rowIndex*this._cellHeight + this._columnHeadersHeight,
              height:this._cellHeight*(rowHeader.subheaders.length || 1),
              width:this._cellWidth*rowHeader.hspan(),
              zIndex: 1,
              backgroundColor: '#eef8fb'
            }}
          >
            {renderedCell}
          </div>
        )
      }
    }

    // Render data cells
    if (!isScrolling){
      for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
        let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)

        for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
          let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
          let key = `${rowIndex}-${columnIndex}`
          let renderedCell = this.dataCellRenderer({
              columnIndex,
              isScrolling,
              rowIndex
            })

       let child = (
         <div
           key={key}
           className='Grid__cell'
           style={{
             height: this._cellHeight,
             width: this._cellWidth,
             left: columnDatum.offset+this._rowHeadersWidth,
             top: rowDatum.offset+this._columnHeadersHeight
           }}
         >
           {renderedCell}
         </div>
       )
          renderedCells.push(child)
        }
      }
    }

    return renderedCells
  }

  dataCellRenderer({columnIndex, rowIndex, isScrolling}):string|JSX.Element {
    const rowHeaderRow = this.props.pgridwidgetstore.rows.headers[rowIndex];
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1];
    const columnHeaderColumn = this.props.pgridwidgetstore.columns.headers[columnIndex];
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1];
    const cell = new DataCell(this.props.pgridwidgetstore.pgrid,() => (rowHeader as DataHeader).visible() && columnHeader.visible(), rowHeader, columnHeader);
    cell.value = this.props.pgridwidgetstore.pgrid.getData(
      (cell as DataCell).datafield ? (cell as DataCell).datafield.name : null,
      (cell as DataCell).rowDimension,
      (cell as DataCell).columnDimension);
    return <PivotDataCell
            key={columnIndex}
            cell={cell}
            pgridwidgetstore={this.props.pgridwidgetstore}
            />
  }

  columnHeaderRenderer ({columnIndex, rowIndex}){
    const cell = this.props.pgridwidgetstore.columns.headers[rowIndex][columnIndex];
    if (!cell){
      return null;
    }
    else {
      return <PivotHeaderCell
                  cell={cell}
                  pgridwidgetstore={this.props.pgridwidgetstore} />
    }
  }

  rowHeaderRenderer({columnIndex, rowIndex}){
    const cell = this.props.pgridwidgetstore.rows.headers[rowIndex][columnIndex];
    if (!cell){
      return null;
    }
    else {
      return <PivotHeaderCell
                cell={cell}
                pgridwidgetstore={this.props.pgridwidgetstore} />
    }
  }
}
