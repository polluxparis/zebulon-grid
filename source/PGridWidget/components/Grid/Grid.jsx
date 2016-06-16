import React, { Component } from 'react'
import { Grid } from 'react-virtualized'

import HeaderCellComp from '../HeaderCell'
import DataCellComp from '../DataCell'
import { DataCell } from '../../Cells'

export default class OrbGrid extends Component {

  constructor (props) {
    super(props)
    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this)
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this)
  }

  initLayoutInfos (layout, sizes) {
    this._cellHeight = sizes.cell.height
    this._cellWidth = sizes.cell.width

    this._rowVerticalCount = layout.rowHeaders.height
    this._rowHorizontalCount = layout.rowHeaders.width
    this._columnVerticalCount = layout.columnHeaders.height
    this._columnHorizontalCount = layout.columnHeaders.width

    this._rowHeadersWidth = this._rowHorizontalCount * this._cellWidth
    this._columnHeadersHeight = this._columnVerticalCount * this._cellHeight

    this._width = Math.min(sizes.grid.width, this._rowHeadersWidth + this._columnHorizontalCount * this._cellWidth)
    this._height = Math.min(sizes.grid.height, this._columnHeadersHeight + this._rowVerticalCount * this._cellHeight)
  }

  render () {
    console.log('rendering grid')
    const {store} = this.props
    const {rowsUi, columnsUi, layout, sizes} = store

    this.initLayoutInfos(layout, sizes)

    this._columnHeaders = columnsUi.headers
    this._rowHeaders = rowsUi.headers

    return (
      <Grid
        ref={ref => { this._grid = ref }}
        width={this._width}
        height={this._height}
        columnWidth={this._cellWidth}
        rowHeight={this._cellHeight}
        columnCount={this._columnHorizontalCount + this._rowHorizontalCount}
        rowCount={this._columnVerticalCount + this._rowVerticalCount}
        cellRenderer={this._mockCellRenderer}
        cellRangeRenderer={this.cellRangeRenderer}
        overscanRowCount={0}
        overscanColumnCount={0} />
    )
  }

  cellRangeRenderer ({columnSizeAndPositionManager, columnStartIndex, columnStopIndex, isScrolling, rowSizeAndPositionManager, rowStartIndex, rowStopIndex, scrollLeft, scrollTop}) {
    const renderedCells = []

    // to avoid rendering empty cells
    // there is a difference between columnCount (the prop of the Grid object) and the column count except the row headers
    // the -1 is here because there are inferior or equal signs in the loops
    const _columnStopIndex = Math.min(columnStopIndex - this._rowHorizontalCount, this._columnHorizontalCount - 1)
    const _rowStopIndex = Math.min(rowStopIndex - this._columnVerticalCount, this._rowVerticalCount - 1)

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
          backgroundColor: '#fff'}}>
      </div>
    )

    // Render fixed header rows
    for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
      for (let columnHeaderIndex = 0; columnHeaderIndex < this._columnHeaders[columnIndex].length; columnHeaderIndex++) {
        let renderedCell = this.columnHeaderRenderer({
          rowIndex: columnIndex,
          columnIndex: columnHeaderIndex
        })
        let columnHeader = this._columnHeaders[columnIndex][columnHeaderIndex]
        renderedCells.push(
          <div
            key={`fixedrow-${columnHeaderIndex}-${columnIndex}`}
            className={'Grid__cell'}
            style={{
              position: 'fixed',
              left: columnIndex * this._cellWidth + this._rowHeadersWidth,
              top: (this._columnVerticalCount - this._columnHeaders[columnIndex].length + columnHeaderIndex) * this._cellHeight + scrollTop,
              height: this._cellHeight * columnHeader.vspan(),
              width: this._cellWidth * columnHeader.hspan(),
              zIndex: 1,
              backgroundColor: '#eef8fb'
            }}>
            {renderedCell}
          </div>
        )
      }
    }

    // Render fixed left columns
    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      for (let rowHeaderIndex = 0; rowHeaderIndex < this._rowHeaders[rowIndex].length; rowHeaderIndex++) {
        let renderedCell = this.rowHeaderRenderer({
          columnIndex: rowHeaderIndex,
          rowIndex})
        let rowHeader = this._rowHeaders[rowIndex][rowHeaderIndex]
        renderedCells.push(
          <div
            key={`fixedcol-${rowHeaderIndex}-${rowIndex}`}
            className={'Grid__cell'}
            style={{
              position: 'fixed',
              left: (this._rowHorizontalCount - this._rowHeaders[rowIndex].length + rowHeaderIndex) * this._cellWidth + scrollLeft,
              top: rowIndex * this._cellHeight + this._columnHeadersHeight,
              height: this._cellHeight * rowHeader.vspan(),
              width: this._cellWidth * rowHeader.hspan(),
              zIndex: 1,
              backgroundColor: '#eef8fb'
            }}>
            {renderedCell}
          </div>
        )
      }
    }

    // Render data cells
    if (!isScrolling) {
      for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
        let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)

        for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
          let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
          let key = `${rowIndex}-${columnIndex}`
          let renderedCell = this.dataCellRenderer({
            columnIndex,
            isScrolling,
            rowIndex})

          let child = (
            <div
              key={key}
              className='Grid__cell'
              style={{
                height: this._cellHeight,
                width: this._cellWidth,
                left: columnDatum.offset + this._rowHeadersWidth,
                top: rowDatum.offset + this._columnHeadersHeight
              }}>
              {renderedCell}
            </div>
          )
          renderedCells.push(child)
        }
      }
    }

    return renderedCells
  }

  dataCellRenderer ({columnIndex, rowIndex, isScrolling}) {
    const {store} = this.props
    const {rowsUi, columnsUi} = store
    const rowHeaderRow = rowsUi.headers[rowIndex]
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1]
    const columnHeaderColumn = columnsUi.headers[columnIndex]
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1]
    const cell = new DataCell(
      store,
      () => rowHeader.visible() && columnHeader.visible(),
      rowHeader,
      columnHeader
    )
    return <DataCellComp key={`data-${rowIndex}-${columnIndex}`} cell={cell} onDoubleClick={() => store.drilldown(cell)} />
  }

  columnHeaderRenderer ({columnIndex, rowIndex}) {
    const cell = this.props.store.columnsUi.headers[rowIndex][columnIndex]
    if (!cell) {
      return null
    } else {
      return <HeaderCellComp key={`column-${rowIndex}-${columnIndex}`} cell={cell} onToggle={() => 33} />
    }
  }

  rowHeaderRenderer ({columnIndex, rowIndex}) {
    const cell = this.props.store.rowsUi.headers[rowIndex][columnIndex]
    if (!cell) {
      return null
    } else {
      return <HeaderCellComp key={`row-${rowIndex}-${columnIndex}`} cell={cell} onToggle={() => 33} />
    }
  }

  _mockCellRenderer ({columnIndex, rowIndex}) {
    return 33
  }
}
