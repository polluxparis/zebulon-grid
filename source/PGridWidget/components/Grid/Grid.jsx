import { Component } from 'react'
import { Grid } from 'react-virtualized'

import { HeaderCellComp, DataCellComp } from '../PivotCells'
import {DataCell} from '../../Cells'

export default class OrbGrid extends Component {

  constructor ({columns, rows, layout, dataStore}) {
    super({columns, rows, layout, dataStore})
    this.initLayoutInfos({layout})

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this)
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this)
  }

  componentWillUpdate (nextProps, nextState) {
    const {layout} = nextProps
    this.initLayoutInfos(layout)
  }

  initLayoutInfos (layout) {
    this._cellHeight = layout.cell.height
    this._cellWidth = layout.cell.width

    this._rowVerticalCount = layout.rowHeaders.height
    this._rowHorizontalCount = layout.rowHeaders.width
    this._columnVerticalCount = layout.columnHeaders.height
    this._columnHorizontalCount = layout.columnHeaders.width

    this._rowHeadersWidth = this._rowHorizontalCount * this._cellWidth
    this._columnHeadersHeight = this._columnVerticalCount * this._cellHeight

    this._width = Math.min(layout.pivotTable.width, this._rowHeadersWidth + this._columnHorizontalCount * this._cellWidth)
    this._height = Math.min(layout.pivotTable.height, this._columnHeadersHeight + this._rowVerticalCount * this._cellHeight)
  }

  render () {
    console.log('rendering grid')
    const {columns, rows} = this.props

    this._columnHeaders = columns.headers
    this._rowHeaders = rows.headers

    return (
      <Grid
        ref={ref => {
          this._grid = ref
        }}
        width={this._width}
        height={this._height}
        columnWidth={this._cellWidth}
        rowHeight={this._cellHeight}
        columnCount={this._columnHorizontalCount + this._rowHorizontalCount}
        rowCount={this._columnVerticalCount + this._rowVerticalCount}
        cellRangeRenderer={this.cellRangeRenderer}
        overscanRowCount={0}
        overscanColumnCount={0} />
      )
  }

  cellRangeRenderer ({cellCache, cellRenderer, columnSizeAndPositionManager, columnStartIndex, columnStopIndex, isScrolling, rowSizeAndPositionManager, rowStartIndex, rowStopIndex, scrollLeft, scrollTop}) {
    const renderedCells = []

    // to avoid rendering empty cells
    // there is a difference between columnCount (the prop of the Grid object) and the column count except the row headers
    // the -1 is here because there are inferior or equal signs in the loops
    const _columnStopIndex = Math.min(columnStopIndex, this._columnHorizontalCount - 1)
    const _rowStopIndex = Math.min(rowStopIndex, this._rowVerticalCount - 1)

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
        &nbsp
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
              width: this._cellWidth * (columnHeader.subheaders.length || 1),
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
              height: this._cellHeight * (rowHeader.subheaders.length || 1),
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
    const {rows, columns, dataStore} = this.props
    const rowHeaderRow = rows.headers[rowIndex]
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1]
    const columnHeaderColumn = columns.headers[columnIndex]
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1]
    const cell = new DataCell(
      dataStore,
      () => rowHeader.visible() && columnHeader.visible(),
      rowHeader,
      columnHeader
    )
    cell.value = dataStore.getData(
      cell.datafield ? cell.datafield.name : null,
      cell.rowDimension,
      cell.columnDimension)
    return <DataCellComp key={columnIndex} cell={cell} onDoubleClick={undefined} />
  }

  columnHeaderRenderer ({columnIndex, rowIndex}) {
    const {columns} = this.props
    const cell = columns.headers[rowIndex][columnIndex]
    if (!cell) {
      return null
    } else {
      return <HeaderCellComp cell={cell} onToggle={undefined} />
    }
  }

  rowHeaderRenderer ({columnIndex, rowIndex}) {
    const {rows} = this.props
    const cell = rows.headers[rowIndex][columnIndex]
    if (!cell) {
      return null
    } else {
      return <HeaderCellComp cell={cell} onToggle={undefined} />
    }
  }
}
