import React, { Component } from 'react'
import { Grid, AutoSizer } from 'react-virtualized'

import HeaderCellComp from '../HeaderCell'
import DataCellComp from '../DataCell'
import { DataCell } from '../../Cells'

export default class OrbGrid extends Component {

  componentWillReceiveProps (nextProps, nextState) {
    const layout = this.getLayout(nextProps.store)

    this.setState({ ...layout, cellsCache: this._datacells })

    // Change scroll values to stay at the same position when modifying the layout
    this.scrollLeft = this._grid.state.scrollLeft * (layout.columnHorizontalCount / this.state.columnHorizontalCount)
    this.scrollTop = this._grid.state.scrollTop * (layout.rowVerticalCount / this.state.rowVerticalCount)
  }

  componentWillUpdate (nextProps, nextState) {
    // to handle case where all data fields are unactivated
    this._isUpdating = true
    this._grid.forceUpdate()
  }

  componentDidUpdate (prevProps, prevState) {
    this._isUpdating = false
  }

  constructor (props) {
    super(props)

    this.state = {
      ...this.getLayout(props.store),
      cellsCache: new Map()
    }

    this.scrollLeft = 0
    this.scrollTop = 0

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this)
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this)
  }

  getLayout ({layout, sizes}) {
    const cellHeight = sizes.cell.height
    const cellWidth = sizes.cell.width

    const rowVerticalCount = layout.rowHeaders.height
    const rowHorizontalCount = layout.rowHeaders.width
    const columnVerticalCount = layout.columnHeaders.height
    const columnHorizontalCount = layout.columnHeaders.width

    const rowHeadersWidth = rowHorizontalCount * cellWidth
    const rowHeadersHeight = rowVerticalCount * cellHeight
    const columnHeadersHeight = columnVerticalCount * cellHeight
    const columnHeadersWidth = columnHorizontalCount * cellWidth

    const height = Math.min(sizes.grid.height, columnHeadersHeight + rowHeadersHeight)
    const width = Math.min(sizes.grid.width, rowHeadersWidth + columnHeadersWidth)

    return ({
      cellHeight,
      cellWidth,
      rowVerticalCount,
      rowHorizontalCount,
      columnVerticalCount,
      columnHorizontalCount,
      rowHeadersWidth,
      rowHeadersHeight,
      columnHeadersHeight,
      columnHeadersWidth,
      height,
      width
    })
  }

  render () {
    const {
      cellHeight,
      cellWidth,
      columnHeadersHeight,
      columnHeadersWidth,
      columnHorizontalCount,
      columnVerticalCount,
      rowHeadersHeight,
      rowHeadersWidth,
      rowHorizontalCount,
      rowVerticalCount
    } = this.state
    return (
      <AutoSizer>
        {({width, height}) =>
          <Grid
            cellRangeRenderer={this.cellRangeRenderer}
            cellRenderer={this._mockCellRenderer}
            columnCount={columnHorizontalCount + rowHorizontalCount}
            columnWidth={cellWidth}
            height={Math.min(height, columnHeadersHeight + rowHeadersHeight)}
            overscanRowCount={0}
            overscanColumnCount={0}
            ref={ref => { this._grid = ref }}
            rowCount={columnVerticalCount + rowVerticalCount}
            rowHeight={cellHeight}
            scrollLeft={this.scrollLeft}
            scrollTop={this.scrollTop}
            width={Math.min(width, rowHeadersWidth + columnHeadersWidth)}
          />}
      </AutoSizer>
    )
  }

  cellRangeRenderer ({
    cellCache,
    cellClassName,
    cellRenderer,
    cellStyle,
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    horizontalOffsetAdjustment,
    isScrolling,
    rowSizeAndPositionManager,
    rowStartIndex,
    rowStopIndex,
    scrollLeft,
    scrollTop,
    verticalOffsetAdjustment
  }) {
    const {columnsUi, rowsUi} = this.props.store
    const columnHeaders = columnsUi.headers
    const rowHeaders = rowsUi.headers
    const {
      columnHeadersHeight,
      columnHorizontalCount,
      columnVerticalCount,
      rowHeadersWidth,
      rowHorizontalCount,
      rowVerticalCount
    } = this.state
    const renderedCells = []

    // to avoid rendering empty cells
    // there is a difference between columnCount (the prop of the Grid object) and the column count except the row headers
    // the -1 is here because there are inferior or equal signs in the loops
    const _columnStopIndex = Math.min(columnStopIndex - rowHorizontalCount, columnHorizontalCount - 1)
    const _rowStopIndex = Math.min(rowStopIndex - columnVerticalCount, rowVerticalCount - 1)

    const visibleRows = (_rowStopIndex - rowStartIndex) + 1
    const visibleColumns = (_columnStopIndex - columnStartIndex) + 1

    // Top-left corner piece
    renderedCells.push(
      <div
        key='fixed-fixed'
        className={'Grid__cell'}
        style={{
          position: 'fixed',
          left: scrollLeft,
          top: scrollTop,
          width: rowHeadersWidth,
          height: columnHeadersHeight,
          zIndex: 2,
          backgroundColor: '#fff'
        }}>
      </div>
    )

    // Render fixed header rows

    // Render big cells on top of current cells if necessary
    if (columnHeaders[columnStartIndex].length < columnVerticalCount) {
      let columnHeader = columnHeaders[columnStartIndex][0]
      while (columnHeader.parent) {
        columnHeader = columnHeader.parent
        renderedCells.push(this.columnHeaderRenderer({columnHeader, scrollLeft, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment}))
      }
    }

    for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
      for (let columnHeaderIndex = 0; columnHeaderIndex < columnHeaders[columnIndex].length; columnHeaderIndex++) {
        let columnHeader = columnHeaders[columnIndex][columnHeaderIndex]
        renderedCells.push(this.columnHeaderRenderer({columnHeader, scrollLeft, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment}))
      }
    }

    // Render fixed left columns

    // Render big cells on the left of current cells if necessary
    if (rowHeaders[rowStartIndex].length < rowHorizontalCount) {
      let rowHeader = rowHeaders[rowStartIndex][0]
      while (rowHeader.parent) {
        rowHeader = rowHeader.parent
        renderedCells.push(this.rowHeaderRenderer({rowHeader, scrollLeft, scrollTop, rowStartIndex, visibleRows, verticalOffsetAdjustment}))
      }
    }

    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      for (let rowHeaderIndex = 0; rowHeaderIndex < rowHeaders[rowIndex].length; rowHeaderIndex++) {
        let rowHeader = rowHeaders[rowIndex][rowHeaderIndex]
        renderedCells.push(this.rowHeaderRenderer({rowHeader, scrollLeft, scrollTop, rowStartIndex, visibleRows, verticalOffsetAdjustment}))
      }
    }

    // Render data cells
    this._datacells = new Map()
    if (!isScrolling) {
      for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
        let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
        for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
          let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
          renderedCells.push(this.dataCellRenderer({columnIndex, rowIndex, columnDatum, rowDatum, horizontalOffsetAdjustment, visibleRows, visibleColumns, verticalOffsetAdjustment}))
        }
      }
    }

    return renderedCells
  }

  dataCellRenderer ({columnIndex, rowIndex, columnDatum, rowDatum, horizontalOffsetAdjustment, visibleRows, visibleColumns, verticalOffsetAdjustment}) {
    const {cellHeight, cellWidth, rowHeadersWidth, columnHeadersHeight} = this.state
    const {store, drilldown} = this.props
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
    let style = {
      position: 'fixed',
      height: cellHeight,
      width: cellWidth,
      left: columnDatum.offset + rowHeadersWidth + horizontalOffsetAdjustment,
      top: rowDatum.offset + columnHeadersHeight + verticalOffsetAdjustment
    }
    const key = `${rowHeader.key}-//-${columnHeader.key}`
    this._datacells.set(key, cell)
    const renderedCell = <DataCellComp key={`data-${rowIndex % visibleRows}-${columnIndex % visibleColumns}`} cell={cell} onDoubleClick={() => drilldown(cell)} />
    let valueHasChanged = false
    if (this._isUpdating) {
      const oldcell = this.state.cellsCache.get(key)
      if (oldcell && cell.value !== oldcell.value) {
        valueHasChanged = true
      }
    }
    return (
      <div
        key={`${rowIndex % visibleRows}-${columnIndex % visibleColumns}`}
        className={valueHasChanged ? 'Grid__cell highlighted' : 'Grid__cell normal'}
        style={style}>
        {renderedCell}
      </div>
    )
  }

  columnHeaderRenderer ({columnHeader, scrollLeft, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment}) {
    const {cellWidth, cellHeight, rowHeadersWidth} = this.state
    let {x, y} = columnHeader
    let renderedCell = <HeaderCellComp key={`row-${x % visibleColumns}-${y}`} cell={columnHeader} onToggle={() => 33} />
    const left = x * cellWidth + rowHeadersWidth + horizontalOffsetAdjustment
    const width = cellWidth * columnHeader.hspan()
    const affix = width > cellWidth && x <= columnStartIndex
    return (
      <div
      // add 1 to key modulo to avoid collision when rendering parent cells
        key={`fixedrow-${x % (visibleColumns + 1)}-${y}`}
        className={'Grid__cell'}
        style={{
          position: 'fixed',
          left,
          top: y * cellHeight + scrollTop,
          height: cellHeight * columnHeader.vspan(),
          width,
          zIndex: 1,
          backgroundColor: '#eef8fb'
        }}>
        <div style={affix ? {position: 'relative', left: Math.min(scrollLeft % width, width - cellWidth), color: 'red'} : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  rowHeaderRenderer ({rowHeader, scrollLeft, scrollTop, rowStartIndex, visibleRows, visibleColumns, verticalOffsetAdjustment}) {
    const {cellWidth, cellHeight, columnHeadersHeight} = this.state
    let {x, y} = rowHeader
    let renderedCell = <HeaderCellComp key={`col-${x % visibleRows}-${y}`} cell={rowHeader} onToggle={() => 33} />
    const top = x * cellHeight + columnHeadersHeight + verticalOffsetAdjustment
    const height = cellHeight * rowHeader.vspan()
    const affix = height > cellHeight && x <= rowStartIndex
    return (
      <div
        // add 1 to key modulo to avoid collision when rendering parent cells
        key={`fixedcol-${x % (visibleRows + 1)}-${y}`}
        className={'Grid__cell'}
        style={{
          position: 'fixed',
          left: y * cellWidth + scrollLeft,
          top,
          height,
          width: cellWidth * rowHeader.hspan(),
          zIndex: 1,
          backgroundColor: '#eef8fb'
        }}>
        <div style={affix ? {position: 'relative', top: Math.min(scrollTop % height, height - cellHeight), color: 'red'} : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  _mockCellRenderer ({columnIndex, rowIndex}) {
    return 33
  }
}
