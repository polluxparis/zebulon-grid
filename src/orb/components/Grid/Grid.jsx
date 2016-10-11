import React, { Component } from 'react'
import {findDOMNode} from 'react-dom'
import { Grid, AutoSizer } from 'react-virtualized'

import HeaderCellComp from '../HeaderCell'
import DataCellComp from '../DataCell'
import { DataCell } from '../../Cells'
import {rightArrow, downArrow} from '../../assets/icons'
import {isInRange} from '../../Utils'
// import './Grid.css'

export default class OrbGrid extends Component {

  componentDidMount () {
    document.addEventListener('mouseup', this.handleMouseUp)
    document.addEventListener('mousedown', this.handleDocumentMouseDown)
    document.addEventListener('copy', this.handleCopy)
  }

  componentDidUnMount () {
    document.removeEventListener('mouseup', this.handleMouseUp)
    document.removeEventListener('mousedown', this.handleDocumentMouseDown)
    document.removeEventListener('copy', this.handleCopy)
  }

  componentWillReceiveProps (nextProps, nextState) {
    const layout = this.getLayout(nextProps.store)

    this.setState({ ...layout, cellsCache: this._datacells || new Map() })

    // Change scroll values to stay at the same position when modifying the layout
    this.scrollLeft = this._grid.state.scrollLeft * (layout.columnHorizontalCount / this.state.columnHorizontalCount)
    this.scrollTop = this._grid.state.scrollTop * (layout.rowVerticalCount / this.state.rowVerticalCount)
  }

  componentWillUpdate (nextProps, nextState) {
    this._isUpdating = true
    // to handle case where all data fields are unactivated
    this._grid.forceUpdate()
  }

  componentDidUpdate (prevProps, prevState) {
    this._isUpdating = false
  }

  constructor (props) {
    super(props)

    this.state = {
      ...this.getLayout(props.store),
      cellsCache: new Map(),
      selectedCellStart: null,
      selectedCellEnd: null
    }

    this.scrollLeft = 0
    this.scrollTop = 0

    this._isMouseDown = false

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this)
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this)
    this.dimensionHeaderRenderer = this.dimensionHeaderRenderer.bind(this)
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseUp = this.handleMouseUp.bind(this)
    this.handleMouseOver = this.handleMouseOver.bind(this)
    this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this)
    this.handleCopy = this.handleCopy.bind(this)
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

  handleMouseDown (e, [rowIndex, columnIndex]) {
    if (e.button === 0) {
      this._isMouseDown = true
      this.setState({selectedCellStart: [rowIndex, columnIndex]})
      this.setState({selectedCellEnd: [rowIndex, columnIndex]})
    }
  }

  handleMouseUp () {
    this._isMouseDown = false
  }

  handleMouseOver ([rowIndex, columnIndex]) {
    if (this._isMouseDown) {
      this.setState({selectedCellEnd: [rowIndex, columnIndex]})
    }
  }

  handleDocumentMouseDown (e) {
    if (e.button === 0) {
      if (!this._isMouseDown) {
        this.setState({selectedCellStart: null, selectedCellEnd: null})
      }
    }
  }

  handleCopy () {
    try {
      if (findDOMNode(this._grid) === document.activeElement) {
        const bodyElement = document.getElementsByTagName('body')[0]
        const clipboardTextArea = document.createElement('textarea')
        clipboardTextArea.style.position = 'absolute'
        clipboardTextArea.style.left = '-10000px'
        bodyElement.appendChild(clipboardTextArea)
        clipboardTextArea.innerHTML = this.getSelectedText()
        clipboardTextArea.select()
        window.setTimeout(() => { bodyElement.removeChild(clipboardTextArea) }, 0)
      }
    } catch (error) {
      console.error('error in handleCopy', error)
    }
  }

  getSelectedText () {
    const {selectedCellStart, selectedCellEnd} = this.state
    const {columnsUi, rowsUi} = this.props.store

    // Build rows headers array
    const rowsRange = [Math.min(selectedCellStart[0], selectedCellEnd[0]), Math.max(selectedCellStart[0], selectedCellEnd[0]) + 1]
    const rowHeaderLeafs = rowsUi.headers.slice(...rowsRange).map(headers => headers[headers.length - 1])
    const rows = []
    for (let header of rowHeaderLeafs) {
      let res = []
      while (header) {
        res.unshift(header.caption)
        header = header.parent
      }
      rows.push(res)
    }

    // Build columns headers array
    const columnsRange = [Math.min(selectedCellStart[1], selectedCellEnd[1]), Math.max(selectedCellStart[1], selectedCellEnd[1]) + 1]
    const columnHeaderLeafs = columnsUi.headers.slice(...columnsRange).map(headers => headers[headers.length - 1])
    const columns = []
    for (let header of columnHeaderLeafs) {
      let res = []
      while (header) {
        res.unshift(header.caption)
        header = header.parent
      }
      columns.push(res)
    }

    // Build data array
    const cells = []
    for (let rowHeader of rowHeaderLeafs) {
      let cellRow = []
      for (let columnHeader of columnHeaderLeafs) {
        const caption = new DataCell(this.props.store, true, rowHeader, columnHeader).caption
        cellRow.push(caption)
      }
      cells.push(cellRow)
    }
    const rowDimensions = rowsUi.dimensionHeaders.map(header => header.value.caption)
    const columnDimensions = columnsUi.dimensionHeaders.map(header => header.value.caption)

    // Format data to text
    let output = ''
    // First rows with only the dimension and columns headers
    const depth = columns[0].length
    const width = rows[0].length
    for (let y = 0; y < depth; y++) {
      for (let x = 0; x < width; x++) {
        if (x === width - 1 && y < depth - 1) {
          output += `${columnDimensions[y] || ''}\t`
        } else if (y === depth - 1 && x < width - 1) {
          output += `${rowDimensions[x] || ''}\t`
        } else if (y === depth - 1 && x === width - 1) {
          // Handle corner case
          // Dimension header in bottom right cell can refer to a column header
          // or a row header depending on data headers location
          if (this.props.store.config.dataHeadersLocation === 'columns') {
            output += `${rowDimensions[x] || ''}\t`
          } else {
            output += `${columnDimensions[y] || ''}\t`
          }
        } else {
          output += '\t'
        }
      }
      for (let column of columns) {
        output += `${column[y]}\t`
      }
      output = output.slice(0, -1)
      output += '\n'
    }
    // Other rows with rows headers and data
    for (let y = 0; y < rows.length; y++) {
      for (let x = 0; x < width; x++) {
        output += `${rows[y][x]}\t`
      }
      for (let x = 0; x < columnHeaderLeafs.length; x++) {
        output += `${cells[y][x]}\t`
      }
      output = output.slice(0, -1)
      output += '\n'
    }
    console.log(output)
    output = output.slice(0, -1)
    return output
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
    const columnDimensionHeaders = columnsUi.dimensionHeaders
    const rowDimensionHeaders = rowsUi.dimensionHeaders

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
        className={'ReactVirtualized__Grid__cell'}
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

    // Render dimension headers

    for (let [index, dimensionHeader] of columnDimensionHeaders.entries()) {
      renderedCells.push(this.dimensionHeaderRenderer({scrollLeft, scrollTop, dimensionHeader, index}))
    }

    for (let [index, dimensionHeader] of rowDimensionHeaders.entries()) {
      renderedCells.push(this.dimensionHeaderRenderer({scrollLeft, scrollTop, dimensionHeader, index}))
    }

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
    // if (!isScrolling) {
    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
      for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
        let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
        renderedCells.push(this.dataCellRenderer({columnIndex, rowIndex, columnDatum, rowDatum, scrollLeft, scrollTop, horizontalOffsetAdjustment, visibleRows, visibleColumns, verticalOffsetAdjustment}))
      }
    }
    // }

    return renderedCells
  }

  dataCellRenderer ({columnIndex, rowIndex, columnDatum, rowDatum, horizontalOffsetAdjustment, visibleRows, visibleColumns, verticalOffsetAdjustment, scrollTop, scrollLeft}) {
    const {cellHeight, cellWidth, rowHeadersWidth, columnHeadersHeight, selectedCellStart, selectedCellEnd} = this.state
    const {store, drilldown} = this.props
    const {rowsUi, columnsUi} = store
    const rowHeaderRow = rowsUi.headers[rowIndex]
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1]
    const columnHeaderColumn = columnsUi.headers[columnIndex]
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1]
    let selected = false
    if (selectedCellStart && selectedCellEnd) {
      selected = isInRange([rowIndex, columnIndex], selectedCellStart, selectedCellEnd)
    }

    const cell = new DataCell(
      store,
      () => rowHeader.visible() && columnHeader.visible(),
      rowHeader,
      columnHeader
    )
    let style = {
      border: 'solid lightgrey thin',
      boxSizing: 'border-box',
      padding: '0.2em',
      overflow: 'hidden',
      position: 'fixed',
      height: cellHeight,
      width: cellWidth,
      // The modulos allow discrete scrolling
      left: columnDatum.offset + rowHeadersWidth + horizontalOffsetAdjustment + (scrollLeft % cellWidth),
      top: rowDatum.offset + columnHeadersHeight + verticalOffsetAdjustment + (scrollTop % cellHeight)
    }
    let unEvenRowStyle = {
      backgroundColor: 'rgba(211, 211, 211, 0.4)'
    }
    let evenRowStyle = {
      backgroundColor: 'white'
    }

    if (rowIndex % 2) {
      style = {...style, ...unEvenRowStyle}
    } else {
      style = {...style, ...evenRowStyle}
    }

    let selectedStyle = {
      backgroundColor: 'lightcyan'
    }

    if (selected) {
      style = {...style, ...selectedStyle}
    }

    const key = `${rowHeader.key}-//-${columnHeader.key}`
    this._datacells.set(key, cell)
    const renderedCell = <DataCellComp
      key={`data-${rowIndex % visibleRows}-${columnIndex % visibleColumns}`}
      cell={cell}
      onDoubleClick={() => drilldown(cell)}
      />
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
        className={valueHasChanged ? 'ReactVirtualized__Grid__cell OrbGrid-cell-highlighted' : 'ReactVirtualized__Grid__cell normal'}
        style={style}
        onMouseDown={(e) => this.handleMouseDown(e, [rowIndex, columnIndex])}
        onMouseOver={() => this.handleMouseOver([rowIndex, columnIndex])}
        >
        {renderedCell}
      </div>
    )
  }

  columnHeaderRenderer ({columnHeader, scrollLeft, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment}) {
    const {cellWidth, cellHeight, rowHeadersWidth, columnHorizontalCount} = this.state
    const {x, y} = columnHeader
    const renderedCell = <HeaderCellComp key={`row-${x % visibleColumns}-${y}`} cell={columnHeader} onToggle={() => 33} />
    const left = x * cellWidth + rowHeadersWidth + horizontalOffsetAdjustment
    const width = cellWidth * columnHeader.hspan()
    const affix = width > cellWidth && x <= columnStartIndex
    return (
      <div
      // add 1 to key modulo to avoid collision when rendering parent cells
        key={`fixedrow-${x % (visibleColumns + columnHorizontalCount)}-${y}`}
        className={'ReactVirtualized__Grid__cell'}
        style={{
          border: 'solid lightgrey thin',
          boxSizing: 'border-box',
          padding: '0.2em',
          overflow: 'hidden',
          position: 'fixed',
          // to have discrete scroll
          left: left + (scrollLeft % cellWidth),
          top: y * cellHeight + scrollTop,
          height: cellHeight * columnHeader.vspan(),
          width,
          zIndex: 1,
          backgroundColor: '#eef8fb'
        }}>
        <div style={affix ? {
          position: 'relative',
          // to keep the label visible upon scrolling
          left: Math.floor((scrollLeft + rowHeadersWidth - left) / cellWidth) * cellWidth
        } : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  rowHeaderRenderer ({rowHeader, scrollLeft, scrollTop, rowStartIndex, visibleRows, visibleColumns, verticalOffsetAdjustment}) {
    const {cellWidth, cellHeight, columnHeadersHeight, rowVerticalCount} = this.state
    const {x, y} = rowHeader
    const renderedCell = <HeaderCellComp key={`col-${x % visibleRows}-${y}`} cell={rowHeader} onToggle={() => 33} />
    const top = x * cellHeight + columnHeadersHeight + verticalOffsetAdjustment
    const height = cellHeight * rowHeader.vspan()
    const affix = height > cellHeight && x <= rowStartIndex
    return (
      <div
        // add 1 to key modulo to avoid collision when rendering parent cells
        key={`fixedcol-${x % (visibleRows + rowVerticalCount)}-${y}`}
        className={'ReactVirtualized__Grid__cell'}
        style={{
          border: 'solid lightgrey thin',
          boxSizing: 'border-box',
          padding: '0.2em',
          overflow: 'hidden',
          position: 'fixed',
          left: y * cellWidth + scrollLeft,
          // to have discrete scroll
          top: top + (scrollTop % cellHeight),
          height,
          width: cellWidth * rowHeader.hspan(),
          zIndex: 1,
          backgroundColor: '#eef8fb'
        }}>
        <div style={affix ? {
          position: 'relative',
          // to keep the label visible upon scrolling
          top: Math.floor((scrollTop + columnHeadersHeight - top) / cellHeight) * cellHeight
        } : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  dimensionHeaderRenderer ({dimensionHeader, index, scrollLeft, scrollTop}) {
    const {rowHeadersWidth, columnHeadersHeight, cellHeight, cellWidth} = this.state

    const axe = dimensionHeader.axetype === 1 ? 'column' : 'row'

    let left
    let top
    if (axe === 'column') {
      left = scrollLeft + rowHeadersWidth - cellWidth
      top = scrollTop + cellHeight * index
    } else {
      left = scrollLeft + cellWidth * index
      top = scrollTop + columnHeadersHeight - cellHeight
    }
    return <div
      key={`fixed-dim-${axe}-${index}`}
      className={'ReactVirtualized__Grid__cell'}
      style={{
        position: 'fixed',
        left,
        top,
        width: cellWidth,
        height: cellHeight,
        zIndex: 3,
        border: 'lightgrey 0.1em solid',
        boxSizing: 'border-box',
        textAlign: 'left',
        display: 'flex',
        justifyContent: 'space-between',
        padding: '0.2em',
        backgroundColor: '#fafad2'
      }}>
      {dimensionHeader.value.caption}
      {axe === 'column' ? rightArrow : downArrow}
    </div>
  }

  _mockCellRenderer ({columnIndex, rowIndex}) {
    return 33
  }
}
