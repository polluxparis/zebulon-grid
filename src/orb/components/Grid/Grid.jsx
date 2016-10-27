import React, { Component } from 'react'
import {findDOMNode} from 'react-dom'
import { Grid, AutoSizer } from 'react-virtualized'

import HeaderCellComp from '../HeaderCell'
import DataCellComp from '../DataCell'
import { DataCell } from '../../Cells'
import {isInRange} from '../../Utils'

function replaceNullAndUndefined (val) {
  if (val === null || val === undefined) {
    return ''
  } else {
    return val
  }
}

function getHeaderSize (sizeAndPositionManager, index, span) {
    let res = 0
    for (let i = 0; i < span; i++) {
      res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size
    }
    return res
}

function getSelectedText ({selectedCellStart, selectedCellEnd, store}) {
  const {columnsUi, rowsUi} = store

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
      const caption = new DataCell(store, true, rowHeader, columnHeader).caption
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
        output += `${replaceNullAndUndefined(columnDimensions[y])}\t`
      } else if (y === depth - 1 && x < width - 1) {
        output += `${replaceNullAndUndefined(rowDimensions[x])}\t`
      } else if (y === depth - 1 && x === width - 1) {
        // Handle corner case
        // Dimension header in bottom right cell can refer to a column header
        // or a row header depending on data headers location
        if (store.config.dataHeadersLocation === 'columns') {
          output += `${replaceNullAndUndefined(rowDimensions[x])}\t`
        } else {
          output += `${replaceNullAndUndefined(columnDimensions[y])}\t`
        }
      } else {
        output += '\t'
      }
    }
    for (let column of columns) {
      output += `${replaceNullAndUndefined(column[y])}\t`
    }
    output = output.slice(0, -1)
    output += '\n'
  }
  // Other rows with rows headers and data
  for (let y = 0; y < rows.length; y++) {
    for (let x = 0; x < width; x++) {
      output += `${replaceNullAndUndefined(rows[y][x])}\t`
    }
    for (let x = 0; x < columnHeaderLeafs.length; x++) {
      output += `${replaceNullAndUndefined(cells[y][x])}\t`
    }
    output = output.slice(0, -1)
    output += '\n'
  }
  output = output.slice(0, -1)
  return output
}


export default class OrbGrid extends Component {
  constructor (props) {
    super(props)

    this.defaultCellWidth = this.props.store.sizes.cell.width
    this.defaultCellHeight = this.props.store.sizes.cell.height

    this.state = {
      cellsCache: new Map(),
      selectedCellStart: null,
      selectedCellEnd: null,
      columnWidths: new Map(),
      rowHeights: new Map()
    }

    // State is set in two parts
    this.state = {...this.state, ...this.getLayout(props.store)}


    this.scrollLeft = 0
    this.scrollTop = 0

    this._isMouseDown = false

    this.getLayout = this.getLayout.bind(this)
    this.getColumnWidth = this.getColumnWidth.bind(this)
    this.getRowHeight = this.getRowHeight.bind(this)
    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.rowHeaderRenderer = this.rowHeaderRenderer.bind(this)
    this.columnHeaderRenderer = this.columnHeaderRenderer.bind(this)
    this.dimensionHeaderRenderer = this.dimensionHeaderRenderer.bind(this)
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseUp = this.handleMouseUp.bind(this)
    this.handleMouseOver = this.handleMouseOver.bind(this)
    this.handleKeyDown = this.handleKeyDown.bind(this)
    this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this)
    this.handleCopy = this.handleCopy.bind(this)
  }

  componentDidMount () {
    document.addEventListener('mouseup', this.handleMouseUp)
    document.addEventListener('mousedown', this.handleDocumentMouseDown)
    document.addEventListener('copy', this.handleCopy)
    document.addEventListener('keydown', this.handleKeyDown)
  }

  componentDidUnMount () {
    document.removeEventListener('mouseup', this.handleMouseUp)
    document.removeEventListener('mousedown', this.handleDocumentMouseDown)
    document.removeEventListener('copy', this.handleCopy)
    document.removeEventListener('keydown', this.handleKeyDown)
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
    // this._grid.forceUpdate()
  }

  componentDidUpdate (prevProps, prevState) {
    this._isUpdating = false
  }

  getLayout (store) {
    const {layout, sizes, columnsUi, rowsUi} = store
    const rowVerticalCount = layout.rowHeaders.height
    const rowHorizontalCount = layout.rowHeaders.width
    const columnVerticalCount = layout.columnHeaders.height
    const columnHorizontalCount = layout.columnHeaders.width

    let rowHeadersWidth = 0
    for (let header of rowsUi.headers[0]) {
      rowHeadersWidth += this.getColumnWidth({index: header.x})
    }
    let rowHeadersHeight = 0
    for (let headers of rowsUi.headers) {
      rowHeadersHeight += this.getRowHeight({index: headers[0].x})
    }
    let columnHeadersWidth = 0
    for (let headers of columnsUi.headers) {
      columnHeadersWidth += this.getColumnWidth({index: headers[0].x})
    }
    let columnHeadersHeight = 0
    for (let header of columnsUi.headers[0]) {
      columnHeadersHeight += this.getRowHeight({index: header.x})
    }

    const height = Math.min(sizes.grid.height, columnHeadersHeight + rowHeadersHeight)
    const width = Math.min(sizes.grid.width, rowHeadersWidth + columnHeadersWidth)

    return ({
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

  getColumnWidth ({index}) {
    // if (index === 1) return 150
    const {columnWidths} = this.state
    return columnWidths.get(index) || this.defaultCellWidth
  }

  getRowHeight ({index}) {
    // if (index === 1) return 60
    const {rowHeights} = this.state
    return rowHeights.get(index) || this.defaultCellHeight
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

  handleKeyDown (e) {
    const {rowsUi, columnsUi} = this.props.store
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (findDOMNode(this._grid) === e.target) {
        this.setState({selectedCellStart: [0, 0], selectedCellEnd: [rowsUi.headers.length, columnsUi.headers.length]})
      }
      e.preventDefault()
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
        const {selectedCellStart, selectedCellEnd} = this.state
        clipboardTextArea.innerHTML = getSelectedText({selectedCellStart, selectedCellEnd, props: this.props.store})
        clipboardTextArea.select()
        window.setTimeout(() => { bodyElement.removeChild(clipboardTextArea) }, 0)
      }
    } catch (error) {
      console.error('error in handleCopy', error)
    }
  }

  render () {
    const {
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
            columnWidth={this.getColumnWidth}
            height={Math.min(height, columnHeadersHeight + rowHeadersHeight)}
            overscanRowCount={0}
            overscanColumnCount={0}
            ref={ref => { this._grid = ref }}
            rowCount={columnVerticalCount + rowVerticalCount}
            rowHeight={this.getRowHeight}
            scrollLeft={this.scrollLeft}
            scrollTop={this.scrollTop}
            style={{fontSize: `${this.props.store.zoom*100}%`}}
            width={Math.min(width, rowHeadersWidth + columnHeadersWidth)}
          />
      }
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
    // The check on the presence of the header is necessary because it can be out of bounds when the headers array is modified
    if (columnHeaders[columnStartIndex] && columnHeaders[columnStartIndex].length < columnVerticalCount) {
      let columnHeader = columnHeaders[columnStartIndex][0]
      while (columnHeader.parent) {
        columnHeader = columnHeader.parent
        const span = columnHeader.hspan()
        let {size, offset} = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnHeader.x)
        size = getHeaderSize(columnSizeAndPositionManager, columnHeader.x, span)
        let lastChild = columnHeader
        while (lastChild.subheaders && lastChild.subheaders.length){
          lastChild = lastChild.subheaders[lastChild.subheaders.length - 1]
        }
        const lastChildSize = getHeaderSize(columnSizeAndPositionManager, lastChild.x, lastChild.hspan())
        renderedCells.push(this.columnHeaderRenderer({columnHeader, scrollLeft, scrollTop, size, offset, columnStartIndex, visibleColumns, horizontalOffsetAdjustment, span, lastChildSize}))
      }
    }

    for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
      let {size, offset} = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
      for (let columnHeaderIndex = 0; columnHeaderIndex < columnHeaders[columnIndex].length; columnHeaderIndex++) {
        let columnHeader = columnHeaders[columnIndex][columnHeaderIndex]
        const span = columnHeader.hspan()
        size = getHeaderSize(columnSizeAndPositionManager, columnIndex, span)
        renderedCells.push(this.columnHeaderRenderer({columnHeader, scrollLeft, size, offset, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment, span}))
      }
    }

    // Render fixed left columns

    // Render big cells on the left of current cells if necessary
    // The check on the presence of the header is necessary because it can be out of bounds when the headers array is modified
    if (rowHeaders[rowStartIndex] && rowHeaders[rowStartIndex].length < rowHorizontalCount) {
      let rowHeader = rowHeaders[rowStartIndex][0]
      while (rowHeader.parent) {
        rowHeader = rowHeader.parent
        const span = rowHeader.vspan()
        let {size, offset} = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowHeader.x)
        size = getHeaderSize(rowSizeAndPositionManager, rowHeader.x, span)
        let lastChild = rowHeader
        while (lastChild.subheaders && lastChild.subheaders.length){
          lastChild = lastChild.subheaders[lastChild.subheaders.length - 1]
        }

        const lastChildSize = getHeaderSize(rowSizeAndPositionManager, lastChild.x, lastChild.vspan())
        renderedCells.push(this.rowHeaderRenderer({rowHeader, scrollLeft, scrollTop, size, offset, rowStartIndex, visibleRows, verticalOffsetAdjustment, span, lastChildSize}))
      }
    }

    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      let {size, offset} = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
      for (let rowHeaderIndex = 0; rowHeaderIndex < rowHeaders[rowIndex].length; rowHeaderIndex++) {
        let rowHeader = rowHeaders[rowIndex][rowHeaderIndex]
        const span = rowHeader.vspan()
        size = getHeaderSize(rowSizeAndPositionManager, rowIndex, span)
        renderedCells.push(this.rowHeaderRenderer({rowHeader, size, offset, scrollLeft, scrollTop, rowStartIndex, visibleRows, verticalOffsetAdjustment, span}))
      }
    }

    // Render data cells
    this._datacells = new Map()
    // if (!isScrolling) {
    for (let rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
      let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
      for (let columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
        let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
        renderedCells.push(this.dataCellRenderer({columnIndex,
          rowIndex,
          columnDatum,
          rowDatum,
          scrollLeft,
          scrollTop,
          horizontalOffsetAdjustment,
          visibleRows,
          visibleColumns,
          verticalOffsetAdjustment}))
      }
    }
    // }
    return renderedCells
  }

  dataCellRenderer ({columnIndex, rowIndex, columnDatum, rowDatum, horizontalOffsetAdjustment, visibleRows, visibleColumns, verticalOffsetAdjustment, scrollTop, scrollLeft}) {
    const {rowHeadersWidth, columnHeadersHeight, selectedCellStart, selectedCellEnd} = this.state
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
      height: rowDatum.size,
      width: columnDatum.size,
      // The modulos allow discrete scrolling
      // left: columnDatum.offset + rowHeadersWidth + horizontalOffsetAdjustment + (scrollLeft % this.defaultCellWidth),
      // top: rowDatum.offset + columnHeadersHeight + verticalOffsetAdjustment + (scrollTop % this.defaultCellHeight)
      left: columnDatum.offset + rowHeadersWidth + horizontalOffsetAdjustment,
      top: rowDatum.offset + columnHeadersHeight + verticalOffsetAdjustment
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

  columnHeaderRenderer ({columnHeader, scrollLeft, scrollTop, columnStartIndex, visibleColumns, horizontalOffsetAdjustment, size, offset, span, lastChildSize}) {
    const {rowHeadersWidth, columnHorizontalCount} = this.state
    const {x, y} = columnHeader
    const renderedCell = <HeaderCellComp key={`row-${x % visibleColumns}-${y}`} cell={columnHeader} onToggle={() => 33} />
    const left = offset + rowHeadersWidth + horizontalOffsetAdjustment
    const top = y * this.defaultCellHeight + scrollTop
    const width = size
    const height = this.defaultCellHeight * columnHeader.vspan()
    const affix = span > 1 && x <= columnStartIndex
    return (
      <div
        key={`fixedrow-${x % (visibleColumns + columnHorizontalCount)}-${y}`}
        className={'ReactVirtualized__Grid__cell'}
        style={{
          border: 'solid lightgrey thin',
          boxSizing: 'border-box',
          padding: '0.2em',
          overflow: 'hidden',
          backgroundColor: '#eef8fb',
          position: 'fixed',
          // left: left + (scrollLeft % this.defaultCellWidth), // for discrete scroll
          left,
          top,
          height,
          width,
          zIndex: 1
        }}>
        <div style={affix ? {
          // To keep the label visible upon scrolling
          position: 'relative',
          color: 'red',
          // left: Math.floor((scrollLeft + rowHeadersWidth - left) / this.defaultCellWidth) * this.defaultCellWidth // for discrete scroll
          left: Math.min(scrollLeft + rowHeadersWidth - left, width - (lastChildSize || 0)) // for continuous scroll
        } : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  rowHeaderRenderer ({rowHeader, scrollLeft, scrollTop, rowStartIndex, visibleRows, visibleColumns, verticalOffsetAdjustment, size, offset, span, lastChildSize}) {
    const {columnHeadersHeight, rowVerticalCount} = this.state
    const {x, y} = rowHeader
    const renderedCell = <HeaderCellComp key={`col-${x % visibleRows}-${y}`} cell={rowHeader} onToggle={() => 33} />
    // const top = x * this.defaultCellHeight + columnHeadersHeight + verticalOffsetAdjustment
    const top = offset + columnHeadersHeight + verticalOffsetAdjustment
    const left = y * this.defaultCellWidth + scrollLeft
    // const height = this.defaultCellHeight * rowHeader.vspan()
    const height = size
    const width = this.defaultCellWidth * rowHeader.hspan()
    const affix = span > 1 && x <= rowStartIndex
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
          backgroundColor: '#eef8fb',
          position: 'fixed',
          // to have discrete scroll
          // top: top + (scrollTop % this.defaultCellHeight),
          top,
          left,
          height,
          width,
          zIndex: 1,
        }}>
        <div style={affix ? {
          position: 'relative',
          color: 'red',
          // to keep the label visible upon scrolling
          // top: Math.floor((scrollTop + columnHeadersHeight - top) / this.defaultCellHeight) * this.defaultCellHeight // for discrete scroll
          top: Math.min(scrollTop + columnHeadersHeight - top, height - (lastChildSize || 0))
        } : {}}>
          {renderedCell}
        </div>
      </div>
    )
  }

  dimensionHeaderRenderer ({dimensionHeader, index, scrollLeft, scrollTop}) {
    const {rowHeadersWidth, columnHeadersHeight} = this.state

    const axe = dimensionHeader.axetype === 1 ? 'column' : 'row'

    let left
    let top
    if (axe === 'column') {
      left = scrollLeft + rowHeadersWidth - this.defaultCellWidth
      top = scrollTop + this.defaultCellHeight * index
    } else {
      left = scrollLeft + this.defaultCellWidth * index
      top = scrollTop + columnHeadersHeight - this.defaultCellHeight
    }
    return <div
      key={`fixed-dim-${axe}-${index}`}
      className={'ReactVirtualized__Grid__cell'}
      style={{
        position: 'fixed',
        left,
        top,
        width: this.defaultCellWidth,
        height: this.defaultCellHeight,
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
      {/* {axe === 'column' ? <RightArrow zoom={this.props.store.zoom} /> : <DownArrow zoom={this.props.store.zoom} />} */}
    </div>
  }

  _mockCellRenderer ({columnIndex, rowIndex}) {
    return 33
  }
}
