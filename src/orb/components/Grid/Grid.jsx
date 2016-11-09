import React, { Component } from 'react'
import {findDOMNode} from 'react-dom'
import { Grid as ReactVirtualizedGrid, AutoSizer } from 'react-virtualized'
import {DropTarget} from 'react-dnd'

import HeaderCellComp from '../HeaderCell'
import DataCellComp from '../DataCell'
import { DataCell } from '../../Cells'
import {isInRange} from '../../Utils'
import DragLayer from './DragLayer'
import ResizeHandle from './ResizeHandle'
import {AxisType} from '../../Axis'

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

function getLeafSubheaders (header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader => getLeafSubheaders(subheader, result))
    return result
  } else {
    result.push(header)
    return result
  }
}

export class Grid extends Component {
  constructor (props) {
    super(props)
    const {store} = props

    this.state = {
      rowVerticalCount: store.layout.rowVerticalCount,
      rowHorizontalCount: store.layout.rowHorizontalCount,
      columnVerticalCount: store.layout.columnVerticalCount,
      columnHorizontalCount: store.layout.columnHorizontalCount,
      cellsCache: {},
      selectedCellStart: null,
      selectedCellEnd: null
    }

    this.scrollLeft = 0
    this.scrollTop = 0

    this._isMouseDown = false

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this)
    this.dataCellRenderer = this.dataCellRenderer.bind(this)
    this.headerRenderer = this.headerRenderer.bind(this)
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

  componentWillReceiveProps (nextProps) {
    // Change scroll values to stay at the same position when modifying the layout
    this.scrollLeft = this._grid.state.scrollLeft * (this.props.store.layout.columnHorizontalCount / this.state.columnHorizontalCount)
    this.scrollTop = this._grid.state.scrollTop * (this.props.store.layout.rowVerticalCount / this.state.rowVerticalCount)

    this.setState({
      rowVerticalCount: nextProps.store.layout.rowVerticalCount,
      rowHorizontalCount: nextProps.store.layout.rowHorizontalCount,
      columnVerticalCount: nextProps.store.layout.columnVerticalCount,
      columnHorizontalCount: nextProps.store.layout.columnHorizontalCount,
      cellsCache: this._datacells || {} })
  }

  componentWillUpdate (nextProps, nextState) {
    this._isUpdating = true
    // Clean cache for cell sizes
    // Call forceUpdate on the grid, so cannot be done in render
    this._grid.recomputeGridSize()
  }

  componentDidUpdate (prevProps, prevState) {
    this._isUpdating = false
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
    if (e.button === 0 && this.state.selectedCellStart) {
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
        clipboardTextArea.innerHTML = getSelectedText({selectedCellStart, selectedCellEnd, store: this.props.store})
        clipboardTextArea.select()
        window.setTimeout(() => { bodyElement.removeChild(clipboardTextArea) }, 0)
      }
    } catch (error) {
      console.error('error in handleCopy', error)
    }
  }

  render () {
    const {connectDropTarget, store} = this.props
    const {columnHorizontalCount, columnVerticalCount, rowHorizontalCount, rowVerticalCount} = this.state
    return connectDropTarget(
        <div style={{height: 'inherit'}}>
          <DragLayer />
          <AutoSizer>
            {({width, height}) => {
              // Add 15 pixels to avoid that the grid go under a scrollbar
              // We should use the scrollbar size instead on Windows and Linux and nothing on macOs
              // TO SEE
              this.height = Math.min(height, store.sizes.rowHeadersHeight + store.sizes.columnHeadersHeight + 15)
              this.width = Math.min(width, store.sizes.columnHeadersWidth + store.sizes.rowHeadersWidth + 15)
              return (
                <ReactVirtualizedGrid
                  cellRangeRenderer={this.cellRangeRenderer}
                  cellRenderer={this._mockCellRenderer}
                  columnCount={columnHorizontalCount + rowHorizontalCount}
                  columnWidth={store.getColumnWidth.bind(store)}
                  height={this.height}
                  overscanRowCount={0}
                  overscanColumnCount={0}
                  ref={ref => { this._grid = ref }}
                  rowCount={rowVerticalCount + columnVerticalCount}
                  rowHeight={store.getRowHeight.bind(store)}
                  scrollLeft={this.scrollLeft}
                  scrollTop={this.scrollTop}
                  style={{fontSize: `${this.props.store.zoom*100}%`}}
                  width={this.width}
                />)
            }
            }
          </AutoSizer>
        </div>
        )
  }

  cellRangeRenderer ({ cellCache, cellClassName, cellRenderer, cellStyle, columnSizeAndPositionManager, columnStartIndex, columnStopIndex, horizontalOffsetAdjustment, isScrolling, rowSizeAndPositionManager, rowStartIndex, rowStopIndex, scrollLeft, scrollTop, verticalOffsetAdjustment }) {
    const {columnVerticalCount, rowHorizontalCount} = this.state
    const {store} = this.props
    const {columnsUi, rowsUi} = store
    const columnHeaders = columnsUi.headers
    const rowHeaders = rowsUi.headers
    const columnDimensionHeaders = columnsUi.dimensionHeaders
    const rowDimensionHeaders = rowsUi.dimensionHeaders


    const renderedCells = []

    // Because of the offset caused by the fixed headers, we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    columnStopIndex = Math.min(columnStopIndex, columnHeaders.length - 1)
    rowStopIndex = Math.min(rowStopIndex, rowHeaders.length - 1)

    const visibleRows = (rowStopIndex - rowStartIndex) + 1
    const visibleColumns = (columnStopIndex - columnStartIndex) + 1

    // Top-left corner piece
    renderedCells.push(
      <div
        key='fixed-fixed'
        className={'ReactVirtualized__Grid__cell'}
        style={{
          position: 'fixed',
          left: scrollLeft,
          top: scrollTop,
          width: this.props.store.sizes.rowHeadersWidth,
          height: this.props.store.sizes.columnHeadersHeight,
          zIndex: 2,
          backgroundColor: '#fff'
        }}>
      </div>
    )

    // Render dimension headers

    // Get width for column dimension headers
    let fieldWhoseWidthToGet
    if (store.config.dataHeadersLocation === 'rows') {
      // Dimension headers are on top of the measures column
      fieldWhoseWidthToGet = '__measures__'
    } else if (store.rows.fields.length) {
      // Dimension headers are on top of the column of the last field of the row headers
      fieldWhoseWidthToGet = store.rows.fields[store.rows.fields.length - 1].code
    } else {
      // Dimension headers are on top of the Total header --> get default width
      fieldWhoseWidthToGet = null
    }
    const width = this.props.store.getDimensionSize(AxisType.ROWS, fieldWhoseWidthToGet)
    const left = scrollLeft + this.props.store.sizes.rowHeadersWidth - width
    for (let dimensionHeader of columnDimensionHeaders) {
      const field = dimensionHeader.value
      const top = scrollTop + this.props.store.dimensionPositions.columns[field.code]
      const height = this.props.store.getDimensionSize(AxisType.COLUMNS, field.code)
      renderedCells.push(this.dimensionHeaderRenderer({left, top, width, height, field, mainDirection: 'right', crossFieldCode: fieldWhoseWidthToGet, scrollLeft, scrollTop}))
    }

    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet
    if (store.config.dataHeadersLocation === 'columns') {
      // Dimension headers are to the left of the measures row
      fieldWhoseHeightToGet = '__measures__'
    } else if (store.columns.fields.length) {
      // Dimension headers are to the left of the row of the last field of the column headers
      fieldWhoseHeightToGet = store.columns.fields[store.columns.fields.length - 1].code
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      fieldWhoseHeightToGet = null
    }
    const height = this.props.store.getDimensionSize(AxisType.COLUMNS, fieldWhoseHeightToGet)
    const top = scrollTop + this.props.store.sizes.columnHeadersHeight - height
    for (let dimensionHeader of rowDimensionHeaders) {
      const field = dimensionHeader.value
      const left = scrollLeft + this.props.store.dimensionPositions.rows[field.code]
      const width = this.props.store.getDimensionSize(AxisType.ROWS, field.code)
      renderedCells.push(this.dimensionHeaderRenderer({left, top, height, width, field, mainDirection: 'down', crossFieldCode: fieldWhoseHeightToGet, scrollLeft, scrollTop}))
    }

    // Render fixed header rows

    // Render big cells on top of current cells if necessary
    // The check on the presence of the header is necessary because it can be out of bounds when the headers array is modified
    if (columnHeaders[columnStartIndex] && columnHeaders[columnStartIndex].length < columnVerticalCount) {
      let header = columnHeaders[columnStartIndex][0]
      while (header.parent) {
        header = header.parent
        const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(header.x)
        const left = main.offset + horizontalOffsetAdjustment + this.props.store.sizes.rowHeadersWidth
        const span = header.hspan()
        const width = getHeaderSize(columnSizeAndPositionManager, header.x, span)
        const top = scrollTop + this.props.store.dimensionPositions.columns[header.dim.field.code]
        const height = this.props.store.getDimensionSize(AxisType.COLUMNS, header.dim.field.code)
        renderedCells.push(this.headerRenderer({axis: AxisType.COLUMNS,  header, left, top, width, height, span, startIndex: rowStartIndex, scrollLeft, scrollTop}))
      }
    }

    for (let columnIndex = columnStartIndex; columnIndex <= columnStopIndex; columnIndex++) {
      const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
      const left = main.offset + horizontalOffsetAdjustment + this.props.store.sizes.rowHeadersWidth
      for (let index = 0; index < columnHeaders[columnIndex].length; index++) {
        const header = columnHeaders[columnIndex][index]
        const span = header.hspan()
        const width = getHeaderSize(columnSizeAndPositionManager, columnIndex, span)
        // 3 cases: normal dimension header, measure header or total header
        let top = scrollTop
        let height
        if (!header.dim) {
          // Measure header
          height = this.props.store.getDimensionSize(AxisType.COLUMNS, '__measures__')
          top += this.props.store.dimensionPositions.columns['__measures__']
        } else if (header.dim.field) {
          // Normal dimension header
          height = this.props.store.getDimensionSize(AxisType.COLUMNS, header.dim.field.code)
          top += this.props.store.dimensionPositions.columns[header.dim.field.code]
        } else {
          // Total header
          height = this.props.store.getDimensionSize(AxisType.COLUMNS, '__total__')
        }
        renderedCells.push(this.headerRenderer({axis: AxisType.COLUMNS, header, left, top, width, height, span, startIndex: columnStartIndex, scrollLeft, scrollTop}))
      }
    }

    // Render fixed left columns

    // Render big cells on the left of current cells if necessary
    // The check on the presence of the header is necessary because it can be out of bounds when the headers array is modified
    if (rowHeaders[rowStartIndex] && rowHeaders[rowStartIndex].length < rowHorizontalCount) {
      let header = rowHeaders[rowStartIndex][0]
      while (header.parent) {
        header = header.parent
        const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(header.x)
        const span = header.vspan()
        const top = main.offset + verticalOffsetAdjustment + this.props.store.sizes.columnHeadersHeight
        const height = getHeaderSize(rowSizeAndPositionManager, header.x, span)
        const width = this.props.store.getDimensionSize(AxisType.ROWS, header.dim.field.code)
        const left = scrollLeft + this.props.store.dimensionPositions.rows[header.dim.field.code]
        renderedCells.push(this.headerRenderer({axis: AxisType.ROWS,  header, left, top, width, height, span, startIndex: rowStartIndex, scrollLeft, scrollTop}))
      }
    }

    for (let rowIndex = rowStartIndex; rowIndex <= rowStopIndex; rowIndex++) {
      const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
      const top = main.offset + verticalOffsetAdjustment + this.props.store.sizes.columnHeadersHeight
      for (let index = 0; index < rowHeaders[rowIndex].length; index++) {
        const header = rowHeaders[rowIndex][index]
        const span = header.vspan()
        const height = getHeaderSize(rowSizeAndPositionManager, rowIndex, span)
        // 3 cases: normal dimension header, measure header or total header
        let width
        let left = scrollLeft
        if (!header.dim) {
          // Measure header
          width = this.props.store.getDimensionSize(AxisType.ROWS, '__measures__')
          left += this.props.store.dimensionPositions.rows['__measures__']
        } else if (header.dim.field) {
          // Normal dimension header
          width = this.props.store.getDimensionSize(AxisType.ROWS, header.dim.field.code)
          left += this.props.store.dimensionPositions.rows[header.dim.field.code]
        } else {
          // Total header
          width = this.props.store.getDimensionSize(AxisType.ROWS, '__total__')
        }
        renderedCells.push(this.headerRenderer({axis: AxisType.ROWS,  header, left, top, width, height, span, startIndex: rowStartIndex, scrollLeft, scrollTop}))
      }
    }

    // Render data cells
    this._datacells = {}
    // if (!isScrolling) {
    for (let rowIndex = rowStartIndex; rowIndex <= rowStopIndex; rowIndex++) {
      let rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex)
      for (let columnIndex = columnStartIndex; columnIndex <= columnStopIndex; columnIndex++) {
        let columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex)
        renderedCells.push(this.dataCellRenderer({
          columnIndex,
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
    const {selectedCellStart, selectedCellEnd} = this.state
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
      overflow: 'hidden',
      position: 'fixed',
      height: rowDatum.size,
      width: columnDatum.size,
      // The modulos allow discrete scrolling
      // left: columnDatum.offset + this.props.store.sizes.rowHeadersWidth + horizontalOffsetAdjustment + (scrollLeft % this.defaultCellWidth),
      // top: rowDatum.offset + this.props.store.sizes.columnHeadersHeight + verticalOffsetAdjustment + (scrollTop % this.defaultCellHeight)
      left: columnDatum.offset + horizontalOffsetAdjustment + this.props.store.sizes.rowHeadersWidth,
      top: rowDatum.offset + verticalOffsetAdjustment + this.props.store.sizes.columnHeadersHeight
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
    this._datacells[key] = cell.value
    const renderedCell = <DataCellComp
      key={`data-${rowIndex % visibleRows}-${columnIndex % visibleColumns}`}
      cell={cell}
      onDoubleClick={() => drilldown(cell)}
                         />
    let valueHasChanged = false
    if (this._isUpdating) {
      const oldcell = this.state.cellsCache[key]
      if (oldcell !== undefined && cell.value !== oldcell) {
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

  headerRenderer ({axis, header, left, top, width, height, span, startIndex, scrollLeft, scrollTop}) {
    const {x, y} = header
    const renderedCell = <HeaderCellComp key={`${axis}-${x}-${y}`} cell={header} onToggle={() => 33} />
    let innerHeader = renderedCell
    let previewSize = {}
    if (axis === AxisType.COLUMNS) {
      previewSize.right = this.props.store.sizes.columnHeadersHeight
      previewSize.bottom = this.width
    } else {
      previewSize.right = this.height
      previewSize.bottom = this.props.store.sizes.rowHeadersWidth
    }
    // Handle affix
    if (span > 1) {
      const getLastChildSize = (axis, header) => {
        let lastChild = header
          while (lastChild.subheaders && lastChild.subheaders.length){
            lastChild = lastChild.subheaders[lastChild.subheaders.length - 1]
          }
          return this.props.store.getLeafHeaderSize(axis, header.key)
      }
      let lastChildSize
      let style
      let offset
      if (axis === AxisType.COLUMNS) {
        if (x <= startIndex) {
          lastChildSize = getLastChildSize(axis, header)
          offset = Math.min(scrollLeft + this.props.store.sizes.rowHeadersWidth - left, width - (lastChildSize || 0))
          style = {position: 'relative', left: offset}
        }
      } else {
        if (x <= startIndex) {
          lastChildSize = getLastChildSize(axis, header)
          offset = Math.min(scrollTop + this.props.store.sizes.columnHeadersHeight - top, height - (lastChildSize || 0))
          style = {position: 'relative', top: offset}
        }
      }
      innerHeader = <div style={style}>{renderedCell}</div>
    }
    const leafHeaderId = header.key
    let dimensionId
    if (!header.dim) {
      // Measure header
      dimensionId = '__measures__'
    } else if (header.dim.field) {
      // Normal header
      dimensionId = header.dim.field.code
    } else {
      // Total header
      dimensionId = '__total__'
    }
    const leafSubheaders = header.subheaders ? getLeafSubheaders(header, []) : []
    return (
      <div
        key={`fixed-${axis}-${x}-${y}`}
        className={'ReactVirtualized__Grid__cell'}
        style={{
          border: 'solid lightgrey thin',
          boxSizing: 'border-box',
          overflow: 'hidden',
          backgroundColor: '#eef8fb',
          position: 'fixed',
          left,
          top,
          height,
          width,
          zIndex: 1,
          display: 'flex',
        }}>
        {innerHeader}
        <ResizeHandle position='right' size={height} id={axis === AxisType.COLUMNS ? leafHeaderId : dimensionId} leafSubheaders={leafSubheaders} axis={axis} previewSize={previewSize.right} previewOffset={top - scrollTop}/>
        <ResizeHandle position='bottom' size={width} id={axis === AxisType.ROWS ? leafHeaderId : dimensionId} leafSubheaders={leafSubheaders} axis={axis} previewSize={previewSize.bottom} previewOffset={left - scrollLeft}/>
      </div>
    )
  }

  dimensionHeaderRenderer ({field, left, top, height, width, crossFieldCode, mainDirection, scrollLeft, scrollTop}) {
    const ids = {}
    if (mainDirection === 'down') {
      ids.right = field.code
      ids.bottom = crossFieldCode
    } else {
      ids.bottom = field.code
      ids.right = crossFieldCode
    }
    return <div
      key={`fixed-dim-${field.code}`}
      className={'ReactVirtualized__Grid__cell'}
      style={{
        position: 'fixed',
        left,
        top,
        width,
        height,
        zIndex: 3,
        border: 'lightgrey 0.1em solid',
        boxSizing: 'border-box',
        textAlign: 'left',
        display: 'flex',
        backgroundColor: '#fafad2',
      }}>
      <span>{field.caption}</span>
      <ResizeHandle position='right' size={height} id={ids.right} isOnDimensionHeader axis={AxisType.ROWS} previewSize={this.height} previewOffset={top - scrollTop}/>
      <ResizeHandle position='bottom' size={width} id={ids.bottom} isOnDimensionHeader axis={AxisType.COLUMNS} previewSize={this.width} previewOffset={left - scrollLeft}/>
      {/* {axis === 'column' ? <RightArrow zoom={store.zoom} /> : <DownArrow zoom={store.zoom} />} */}
    </div>
  }

  _mockCellRenderer ({columnIndex, rowIndex}) {
    return 33
  }
}

const gridSpec = {
  drop (props, monitor, component) {
    const handle = monitor.getItem()
    const initialOffset = monitor.getInitialClientOffset()
    const offset = monitor.getClientOffset()
    component.props.store.updateCellSizes(handle, offset, initialOffset)
  }
}

const collect = (connect, monitor) => ({
  connectDropTarget: connect.dropTarget()
})

export default DropTarget('cell-resize-handle', gridSpec, collect)(Grid)
