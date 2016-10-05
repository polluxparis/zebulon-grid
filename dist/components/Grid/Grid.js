'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactVirtualized = require('react-virtualized');

var _HeaderCell = require('../HeaderCell');

var _HeaderCell2 = _interopRequireDefault(_HeaderCell);

var _DataCell = require('../DataCell');

var _DataCell2 = _interopRequireDefault(_DataCell);

var _Cells = require('../../Cells');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var OrbGrid = function (_Component) {
  _inherits(OrbGrid, _Component);

  _createClass(OrbGrid, [{
    key: 'componentWillReceiveProps',
    value: function componentWillReceiveProps(nextProps, nextState) {
      var layout = this.getLayout(nextProps.store);

      this.setState(_extends({}, layout, { cellsCache: this._datacells || new Map() }));

      // Change scroll values to stay at the same position when modifying the layout
      this.scrollLeft = this._grid.state.scrollLeft * (layout.columnHorizontalCount / this.state.columnHorizontalCount);
      this.scrollTop = this._grid.state.scrollTop * (layout.rowVerticalCount / this.state.rowVerticalCount);
    }
  }, {
    key: 'componentWillUpdate',
    value: function componentWillUpdate(nextProps, nextState) {
      this._isUpdating = true;
      // to handle case where all data fields are unactivated
      this._grid.forceUpdate();
    }
  }, {
    key: 'componentDidUpdate',
    value: function componentDidUpdate(prevProps, prevState) {
      this._isUpdating = false;
    }
  }]);

  function OrbGrid(props) {
    _classCallCheck(this, OrbGrid);

    var _this = _possibleConstructorReturn(this, (OrbGrid.__proto__ || Object.getPrototypeOf(OrbGrid)).call(this, props));

    _this.state = _extends({}, _this.getLayout(props.store), {
      cellsCache: new Map()
    });

    _this.scrollLeft = 0;
    _this.scrollTop = 0;

    _this.cellRangeRenderer = _this.cellRangeRenderer.bind(_this);
    _this.dataCellRenderer = _this.dataCellRenderer.bind(_this);
    _this.rowHeaderRenderer = _this.rowHeaderRenderer.bind(_this);
    _this.columnHeaderRenderer = _this.columnHeaderRenderer.bind(_this);
    return _this;
  }

  _createClass(OrbGrid, [{
    key: 'getLayout',
    value: function getLayout(_ref) {
      var layout = _ref.layout;
      var sizes = _ref.sizes;

      var cellHeight = sizes.cell.height;
      var cellWidth = sizes.cell.width;

      var rowVerticalCount = layout.rowHeaders.height;
      var rowHorizontalCount = layout.rowHeaders.width;
      var columnVerticalCount = layout.columnHeaders.height;
      var columnHorizontalCount = layout.columnHeaders.width;

      var rowHeadersWidth = rowHorizontalCount * cellWidth;
      var rowHeadersHeight = rowVerticalCount * cellHeight;
      var columnHeadersHeight = columnVerticalCount * cellHeight;
      var columnHeadersWidth = columnHorizontalCount * cellWidth;

      var height = Math.min(sizes.grid.height, columnHeadersHeight + rowHeadersHeight);
      var width = Math.min(sizes.grid.width, rowHeadersWidth + columnHeadersWidth);

      return {
        cellHeight: cellHeight,
        cellWidth: cellWidth,
        rowVerticalCount: rowVerticalCount,
        rowHorizontalCount: rowHorizontalCount,
        columnVerticalCount: columnVerticalCount,
        columnHorizontalCount: columnHorizontalCount,
        rowHeadersWidth: rowHeadersWidth,
        rowHeadersHeight: rowHeadersHeight,
        columnHeadersHeight: columnHeadersHeight,
        columnHeadersWidth: columnHeadersWidth,
        height: height,
        width: width
      };
    }
  }, {
    key: 'render',
    value: function render() {
      var _this2 = this;

      var _state = this.state;
      var cellHeight = _state.cellHeight;
      var cellWidth = _state.cellWidth;
      var columnHeadersHeight = _state.columnHeadersHeight;
      var columnHeadersWidth = _state.columnHeadersWidth;
      var columnHorizontalCount = _state.columnHorizontalCount;
      var columnVerticalCount = _state.columnVerticalCount;
      var rowHeadersHeight = _state.rowHeadersHeight;
      var rowHeadersWidth = _state.rowHeadersWidth;
      var rowHorizontalCount = _state.rowHorizontalCount;
      var rowVerticalCount = _state.rowVerticalCount;

      return _react2.default.createElement(
        _reactVirtualized.AutoSizer,
        null,
        function (_ref2) {
          var width = _ref2.width;
          var height = _ref2.height;
          return _react2.default.createElement(_reactVirtualized.Grid, {
            cellRangeRenderer: _this2.cellRangeRenderer,
            cellRenderer: _this2._mockCellRenderer,
            columnCount: columnHorizontalCount + rowHorizontalCount,
            columnWidth: cellWidth,
            height: Math.min(height, columnHeadersHeight + rowHeadersHeight),
            overscanRowCount: 0,
            overscanColumnCount: 0,
            ref: function ref(_ref3) {
              _this2._grid = _ref3;
            },
            rowCount: columnVerticalCount + rowVerticalCount,
            rowHeight: cellHeight,
            scrollLeft: _this2.scrollLeft,
            scrollTop: _this2.scrollTop,
            width: Math.min(width, rowHeadersWidth + columnHeadersWidth)
          });
        }
      );
    }
  }, {
    key: 'cellRangeRenderer',
    value: function cellRangeRenderer(_ref4) {
      var cellCache = _ref4.cellCache;
      var cellClassName = _ref4.cellClassName;
      var cellRenderer = _ref4.cellRenderer;
      var cellStyle = _ref4.cellStyle;
      var columnSizeAndPositionManager = _ref4.columnSizeAndPositionManager;
      var columnStartIndex = _ref4.columnStartIndex;
      var columnStopIndex = _ref4.columnStopIndex;
      var horizontalOffsetAdjustment = _ref4.horizontalOffsetAdjustment;
      var isScrolling = _ref4.isScrolling;
      var rowSizeAndPositionManager = _ref4.rowSizeAndPositionManager;
      var rowStartIndex = _ref4.rowStartIndex;
      var rowStopIndex = _ref4.rowStopIndex;
      var scrollLeft = _ref4.scrollLeft;
      var scrollTop = _ref4.scrollTop;
      var verticalOffsetAdjustment = _ref4.verticalOffsetAdjustment;
      var _props$store = this.props.store;
      var columnsUi = _props$store.columnsUi;
      var rowsUi = _props$store.rowsUi;

      var columnHeaders = columnsUi.headers;
      var rowHeaders = rowsUi.headers;
      var _state2 = this.state;
      var columnHeadersHeight = _state2.columnHeadersHeight;
      var columnHorizontalCount = _state2.columnHorizontalCount;
      var columnVerticalCount = _state2.columnVerticalCount;
      var rowHeadersWidth = _state2.rowHeadersWidth;
      var rowHorizontalCount = _state2.rowHorizontalCount;
      var rowVerticalCount = _state2.rowVerticalCount;

      var renderedCells = [];

      // to avoid rendering empty cells
      // there is a difference between columnCount (the prop of the Grid object) and the column count except the row headers
      // the -1 is here because there are inferior or equal signs in the loops
      var _columnStopIndex = Math.min(columnStopIndex - rowHorizontalCount, columnHorizontalCount - 1);
      var _rowStopIndex = Math.min(rowStopIndex - columnVerticalCount, rowVerticalCount - 1);

      var visibleRows = _rowStopIndex - rowStartIndex + 1;
      var visibleColumns = _columnStopIndex - columnStartIndex + 1;

      // Top-left corner piece
      renderedCells.push(_react2.default.createElement('div', {
        key: 'fixed-fixed',
        className: 'ReactVirtualized__Grid__cell',
        style: {
          position: 'fixed',
          left: scrollLeft,
          top: scrollTop,
          width: rowHeadersWidth,
          height: columnHeadersHeight,
          zIndex: 2,
          backgroundColor: '#fff'
        } }));

      // Render fixed header rows

      // Render big cells on top of current cells if necessary
      if (columnHeaders[columnStartIndex].length < columnVerticalCount) {
        var columnHeader = columnHeaders[columnStartIndex][0];
        while (columnHeader.parent) {
          columnHeader = columnHeader.parent;
          renderedCells.push(this.columnHeaderRenderer({ columnHeader: columnHeader, scrollLeft: scrollLeft, scrollTop: scrollTop, columnStartIndex: columnStartIndex, visibleColumns: visibleColumns, horizontalOffsetAdjustment: horizontalOffsetAdjustment }));
        }
      }

      for (var columnIndex = columnStartIndex; columnIndex <= _columnStopIndex; columnIndex++) {
        for (var columnHeaderIndex = 0; columnHeaderIndex < columnHeaders[columnIndex].length; columnHeaderIndex++) {
          var _columnHeader = columnHeaders[columnIndex][columnHeaderIndex];
          renderedCells.push(this.columnHeaderRenderer({ columnHeader: _columnHeader, scrollLeft: scrollLeft, scrollTop: scrollTop, columnStartIndex: columnStartIndex, visibleColumns: visibleColumns, horizontalOffsetAdjustment: horizontalOffsetAdjustment }));
        }
      }

      // Render fixed left columns

      // Render big cells on the left of current cells if necessary
      if (rowHeaders[rowStartIndex].length < rowHorizontalCount) {
        var rowHeader = rowHeaders[rowStartIndex][0];
        while (rowHeader.parent) {
          rowHeader = rowHeader.parent;
          renderedCells.push(this.rowHeaderRenderer({ rowHeader: rowHeader, scrollLeft: scrollLeft, scrollTop: scrollTop, rowStartIndex: rowStartIndex, visibleRows: visibleRows, verticalOffsetAdjustment: verticalOffsetAdjustment }));
        }
      }

      for (var rowIndex = rowStartIndex; rowIndex <= _rowStopIndex; rowIndex++) {
        for (var rowHeaderIndex = 0; rowHeaderIndex < rowHeaders[rowIndex].length; rowHeaderIndex++) {
          var _rowHeader = rowHeaders[rowIndex][rowHeaderIndex];
          renderedCells.push(this.rowHeaderRenderer({ rowHeader: _rowHeader, scrollLeft: scrollLeft, scrollTop: scrollTop, rowStartIndex: rowStartIndex, visibleRows: visibleRows, verticalOffsetAdjustment: verticalOffsetAdjustment }));
        }
      }

      // Render data cells
      this._datacells = new Map();
      // if (!isScrolling) {
      for (var _rowIndex = rowStartIndex; _rowIndex <= _rowStopIndex; _rowIndex++) {
        var rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(_rowIndex);
        for (var _columnIndex = columnStartIndex; _columnIndex <= _columnStopIndex; _columnIndex++) {
          var columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(_columnIndex);
          renderedCells.push(this.dataCellRenderer({ columnIndex: _columnIndex, rowIndex: _rowIndex, columnDatum: columnDatum, rowDatum: rowDatum, scrollLeft: scrollLeft, scrollTop: scrollTop, horizontalOffsetAdjustment: horizontalOffsetAdjustment, visibleRows: visibleRows, visibleColumns: visibleColumns, verticalOffsetAdjustment: verticalOffsetAdjustment }));
        }
      }
      // }

      return renderedCells;
    }
  }, {
    key: 'dataCellRenderer',
    value: function dataCellRenderer(_ref5) {
      var columnIndex = _ref5.columnIndex;
      var rowIndex = _ref5.rowIndex;
      var columnDatum = _ref5.columnDatum;
      var rowDatum = _ref5.rowDatum;
      var horizontalOffsetAdjustment = _ref5.horizontalOffsetAdjustment;
      var visibleRows = _ref5.visibleRows;
      var visibleColumns = _ref5.visibleColumns;
      var verticalOffsetAdjustment = _ref5.verticalOffsetAdjustment;
      var scrollTop = _ref5.scrollTop;
      var scrollLeft = _ref5.scrollLeft;
      var _state3 = this.state;
      var cellHeight = _state3.cellHeight;
      var cellWidth = _state3.cellWidth;
      var rowHeadersWidth = _state3.rowHeadersWidth;
      var columnHeadersHeight = _state3.columnHeadersHeight;
      var _props = this.props;
      var store = _props.store;
      var drilldown = _props.drilldown;
      var rowsUi = store.rowsUi;
      var columnsUi = store.columnsUi;

      var rowHeaderRow = rowsUi.headers[rowIndex];
      var rowHeader = rowHeaderRow[rowHeaderRow.length - 1];
      var columnHeaderColumn = columnsUi.headers[columnIndex];
      var columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1];
      var cell = new _Cells.DataCell(store, function () {
        return rowHeader.visible() && columnHeader.visible();
      }, rowHeader, columnHeader);
      var style = {
        border: 'solid lightgrey thin',
        boxSizing: 'border-box',
        padding: '0.2em',
        overflow: 'hidden',
        position: 'fixed',
        height: cellHeight,
        width: cellWidth,
        // The modulos allow discrete scrolling
        left: columnDatum.offset + rowHeadersWidth + horizontalOffsetAdjustment + scrollLeft % cellWidth,
        top: rowDatum.offset + columnHeadersHeight + verticalOffsetAdjustment + scrollTop % cellHeight
      };
      var unEvenRowStyle = {
        backgroundColor: 'rgba(211, 211, 211, 0.4)'
      };
      var evenRowStyle = {
        backgroundColor: 'white'
      };
      if (rowIndex % 2) {
        style = _extends({}, style, unEvenRowStyle);
      } else {
        style = _extends({}, style, evenRowStyle);
      }
      var key = rowHeader.key + '-//-' + columnHeader.key;
      this._datacells.set(key, cell);
      var renderedCell = _react2.default.createElement(_DataCell2.default, { key: 'data-' + rowIndex % visibleRows + '-' + columnIndex % visibleColumns, cell: cell, onDoubleClick: function onDoubleClick() {
          return drilldown(cell);
        } });
      var valueHasChanged = false;
      if (this._isUpdating) {
        var oldcell = this.state.cellsCache.get(key);
        if (oldcell && cell.value !== oldcell.value) {
          valueHasChanged = true;
        }
      }
      return _react2.default.createElement(
        'div',
        {
          key: rowIndex % visibleRows + '-' + columnIndex % visibleColumns,
          className: valueHasChanged ? 'ReactVirtualized__Grid__cell highlighted' : 'ReactVirtualized__Grid__cell normal',
          style: style },
        renderedCell
      );
    }
  }, {
    key: 'columnHeaderRenderer',
    value: function columnHeaderRenderer(_ref6) {
      var columnHeader = _ref6.columnHeader;
      var scrollLeft = _ref6.scrollLeft;
      var scrollTop = _ref6.scrollTop;
      var columnStartIndex = _ref6.columnStartIndex;
      var visibleColumns = _ref6.visibleColumns;
      var horizontalOffsetAdjustment = _ref6.horizontalOffsetAdjustment;
      var _state4 = this.state;
      var cellWidth = _state4.cellWidth;
      var cellHeight = _state4.cellHeight;
      var rowHeadersWidth = _state4.rowHeadersWidth;
      var x = columnHeader.x;
      var y = columnHeader.y;

      var renderedCell = _react2.default.createElement(_HeaderCell2.default, { key: 'row-' + x % visibleColumns + '-' + y, cell: columnHeader, onToggle: function onToggle() {
          return 33;
        } });
      var left = x * cellWidth + rowHeadersWidth + horizontalOffsetAdjustment;
      var width = cellWidth * columnHeader.hspan();
      var affix = width > cellWidth && x <= columnStartIndex;
      return _react2.default.createElement(
        'div',
        {
          // add 1 to key modulo to avoid collision when rendering parent cells
          key: 'fixedrow-' + x % (visibleColumns + 1) + '-' + y,
          className: 'ReactVirtualized__Grid__cell',
          style: {
            border: 'solid lightgrey thin',
            boxSizing: 'border-box',
            padding: '0.2em',
            overflow: 'hidden',
            position: 'fixed',
            // to have discrete scroll
            left: left + scrollLeft % cellWidth,
            top: y * cellHeight + scrollTop,
            height: cellHeight * columnHeader.vspan(),
            width: width,
            zIndex: 1,
            backgroundColor: '#eef8fb'
          } },
        _react2.default.createElement(
          'div',
          { style: affix ? {
              position: 'relative',
              // to keep the label visible upon scrolling
              left: scrollLeft % width - scrollLeft % width % cellWidth
            } : {} },
          renderedCell
        )
      );
    }
  }, {
    key: 'rowHeaderRenderer',
    value: function rowHeaderRenderer(_ref7) {
      var rowHeader = _ref7.rowHeader;
      var scrollLeft = _ref7.scrollLeft;
      var scrollTop = _ref7.scrollTop;
      var rowStartIndex = _ref7.rowStartIndex;
      var visibleRows = _ref7.visibleRows;
      var visibleColumns = _ref7.visibleColumns;
      var verticalOffsetAdjustment = _ref7.verticalOffsetAdjustment;
      var _state5 = this.state;
      var cellWidth = _state5.cellWidth;
      var cellHeight = _state5.cellHeight;
      var columnHeadersHeight = _state5.columnHeadersHeight;
      var x = rowHeader.x;
      var y = rowHeader.y;

      var renderedCell = _react2.default.createElement(_HeaderCell2.default, { key: 'col-' + x % visibleRows + '-' + y, cell: rowHeader, onToggle: function onToggle() {
          return 33;
        } });
      var top = x * cellHeight + columnHeadersHeight + verticalOffsetAdjustment;
      var height = cellHeight * rowHeader.vspan();
      var affix = height > cellHeight && x <= rowStartIndex;
      return _react2.default.createElement(
        'div',
        {
          // add 1 to key modulo to avoid collision when rendering parent cells
          key: 'fixedcol-' + x % (visibleRows + 1) + '-' + y,
          className: 'ReactVirtualized__Grid__cell',
          style: {
            border: 'solid lightgrey thin',
            boxSizing: 'border-box',
            padding: '0.2em',
            overflow: 'hidden',
            position: 'fixed',
            left: y * cellWidth + scrollLeft,
            // to have discrete scroll
            top: top + scrollTop % cellHeight,
            height: height,
            width: cellWidth * rowHeader.hspan(),
            zIndex: 1,
            backgroundColor: '#eef8fb'
          } },
        _react2.default.createElement(
          'div',
          { style: affix ? {
              position: 'relative',
              // to keep the label visible upon scrolling
              top: scrollTop % height - scrollTop % height % cellHeight
            } : {} },
          renderedCell
        )
      );
    }
  }, {
    key: '_mockCellRenderer',
    value: function _mockCellRenderer(_ref8) {
      var columnIndex = _ref8.columnIndex;
      var rowIndex = _ref8.rowIndex;

      return 33;
    }
  }]);

  return OrbGrid;
}(_react.Component);

exports.default = OrbGrid;