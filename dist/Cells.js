'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.EmptyCell = exports.ButtonCell = exports.DataCell = exports.DataHeader = exports.Header = exports.HeaderType = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Axe = require('./Axe');

var _orb = require('./orb.store');

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var HeaderType = exports.HeaderType = {
  EMPTY: 1,
  DATA_HEADER: 2,
  DATA_VALUE: 3,
  FIELD_BUTTON: 4,
  INNER: 5,
  WRAPPER: 6,
  SUB_TOTAL: 7,
  GRAND_TOTAL: 8,
  getHeaderClass: function getHeaderClass(headerType, axetype) {
    var cssclass = axetype === _Axe.AxeType.ROWS ? 'header-row' : axetype === _Axe.AxeType.COLUMNS ? 'header-col' : '';
    switch (headerType) {
      case HeaderType.EMPTY:
      case HeaderType.FIELD_BUTTON:
      default:
        cssclass = 'empty';
        break;
      case HeaderType.INNER:
        cssclass = 'header ' + cssclass;
        break;
      case HeaderType.WRAPPER:
        cssclass = 'header ' + cssclass;
        break;
      case HeaderType.SUB_TOTAL:
        cssclass = 'header header-st ' + cssclass;
        break;
      case HeaderType.GRAND_TOTAL:
        cssclass = 'header header-gt ' + cssclass;
        break;
    }

    return cssclass;
  },
  getCellClass: function getCellClass(rowHeaderType, colHeaderType) {
    var cssclass = '';
    switch (rowHeaderType) {
      case HeaderType.GRAND_TOTAL:
        cssclass = 'cell-gt';
        break;
      case HeaderType.SUB_TOTAL:
        if (colHeaderType === HeaderType.GRAND_TOTAL) {
          cssclass = 'cell-gt';
        } else {
          cssclass = 'cell-st';
        }
        break;
      default:
        if (colHeaderType === HeaderType.GRAND_TOTAL) {
          cssclass = 'cell-gt';
        } else if (colHeaderType === HeaderType.SUB_TOTAL) {
          cssclass = 'cell-st';
        } else {
          cssclass = '';
        }
    }
    return cssclass;
  }
};

var CellBase = function () {
  function CellBase(options) {
    _classCallCheck(this, CellBase);

    // CellBase is an abstract class
    // Symbol new.target does not pass in Uglify.js
    // if (new.target === CellBase) {
    //   throw new Error('CellBase is an abstract class and cannot be instantiated directly.')
    // }

    /**
     * axe type (COLUMNS, ROWS, DATA, ...)
     * @type {orb.AxeType}
     */
    this.axetype = options.axetype;
    /**
     * cell type (EMPTY, DATA_VALUE, FIELD_BUTTON, INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL, ...)
     * @type {HeaderType}
     */
    this.type = options.type;
    /**
     * header cell template
     * @type {String}
     */
    this.template = options.template;
    /**
     * header cell value
     * @type {Object}
     */
    this.value = options.value;
    /**
     * is header cell expanded
     * @type {Boolean}
     */
    this.expanded = true;
    /**
     * header cell css class(es)
     * @type {String}
     */
    this.cssclass = options.cssclass;
    /**
     * header cell width
     * @type {Number}
     */
    this.hspan = options.hspan || function () {
      return 1;
    };
    /**
     * gets header cell's height
     * @return {Number}
     */
    this.vspan = options.vspan || function () {
      return 1;
    };
    /**
     * gets wether header cell is visible
     * @return {Boolean}
     */
    this.visible = options.isvisible || function () {
      return true;
    };

    this.key = this.axetype + this.type + this.value;

    this.store = new _orb.Store();
  }

  _createClass(CellBase, [{
    key: 'getState',
    value: function getState() {
      return this.store.get(this.key);
    }
  }, {
    key: 'setState',
    value: function setState(newState) {
      this.store.set(this.key, newState);
    }
  }]);

  return CellBase;
}();

/**
 * Creates a new instance of a row header.
 * @class
 * @memberOf orb.ui
 * @param  {orb.ui.rowHeader} parent - parent header.
 * @param  {orb.dimension} dim - related dimension values container.
 * @param  {HeaderType} type - header type (INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL).
 * @param  {orb.ui.rowHeader} totalHeader - sub total or grand total related header.
 */


var Header = exports.Header = function (_CellBase) {
  _inherits(Header, _CellBase);

  function Header(axetype, headerTypeP, dim, parent, datafieldscount, x, y, subtotalHeader) {
    _classCallCheck(this, Header);

    var isRowsAxe = axetype === _Axe.AxeType.ROWS;
    var headerType = headerTypeP || (dim.depth === 1 ? HeaderType.INNER : HeaderType.WRAPPER);
    var value;
    var hspan;
    var vspan;

    switch (headerType) {
      case HeaderType.GRAND_TOTAL:
        value = 'Total';
        hspan = isRowsAxe ? dim.depth - 1 || 1 : datafieldscount;
        vspan = isRowsAxe ? datafieldscount : dim.depth - 1 || 1;
        break;
      case HeaderType.SUB_TOTAL:
        value = dim.value;
        hspan = isRowsAxe ? dim.depth : datafieldscount;
        vspan = isRowsAxe ? datafieldscount : dim.depth;
        break;
      default:
        value = dim.value;
        hspan = isRowsAxe ? 1 : null;
        vspan = isRowsAxe ? null : 1;
        break;
    }

    var options = {
      axetype: axetype,
      type: headerType,
      template: isRowsAxe ? 'cell-template-row-header' : 'cell-template-column-header',
      value: value,
      cssclass: HeaderType.getHeaderClass(headerType, axetype)
    };

    var _this = _possibleConstructorReturn(this, (Header.__proto__ || Object.getPrototypeOf(Header)).call(this, options));

    _this.isRowsAxe = isRowsAxe;
    _this.hspan = hspan != null ? function () {
      return hspan;
    } : _this.calcSpan;
    _this.vspan = vspan != null ? function () {
      return vspan;
    } : _this.calcSpan;
    _this.isvisible = _this.isParentExpanded;

    _this.subtotalHeader = subtotalHeader;
    _this.parent = parent;
    _this.dim = dim;
    _this.expanded = _this.getState() ? _this.getState().expanded : headerType !== HeaderType.SUB_TOTAL || !dim.field.subTotal.collapsed;
    _this.subheaders = [];

    if (parent != null) {
      _this.parent.subheaders.push(_this);
    }

    _this.datafieldscount = datafieldscount;

    _this.key = parent ? parent.key + '-/-' + _this.value : _this.value;

    _this.x = x;
    _this.y = y;
    return _this;
  }

  _createClass(Header, [{
    key: 'expand',
    value: function expand() {
      this.expanded = true;
      this.setState({
        expanded: this.expanded
      });
    }
  }, {
    key: 'collapse',
    value: function collapse() {
      this.expanded = false;
      this.setState({
        expanded: this.expanded
      });
    }
  }, {
    key: 'isParentExpanded',
    value: function isParentExpanded() {
      if (this.type === HeaderType.SUB_TOTAL) {
        var hparent = this.parent;
        while (hparent != null) {
          if (hparent.subtotalHeader && !hparent.subtotalHeader.expanded) {
            return false;
          }
          hparent = hparent.parent;
        }
        return true;
      } else {
        var isexpanded = this.dim.isRoot || this.dim.isLeaf || !this.dim.field.subTotal.visible || this.subtotalHeader.expanded;
        if (!isexpanded) {
          return false;
        }

        var par = this.parent;
        while (par != null && (!par.dim.field.subTotal.visible || par.subtotalHeader != null && par.subtotalHeader.expanded)) {
          par = par.parent;
        }
        return par == null || par.subtotalHeader == null ? isexpanded : par.subtotalHeader.expanded;
      }
    }
  }, {
    key: 'calcSpan',
    value: function calcSpan(ignoreVisibility) {
      // console.log('calcSpan')
      // console.log(this)
      var span = 0;
      var subSpan;
      var addone = false;

      if (this.isRowsAxe || ignoreVisibility || this.visible()) {
        if (!this.dim.isLeaf) {
          // subdimvals 'own' properties are the set of values for this dimension
          if (this.subheaders.length > 0) {
            for (var i = 0; i < this.subheaders.length; i++) {
              var subheader = this.subheaders[i];
              // if it's not an array
              if (!subheader.dim.isLeaf) {
                subSpan = this.isRowsAxe ? subheader.vspan() : subheader.hspan();
                span += subSpan;
                if (i === 0 && subSpan === 0) {
                  addone = true;
                }
              } else {
                span += this.datafieldscount || 1;
              }
            }
          } else {
            span += this.datafieldscount || 1;
          }
        } else {
          return this.datafieldscount || 1;
        }
        return span + (addone ? 1 : 0);
      }
      return span;
    }
  }]);

  return Header;
}(CellBase);

var DataHeader = exports.DataHeader = function (_CellBase2) {
  _inherits(DataHeader, _CellBase2);

  function DataHeader(axetype, datafield, parent, x, y) {
    _classCallCheck(this, DataHeader);

    var _this2 = _possibleConstructorReturn(this, (DataHeader.__proto__ || Object.getPrototypeOf(DataHeader)).call(this, {
      axetype: axetype,
      type: HeaderType.DATA_HEADER,
      template: 'cell-template-dataheader',
      value: datafield,
      cssclass: HeaderType.getHeaderClass(parent.type, parent.axetype),
      isvisible: parent.visible
    }));

    _this2.parent = parent;

    _this2.key = parent ? parent.key + '-/-' + datafield.name : datafield.name;

    _this2.x = x;
    _this2.y = y;
    return _this2;
  }

  return DataHeader;
}(CellBase);

var DataCell = exports.DataCell = function (_CellBase3) {
  _inherits(DataCell, _CellBase3);

  function DataCell(store, isvisible, rowinfo, colinfo) {
    _classCallCheck(this, DataCell);

    var rowDimension = rowinfo.type === HeaderType.DATA_HEADER ? rowinfo.parent.dim : rowinfo.dim;
    var columnDimension = colinfo.type === HeaderType.DATA_HEADER ? colinfo.parent.dim : colinfo.dim;
    var rowType = rowinfo.type === HeaderType.DATA_HEADER ? rowinfo.parent.type : rowinfo.type;
    var colType = colinfo.type === HeaderType.DATA_HEADER ? colinfo.parent.type : colinfo.type;

    var datafield = store.config.activatedDataFieldsCount > 1 ? store.config.dataHeadersLocation === 'rows' ? rowinfo.value : colinfo.value : store.config.activatedDataFields[0];

    var _this3 = _possibleConstructorReturn(this, (DataCell.__proto__ || Object.getPrototypeOf(DataCell)).call(this, {
      axetype: null,
      type: HeaderType.DATA_VALUE,
      template: 'cell-template-datavalue',
      value: store.getData(datafield ? datafield.name : null, rowDimension, columnDimension),
      cssclass: 'cell ' + HeaderType.getCellClass(rowType, colType),
      isvisible: isvisible
    }));

    _this3.rowDimension = rowDimension;
    _this3.columnDimension = columnDimension;
    _this3.rowType = rowType;
    _this3.colType = colType;
    _this3.datafield = datafield;
    return _this3;
  }

  return DataCell;
}(CellBase);

var ButtonCell = exports.ButtonCell = function (_CellBase4) {
  _inherits(ButtonCell, _CellBase4);

  function ButtonCell(field) {
    _classCallCheck(this, ButtonCell);

    return _possibleConstructorReturn(this, (ButtonCell.__proto__ || Object.getPrototypeOf(ButtonCell)).call(this, {
      axetype: null,
      type: HeaderType.FIELD_BUTTON,
      template: 'cell-template-fieldbutton',
      value: field,
      cssclass: HeaderType.getHeaderClass(HeaderType.FIELD_BUTTON)
    }));
  }

  return ButtonCell;
}(CellBase);

var EmptyCell = exports.EmptyCell = function (_CellBase5) {
  _inherits(EmptyCell, _CellBase5);

  function EmptyCell(hspan, vspan) {
    _classCallCheck(this, EmptyCell);

    return _possibleConstructorReturn(this, (EmptyCell.__proto__ || Object.getPrototypeOf(EmptyCell)).call(this, {
      axetype: null,
      type: HeaderType.EMPTY,
      template: 'cell-template-empty',
      value: null,
      cssclass: HeaderType.getHeaderClass(HeaderType.EMPTY),
      hspan: function (_hspan) {
        function hspan() {
          return _hspan.apply(this, arguments);
        }

        hspan.toString = function () {
          return _hspan.toString();
        };

        return hspan;
      }(function () {
        return hspan;
      }),
      vspan: function (_vspan) {
        function vspan() {
          return _vspan.apply(this, arguments);
        }

        vspan.toString = function () {
          return _vspan.toString();
        };

        return vspan;
      }(function () {
        return vspan;
      })
    }));
  }

  return EmptyCell;
}(CellBase);