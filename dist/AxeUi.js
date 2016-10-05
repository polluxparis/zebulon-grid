'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _Axe = require('./Axe');

var _Cells = require('./Cells');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
var AxeUi = function () {
  function AxeUi(axeModel) {
    _classCallCheck(this, AxeUi);

    /**
     * Dimensions axe
     * @type {orb.axe}
     */
    this.axe = axeModel;

    /**
     * Headers render properties
     * @type {Array}
     */
    this.headers = [];

    this._x = 0;

    this.build();
  }

  _createClass(AxeUi, [{
    key: 'build',
    value: function build() {
      var headers = [];
      var grandtotalHeader = void 0;
      var y = void 0;

      if (this.axe != null) {
        if (this.axe.root.values.length > 0 || this.axe.store.config.grandTotal.rowsvisible) {
          headers.push([]);

          // Fill Rows layout infos
          y = this.getUiInfo(headers, this.axe.root, this.axe.type);

          if (this.axe.store.config.grandTotal.rowsvisible) {
            var lastrow = headers[headers.length - 1];
            grandtotalHeader = new _Cells.Header(this.axe.type, _Cells.HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount(), this._x, y);
            if (lastrow.length === 0) {
              lastrow.push(grandtotalHeader);
            } else {
              headers.push([grandtotalHeader]);
            }
          }
        }

        if (headers.length === 0) {
          headers.push([grandtotalHeader = new _Cells.Header(this.axe.type, _Cells.HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount(), this._x, y = 0)]);
        }

        if (grandtotalHeader) {
          // add grand-total data headers if more than 1 data field and they will be the leaf headers
          this.addDataHeaders(headers, grandtotalHeader, y + 1);
        }
      }
      this.headers = headers;
    }
  }, {
    key: 'addDataHeaders',
    value: function addDataHeaders(infos, parent, y) {
      if (this.isMultiDataFields()) {
        var lastInfosArray = infos[infos.length - 1];
        for (var datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
          lastInfosArray.push(new _Cells.DataHeader(this.axe.store.config.dataHeadersLocation === 'columns' ? _Axe.AxeType.COLUMNS : _Axe.AxeType.ROWS, this.axe.store.config.activatedDataFields[datafieldindex], parent, this._x++, y));
          if (datafieldindex < this.dataFieldsCount() - 1) {
            infos.push(lastInfosArray = []);
          }
        }
      } else {
        this._x++;
      }
    }

    /**
     * Fills the infos array given in argument with the dimension layout infos as row.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {object}  infos - array to fill with ui dimension info
     * @param  {number}  axetype - type of the axe (rows or columns)
     */

  }, {
    key: 'getUiInfo',
    value: function getUiInfo(infos, dimension, axetype) {
      var y = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;

      if (dimension.values.length > 0) {
        var infosMaxIndex = infos.length - 1;
        var lastInfosArray = infos[infosMaxIndex];
        var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

        for (var valIndex = 0; valIndex < dimension.values.length; valIndex++) {
          var subvalue = dimension.values[valIndex];
          var subdim = dimension.subdimvals[subvalue];

          var subTotalHeader;
          if (!subdim.isLeaf && subdim.field.subTotal.visible) {
            // x here will probably create bugs. To change when necessary
            subTotalHeader = new _Cells.Header(axetype, _Cells.HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount(), this._x, y);
          } else {
            subTotalHeader = null;
          }

          var newHeader = new _Cells.Header(axetype, null, subdim, parent, this.dataFieldsCount(), this._x, y, subTotalHeader);

          if (valIndex > 0) {
            infos.push(lastInfosArray = []);
          }

          lastInfosArray.push(newHeader);

          if (!subdim.isLeaf) {
            this.getUiInfo(infos, subdim, axetype, y + 1);
            if (subdim.field.subTotal.visible) {
              infos.push([subTotalHeader]);

              // add sub-total data headers if more than 1 data field and they will be the leaf headers
              this.addDataHeaders(infos, subTotalHeader, y + 1);
            }
          } else {
            // add data headers if more than 1 data field and they will be the leaf headers
            this.addDataHeaders(infos, newHeader, y + 1);
          }
        }
      }
      return y;
    }
  }, {
    key: 'dataFieldsCount',
    value: function dataFieldsCount() {
      return this.axe.store.config.dataHeadersLocation === 'columns' && this.axe.type === _Axe.AxeType.COLUMNS || this.axe.store.config.dataHeadersLocation === 'rows' && this.axe.type === _Axe.AxeType.ROWS ? this.axe.store.config.activatedDataFieldsCount : 1;
    }
  }, {
    key: 'isMultiDataFields',
    value: function isMultiDataFields() {
      return this.dataFieldsCount() > 1;
    }
  }, {
    key: 'toggleFieldExpansion',
    value: function toggleFieldExpansion(field, newState) {
      var toToggle = [];
      var allExpanded = true;
      var hIndex;

      for (var i = 0; i < this.headers.length; i++) {
        for (hIndex = 0; hIndex < this.headers[i].length; hIndex++) {
          var header = this.headers[i][hIndex];
          if (header.type === _Cells.HeaderType.SUB_TOTAL && (field == null || header.dim.field.name === field.name)) {
            toToggle.push(header);
            allExpanded = allExpanded && header.expanded;
          }
        }
      }

      if (newState !== undefined) {
        allExpanded = !newState;
      }

      if (toToggle.length > 0) {
        for (hIndex = 0; hIndex < toToggle.length; hIndex++) {
          if (allExpanded) {
            toToggle[hIndex].collapse();
          } else {
            toToggle[hIndex].expand();
          }
        }
        return true;
      }

      return false;
    }
  }]);

  return AxeUi;
}();

exports.default = AxeUi;