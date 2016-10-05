'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var DataCellComp = function (_Component) {
  _inherits(DataCellComp, _Component);

  function DataCellComp() {
    _classCallCheck(this, DataCellComp);

    return _possibleConstructorReturn(this, (DataCellComp.__proto__ || Object.getPrototypeOf(DataCellComp)).apply(this, arguments));
  }

  _createClass(DataCellComp, [{
    key: 'render',
    value: function render() {
      var _props = this.props;
      var cell = _props.cell;
      var onDoubleClick = _props.onDoubleClick;

      this._latestVisibleState = false;

      this._latestVisibleState = cell.visible();

      var value = cell.datafield && cell.datafield.formatFunc ? cell.datafield.formatFunc()(cell.value) : cell.value;

      var divcontent = [_react2.default.createElement(
        'div',
        { key: 'cell-value', ref: 'cellContent', className: 'cell-data' },
        value
      )];

      return _react2.default.createElement(
        'div',
        {
          style: { width: '100%', height: '100%', textAlign: 'right' },
          onDoubleClick: onDoubleClick
        },
        divcontent
      );
    }
  }]);

  return DataCellComp;
}(_react.Component);

exports.default = DataCellComp;