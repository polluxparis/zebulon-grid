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

var DataButton = function (_Component) {
  _inherits(DataButton, _Component);

  function DataButton(props) {
    _classCallCheck(this, DataButton);

    var _this = _possibleConstructorReturn(this, (DataButton.__proto__ || Object.getPrototypeOf(DataButton)).call(this, props));

    _this.onClick = _this.onClick.bind(_this);
    return _this;
  }

  _createClass(DataButton, [{
    key: 'onClick',
    value: function onClick() {
      var _props = this.props;
      var field = _props.field;
      var store = _props.store;

      store.toggleDataField(field.name);
    }
  }, {
    key: 'render',
    value: function render() {
      var _props2 = this.props;
      var active = _props2.active;
      var field = _props2.field;

      var fieldAggFunc = _react2.default.createElement(
        'small',
        null,
        ' (' + field.aggregateFuncName + ')'
      );
      var inactiveStyle = {
        backgroundColor: '#cccccc',
        borderRadius: 4,
        padding: 4,
        cursor: 'default'
      };
      var activeStyle = {
        border: 'solid #cccccc 1px',
        borderRadius: 4,
        padding: 4,
        cursor: 'default'
      };
      return _react2.default.createElement(
        'div',
        { style: active ? activeStyle : inactiveStyle, onClick: this.onClick },
        field.caption,
        fieldAggFunc
      );
    }
  }]);

  return DataButton;
}(_react.Component);

exports.default = DataButton;