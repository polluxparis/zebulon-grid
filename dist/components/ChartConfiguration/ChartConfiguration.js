'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _FixedFieldButton = require('../FixedFieldButton');

var _FixedFieldButton2 = _interopRequireDefault(_FixedFieldButton);

var _DataButton = require('../DataButton');

var _DataButton2 = _interopRequireDefault(_DataButton);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var ChartConfiguration = function (_Component) {
  _inherits(ChartConfiguration, _Component);

  function ChartConfiguration(props) {
    _classCallCheck(this, ChartConfiguration);

    var _this = _possibleConstructorReturn(this, (ChartConfiguration.__proto__ || Object.getPrototypeOf(ChartConfiguration)).call(this, props));

    _this.clickButton = _this.clickButton.bind(_this);
    return _this;
  }

  _createClass(ChartConfiguration, [{
    key: 'clickButton',
    value: function clickButton(buttonId) {
      var store = this.props.store;

      if (store.config.selectedField.name !== buttonId) {
        store.selectField(buttonId);
      }
    }
  }, {
    key: 'render',
    value: function render() {
      var _this2 = this;

      var store = this.props.store;
      var config = store.config;

      var rowButtons = config.allFields.map(function (field, index) {
        return _react2.default.createElement(
          'div',
          { key: field.name, style: { padding: '0px 4px' }, onClick: function onClick() {
              return _this2.clickButton(field.name);
            } },
          _react2.default.createElement(_FixedFieldButton2.default, {
            key: field.name,
            field: field,
            position: index,
            isSelected: field.name === config.selectedField.name,
            store: store })
        );
      });

      var fieldList = _react2.default.createElement(
        'div',
        null,
        _react2.default.createElement('div', { style: { padding: '7px 4px' }, className: 'flds-grp-cap text-muted' }),
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px', display: 'flex' } },
          rowButtons
        )
      );
      var dropTargetContainerStyle = { display: 'flex', alignItems: 'center' };

      var dataButtons = config.dataFields.map(function (field, index) {
        return _react2.default.createElement(
          'div',
          { style: { padding: '0px 4px' }, key: 'div-' + field.name },
          _react2.default.createElement(_DataButton2.default, {
            key: field.name,
            field: field,
            position: index,
            active: config.activatedDataFields.filter(function (fld) {
              return fld.name === field.name;
            }).length,
            store: store
          })
        );
      });
      var dataButtonsContainer = _react2.default.createElement(
        'div',
        { style: dropTargetContainerStyle },
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' }, className: 'flds-grp-cap text-muted' },
          _react2.default.createElement(
            'div',
            null,
            'Data'
          )
        ),
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' }, className: 'empty' },
          _react2.default.createElement(
            'div',
            { style: { display: 'flex' } },
            dataButtons
          )
        )
      );

      var style = {
        borderSpacing: 0,
        borderCollapse: 'separate'
      };
      return _react2.default.createElement(
        'div',
        { className: 'inner-table upper-buttons', style: style },
        fieldList,
        dataButtonsContainer
      );
    }
  }]);

  return ChartConfiguration;
}(_react.Component);

exports.default = ChartConfiguration;