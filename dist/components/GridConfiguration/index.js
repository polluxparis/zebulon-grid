'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactDnd = require('react-dnd');

var _reactDndHtml5Backend = require('react-dnd-html5-backend');

var _reactDndHtml5Backend2 = _interopRequireDefault(_reactDndHtml5Backend);

var _DragManager = require('../../DragManager');

var _DragManager2 = _interopRequireDefault(_DragManager);

var _FieldButton = require('../FieldButton');

var _FieldButton2 = _interopRequireDefault(_FieldButton);

var _DataButton = require('../DataButton');

var _DataButton2 = _interopRequireDefault(_DataButton);

var _FieldList = require('../FieldList');

var _FieldList2 = _interopRequireDefault(_FieldList);

var _Axe = require('../../Axe');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var GridConfiguration = function (_Component) {
  _inherits(GridConfiguration, _Component);

  function GridConfiguration(props) {
    _classCallCheck(this, GridConfiguration);

    var _this = _possibleConstructorReturn(this, (GridConfiguration.__proto__ || Object.getPrototypeOf(GridConfiguration)).call(this, props));

    _this.moveButton = _this.moveButton.bind(_this);
    _DragManager2.default.init(_this.moveButton.bind(_this));
    return _this;
  }

  _createClass(GridConfiguration, [{
    key: 'moveButton',
    value: function moveButton(buttonId, oldAxeType, newAxeType, position) {
      var store = this.props.store;

      store.moveField(buttonId, oldAxeType, newAxeType, position);
    }
  }, {
    key: 'render',
    value: function render() {
      var store = this.props.store;
      var config = store.config;

      var unusedFieldList = void 0;
      var dropTargetContainerStyle = { display: 'flex', alignItems: 'center' };

      if (config.canMoveFields) {
        var fieldsButtons = config.availableFields.map(function (field, index) {
          return _react2.default.createElement(_FieldButton2.default, {
            key: field.name,
            field: field,
            axetype: _Axe.AxeType.FIELDS,
            position: index,
            store: store });
        });
        unusedFieldList = _react2.default.createElement(
          'div',
          { style: dropTargetContainerStyle },
          _react2.default.createElement(
            'div',
            { style: { padding: '7px 4px' }, className: 'flds-grp-cap av-flds text-muted' },
            'Fields'
          ),
          _react2.default.createElement(
            'div',
            { style: { padding: '7px 4px' }, className: 'av-flds' },
            _react2.default.createElement(_FieldList2.default, { buttons: fieldsButtons, axetype: _Axe.AxeType.FIELDS, moveButton: this.moveButton })
          )
        );
      } else {
        unusedFieldList = null;
      }

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

      var columnButtons = config.columnFields.map(function (field, index) {
        return _react2.default.createElement(_FieldButton2.default, {
          key: field.name,
          field: field,
          axetype: _Axe.AxeType.COLUMNS,
          position: index,
          store: store });
      });

      var columnFieldList = _react2.default.createElement(
        'div',
        { style: dropTargetContainerStyle },
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' }, className: 'flds-grp-cap text-muted' },
          'Columns'
        ),
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' } },
          _react2.default.createElement(_FieldList2.default, { buttons: columnButtons, axetype: _Axe.AxeType.COLUMNS, moveButton: this.moveButton })
        )
      );

      var rowButtons = config.rowFields.map(function (field, index) {
        return _react2.default.createElement(_FieldButton2.default, {
          key: field.name,
          field: field,
          axetype: _Axe.AxeType.ROWS,
          position: index,
          store: store });
      });

      var rowFieldList = _react2.default.createElement(
        'div',
        { style: dropTargetContainerStyle },
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' }, className: 'flds-grp-cap text-muted' },
          'Rows'
        ),
        _react2.default.createElement(
          'div',
          { style: { padding: '7px 4px' } },
          _react2.default.createElement(_FieldList2.default, { buttons: rowButtons, axetype: _Axe.AxeType.ROWS, moveButton: this.moveButton })
        )
      );

      var style = {
        borderSpacing: 0,
        borderCollapse: 'separate'
      };
      return _react2.default.createElement(
        'div',
        { className: 'inner-table upper-buttons', style: style },
        _react2.default.createElement(
          'div',
          null,
          unusedFieldList,
          columnFieldList,
          rowFieldList
        ),
        dataButtonsContainer
      );
    }
  }]);

  return GridConfiguration;
}(_react.Component);

exports.default = (0, _reactDnd.DragDropContext)(_reactDndHtml5Backend2.default)(GridConfiguration);