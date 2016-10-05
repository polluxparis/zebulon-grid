'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactResizable = require('react-resizable');

var _reactVirtualizedCheckbox = require('react-virtualized-checkbox');

var _reactVirtualizedCheckbox2 = _interopRequireDefault(_reactVirtualizedCheckbox);

var _Utils = require('../../Utils');

var utils = _interopRequireWildcard(_Utils);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var startingHeight = 223;
var startingWidth = 301;

var FilterPanel = function FilterPanel(_ref) {
  var store = _ref.store;
  var field = _ref.field;
  var onFilter = _ref.onFilter;
  var onCancel = _ref.onCancel;

  var filter = store.filters.get(field.name);
  var values = store.getFieldValues(field.name);
  var checkedValues = filter && filter.staticValue.length < values.length ? utils.twoArraysIntersect(values, filter.staticValue) : values;
  var options = values.map(function (val) {
    return { checked: checkedValues.indexOf(val) > -1, label: val };
  });

  var checkboxes = _react2.default.createElement(_reactVirtualizedCheckbox2.default, {
    options: options,
    onOk: function onOk(all, result) {
      return onFilter(all, '', '', result, false);
    },
    onCancel: onCancel,
    maxHeight: startingHeight
  });

  var divStyle = {
    backgroundColor: 'white',
    border: 'solid 1px',
    boxShadow: '0 5px 15px #9d9d9d',
    display: 'flex',
    flexDirection: 'column',
    fontSize: '90%',
    height: '100%',
    justifyContent: 'space-between',
    padding: '3px',
    width: '100%',
    zIndex: 100
  };

  return _react2.default.createElement(
    _reactResizable.ResizableBox,
    { width: startingWidth, height: startingHeight, minConstraints: [startingWidth, startingHeight] },
    _react2.default.createElement(
      'div',
      { style: divStyle, id: 'filter-panel' },
      checkboxes
    )
  );
};

exports.default = FilterPanel;