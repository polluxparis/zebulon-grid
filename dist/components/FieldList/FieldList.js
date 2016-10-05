'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactDnd = require('react-dnd');

var _DropIndicator = require('./DropIndicator');

var _DropIndicator2 = _interopRequireDefault(_DropIndicator);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var FieldList = function FieldList(_ref) {
  var buttons = _ref.buttons;
  var axetype = _ref.axetype;
  var connectDropTarget = _ref.connectDropTarget;
  var isOverCurrent = _ref.isOverCurrent;
  var isOver = _ref.isOver;
  var moveButton = _ref.moveButton;

  var buttonComponents = buttons.map(function (button, index) {
    if (index < buttons.length - 1) {
      return [_react2.default.createElement(
        'div',
        null,
        _react2.default.createElement(_DropIndicator2.default, { isFirst: index === 0, position: index, axetype: axetype, moveButton: moveButton })
      ), _react2.default.createElement(
        'div',
        null,
        button
      )];
    } else {
      return [_react2.default.createElement(
        'div',
        null,
        _react2.default.createElement(_DropIndicator2.default, { isFirst: index === 0, position: index, axetype: axetype, moveButton: moveButton })
      ), _react2.default.createElement(
        'div',
        null,
        button
      ), _react2.default.createElement(
        'div',
        null,
        _react2.default.createElement(_DropIndicator2.default, { isLast: true, position: null, axetype: axetype, moveButton: moveButton })
      )];
    }
  });

  var highlight = buttons.length === 0 ? isOver : isOver && !isOverCurrent;

  var style = {
    border: highlight ? 'dotted rgba(255, 192, 222, 0.7)' : 'dotted rgba(91, 192, 222, 0.7)',
    minHeight: '24px',
    minWidth: '67px',
    borderRadius: 10,
    display: 'flex'
  };

  return connectDropTarget(_react2.default.createElement(
    'div',
    { style: style },
    buttonComponents
  ));
};

var dropTarget = {
  drop: function drop(props, monitor, component) {
    var _monitor$getItem = monitor.getItem();

    var id = _monitor$getItem.id;
    var axetype = _monitor$getItem.axetype;

    props.moveButton(id, axetype, props.axetype, props.position);
  },
  canDrop: function canDrop(props, monitor) {
    return props.buttons.length === 0;
  }
};

var collect = function collect(connect, monitor) {
  return {
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
    isOverCurrent: monitor.isOver({ shallow: true })
  };
};

exports.default = (0, _reactDnd.DropTarget)('button', dropTarget, collect)(FieldList);