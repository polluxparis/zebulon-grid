'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactDnd = require('react-dnd');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var dropTarget = {
  drop: function drop(props, monitor, component) {
    var _monitor$getItem = monitor.getItem();

    var id = _monitor$getItem.id;
    var axetype = _monitor$getItem.axetype;

    props.moveButton(id, axetype, props.axetype, props.position);
  }
};

var collect = function collect(connect, monitor) {
  return {
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
    isOverCurrent: monitor.isOver({ shallow: true })
  };
};

var DropIndicator = function DropIndicator(_ref) {
  var isFirst = _ref.isFirst;
  var isLast = _ref.isLast;
  var isVertical = _ref.isVertical;
  var isOver = _ref.isOver;
  var connectDropTarget = _ref.connectDropTarget;

  var classname = 'drp-indic' + (isVertical ? '-vertical' : '');

  if (isFirst) {
    classname += ' drp-indic-first';
  }

  if (isLast) {
    classname += ' drp-indic-last';
  }

  var style = {
    width: isOver ? '3em' : '0.5em',
    height: '100%'
  };

  return connectDropTarget(_react2.default.createElement('div', { style: style, className: classname }));
};

exports.default = (0, _reactDnd.DropTarget)('button', dropTarget, collect)(DropIndicator);