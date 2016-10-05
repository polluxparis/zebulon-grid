'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _reactDom = require('react-dom');

var _reactDom2 = _interopRequireDefault(_reactDom);

var _Utils = require('./Utils');

var utils = _interopRequireWildcard(_Utils);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  var _moveButton = null;

  var _currDragElement = null;
  var _currDropTarget = null;
  var _currDropIndicator = null;

  var _dragNode = null;
  var _dropTargets = [];
  var _dropIndicators = [];

  function doElementsOverlap(elem1Rect, elem2Rect) {
    return !(elem1Rect.right < elem2Rect.left || elem1Rect.left > elem2Rect.right || elem1Rect.bottom < elem2Rect.top || elem1Rect.top > elem2Rect.bottom);
  }

  function setCurrDropTarget(dropTarget, callback) {
    if (_currDropTarget) {
      signalDragEnd(_currDropTarget, function () {
        _currDropTarget = dropTarget;
        signalDragOver(dropTarget, callback);
      });
    } else {
      _currDropTarget = dropTarget;
      signalDragOver(dropTarget, callback);
    }
  }

  function setCurrDropIndicator(dropIndicator) {
    if (_currDropIndicator) {
      signalDragEnd(_currDropIndicator, function () {
        _currDropIndicator = dropIndicator;
        signalDragOver(dropIndicator);
      });
    } else {
      _currDropIndicator = dropIndicator;
      signalDragOver(dropIndicator);
    }
  }

  function signalDragOver(target, callback) {
    if (target && target.onDragOver) {
      target.onDragOver(callback);
    } else if (callback) {
      callback();
    }
  }

  function signalDragEnd(target, callback) {
    if (target && target.onDragEnd) {
      target.onDragEnd(callback);
    } else if (callback) {
      callback();
    }
  }

  // function getDropTarget () {
  //   return utils.forEach(_dropTargets, target => {
  //     if (target.component.state.isover) {
  //       return target
  //     }
  //   })
  // }
  //
  // function getDropIndicator () {
  //   return utils.forEach(_dropIndicators, indicator => {
  //     if (indicator.component.state.isover) {
  //       return indicator
  //     }
  //   })
  // }
  //
  // var _initialized = false

  return {
    init: function init(moveButton) {
      // _initialized = true
      _moveButton = moveButton;
    },
    setDragElement: function setDragElement(elem) {
      console.log('_currDropTarget', _currDropTarget);
      console.log(elem);
      var prevDragElement = _currDragElement;
      _currDragElement = elem;
      if (_currDragElement !== prevDragElement) {
        if (elem == null) {
          if (_currDropTarget) {
            var position = _currDropIndicator != null ? _currDropIndicator.position : null;
            _moveButton(prevDragElement, _currDropTarget.component.props.axetype, position);
          }

          _dragNode = null;
          setCurrDropTarget(null);
          setCurrDropIndicator(null);
        } else {
          _dragNode = _reactDom2.default.findDOMNode(_currDragElement);
        }
      }
    },
    registerTarget: function registerTarget(target, axetype, dragOverHandler, dargEndHandler) {
      _dropTargets.push({
        component: target,
        axetype: axetype,
        onDragOver: dragOverHandler,
        onDragEnd: dargEndHandler
      });
    },
    unregisterTarget: function unregisterTarget(target) {
      var tindex = void 0;
      for (var i = 0; i < _dropTargets.length; i++) {
        if (_dropTargets[i].component === target) {
          tindex = i;
          break;
        }
      }
      if (tindex != null) {
        _dropTargets.splice(tindex, 1);
      }
    },
    registerIndicator: function registerIndicator(indicator, axetype, position, dragOverHandler, dargEndHandler) {
      _dropIndicators.push({
        component: indicator,
        axetype: axetype,
        position: position,
        onDragOver: dragOverHandler,
        onDragEnd: dargEndHandler
      });
    },
    unregisterIndicator: function unregisterIndicator(indicator) {
      var iindex = void 0;
      for (var i = 0; i < _dropIndicators.length; i++) {
        if (_dropIndicators[i].component === indicator) {
          iindex = i;
          break;
        }
      }
      if (iindex != null) {
        _dropIndicators.splice(iindex, 1);
      }
    },
    elementMoved: function elementMoved() {
      // console.log(`elementMoved`)
      if (_currDragElement != null) {
        (function () {
          var dragNodeRect = _dragNode.getBoundingClientRect();
          var foundTarget = void 0;

          utils.forEach(_dropTargets, function (target) {
            if (!foundTarget) {
              var tnodeRect = _reactDom2.default.findDOMNode(target.component).getBoundingClientRect();
              var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
              if (isOverlap) {
                foundTarget = target;
                return;
              }
            }
          });

          if (foundTarget) {
            setCurrDropTarget(foundTarget, function () {
              var foundIndicator = null;

              utils.forEach(_dropIndicators, function (indicator, index) {
                if (!foundIndicator) {
                  var elementOwnIndicator = indicator.component.props.axetype === _currDragElement.props.axetype && indicator.component.props.position === _currDragElement.props.position;

                  var targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
                  if (targetIndicator && !elementOwnIndicator) {
                    var tnodeRect = _reactDom2.default.findDOMNode(indicator.component).getBoundingClientRect();
                    var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
                    if (isOverlap) {
                      foundIndicator = indicator;
                      return true;
                    }
                  }
                }
              });

              if (!foundIndicator) {
                var axeIndicators = _dropIndicators.filter(function (indicator) {
                  return indicator.component.props.axetype === foundTarget.component.props.axetype;
                });
                if (axeIndicators.length > 0) {
                  foundIndicator = axeIndicators[axeIndicators.length - 1];
                }
              }
              setCurrDropIndicator(foundIndicator);
            });
          }
        })();
      }
    }
  };
}();