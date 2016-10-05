'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _reactDnd = require('react-dnd');

var _Utils = require('../../Utils');

var utils = _interopRequireWildcard(_Utils);

var _FilterPanel = require('../FilterPanel');

var _FilterPanel2 = _interopRequireDefault(_FilterPanel);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var filterImage = 'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAMUlEQVQYlWP4//9/I7GYgSzFDHgAVsX/sQCsirFpQFaI1c0wDegKB0AxeihQFs7EYAAT8WYwzt7jxgAAAABJRU5ErkJggg==) no-repeat 0px 0px';

var FieldButton = function (_Component) {
  _inherits(FieldButton, _Component);

  function FieldButton(props) {
    _classCallCheck(this, FieldButton);

    var _this = _possibleConstructorReturn(this, (FieldButton.__proto__ || Object.getPrototypeOf(FieldButton)).call(this, props));

    _this.state = { filtering: false };

    _this.addFilterPanel = _this.addFilterPanel.bind(_this);
    _this.removeFilterPanel = _this.removeFilterPanel.bind(_this);
    _this.onMouseDown = _this.onMouseDown.bind(_this);
    _this.onFilter = _this.onFilter.bind(_this);
    return _this;
  }

  _createClass(FieldButton, [{
    key: 'addFilterPanel',
    value: function addFilterPanel() {
      var filtering = this.state.filtering;

      if (!filtering) {
        utils.addEventListener(document, 'mousedown', this.onMouseDown);
        this.setState({ filtering: true });
      }
    }
  }, {
    key: 'removeFilterPanel',
    value: function removeFilterPanel() {
      utils.removeEventListener(document, 'mousedown', this.onMouseDown);
      this.setState({ filtering: false });
    }
  }, {
    key: 'onMouseDown',
    value: function onMouseDown(e) {
      var filterPanelNode = document.getElementById('filter-panel');
      var target = e.target || e.srcElement;
      while (target !== null) {
        if (target === filterPanelNode) {
          return true;
        }
        target = target.parentNode;
      }
      this.removeFilterPanel();
    }
  }, {
    key: 'onFilter',
    value: function onFilter(all, operator, term, staticValue, excludeStatic) {
      var _props = this.props;
      var store = _props.store;
      var field = _props.field;
      var axetype = _props.axetype;

      store.applyFilter(field.name, axetype, all, operator, term, staticValue, excludeStatic);
      this.removeFilterPanel();
    }
  }, {
    key: 'render',
    value: function render() {
      var _this2 = this;

      var _props2 = this.props;
      var field = _props2.field;
      var store = _props2.store;
      var axetype = _props2.axetype;
      var connectDragSource = _props2.connectDragSource;
      var isDragging = _props2.isDragging;
      var filtering = this.state.filtering;

      var styles = {
        div: {
          width: isDragging ? 0 : '',
          backgroundColor: '#5bc0de',
          borderRadius: 4,
          cursor: 'default',
          opacity: isDragging ? 0 : 1,
          padding: '0.2em',
          marginTop: '0.2em',
          marginBottom: '0.2em',
          display: 'flex'
        },
        filterPlaceholder: {
          width: 11,
          height: 11,
          margin: '0.2em',
          marginLeft: '0.5em'
        },
        filterButton: {
          width: '100%',
          height: '100%',
          background: filterImage
        }
      };
      return connectDragSource(_react2.default.createElement(
        'div',
        { key: field.name, style: styles.div },
        _react2.default.createElement(
          'div',
          null,
          field.caption
        ),
        _react2.default.createElement(
          'div',
          { style: styles.filterPlaceholder },
          filtering ? _react2.default.createElement(_FilterPanel2.default, {
            field: field,
            axetype: axetype,
            store: store,
            onFilter: this.onFilter,
            onCancel: function onCancel() {
              return _this2.removeFilterPanel();
            }
          }) : _react2.default.createElement('div', {
            onClick: this.addFilterPanel,
            style: styles.filterButton
          })
        )
      ));
    }
  }]);

  return FieldButton;
}(_react.Component);

var fieldSource = {
  beginDrag: function beginDrag(props) {
    return {
      id: props.field.name,
      axetype: props.axetype
    };
  }
};

function collect(connect, monitor) {
  return {
    connectDragSource: connect.dragSource(),
    isDragging: monitor.isDragging()
  };
}

exports.default = (0, _reactDnd.DragSource)('button', fieldSource, collect)(FieldButton);