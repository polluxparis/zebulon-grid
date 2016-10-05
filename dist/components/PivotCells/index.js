'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _Cells = require('../../Cells');

var _reactDom = require('react-dom');

var _reactDom2 = _interopRequireDefault(_reactDom);

var _Utils = require('../../Utils.dom');

var domUtils = _interopRequireWildcard(_Utils);

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var _paddingLeft = null;
var _borderLeft = null;

var HeaderCellComp = function (_Component) {
  _inherits(HeaderCellComp, _Component);

  function HeaderCellComp(props) {
    _classCallCheck(this, HeaderCellComp);

    var _this = _possibleConstructorReturn(this, (HeaderCellComp.__proto__ || Object.getPrototypeOf(HeaderCellComp)).call(this, props));

    _this._latestVisibleState = false;
    _this.expand = _this.expand.bind(_this);
    _this.collapse = _this.collapse.bind(_this);
    return _this;
  }

  _createClass(HeaderCellComp, [{
    key: 'expand',
    value: function expand() {
      this.props.onToggle(this.props.cell, false);
    }
  }, {
    key: 'collapse',
    value: function collapse() {
      this.props.onToggle(this.props.cell, false);
    }
  }, {
    key: 'updateCellInfos',
    value: function updateCellInfos() {
      var node = _reactDom2.default.findDOMNode(this);
      var cell = this.props.cell;
      node['__orb'] = node['__orb'] || {};

      if (!cell.visible()) {
        node['__orb']._visible = false;
      } else {
        var cellContentNode = this.refs['cellContent'];

        var propList = [];
        var retPaddingLeft = _paddingLeft == null;
        var retBorderLeft = !this.props.leftmost && _borderLeft == null;
        var text = node.textContent || node['innerText'];

        if (retPaddingLeft) {
          propList.push('padding-left');
        }

        if (retBorderLeft) {
          propList.push('border-left-width');
        }

        if (propList.length > 0) {
          var nodeStyle = domUtils.getStyle(node, propList, true);

          if (retPaddingLeft) {
            _paddingLeft = parseFloat(nodeStyle[0]);
          }

          if (retBorderLeft) {
            _borderLeft = parseFloat(nodeStyle[retPaddingLeft ? 1 : 0]);
          }
        }

        domUtils.removeClass(node, 'cell-hidden');

        node['__orb']._visible = true;
        if (text !== node['__orb']._lastText || !node['__orb']._textWidth) {
          node['__orb']._lastText = text;
          node['__orb']._textWidth = domUtils.getSize(cellContentNode).width;
        }
        node['__orb']._colSpan = this.props.cell.hspan(true) || 1;
        node['__orb']._rowSpan = this.props.cell.vspan(true) || 1;
        node['__orb']._paddingLeft = _paddingLeft;
        node['__orb']._paddingRight = _paddingLeft;
        node['__orb']._borderLeftWidth = this.props.leftmost ? 0 : _borderLeft;
        node['__orb']._borderRightWidth = 0;
      }
    }
  }, {
    key: 'componentDidMount',
    value: function componentDidMount() {
      // this.updateCellInfos()
    }
  }, {
    key: 'componentDidUpdate',
    value: function componentDidUpdate() {
      // this.updateCellInfos()
    }
  }, {
    key: 'shouldComponentUpdate',
    value: function shouldComponentUpdate(nextProps, nextState) {
      if (nextProps.cell && nextProps.cell === this.props.cell && !this._latestVisibleState && !nextProps.cell.visible()) {
        return false;
      }
      return true;
    }
  }, {
    key: 'render',
    value: function render() {
      var cell = this.props.cell;

      var divcontent = [];
      var value = void 0;
      var cellClick = void 0;
      var headerPushed = false;

      this._latestVisibleState = cell.visible();

      switch (cell.template) {
        case 'cell-template-row-header':
        case 'cell-template-column-header':
          var isWrapper = cell.type === _Cells.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible;
          var isSubtotal = cell.type === _Cells.HeaderType.SUB_TOTAL && !cell.expanded;
          if (isWrapper || isSubtotal) {
            headerPushed = true;

            divcontent.push(_react2.default.createElement(
              'div',
              { key: 'header-value', ref: 'cellContent' },
              _react2.default.createElement(
                'div',
                { className: 'orb-tgl-btn' },
                _react2.default.createElement('div', { className: 'orb-tgl-btn-' + (isWrapper ? 'down' : 'right'), onClick: isWrapper ? this.collapse : this.expand })
              ),
              _react2.default.createElement(
                'div',
                { className: 'hdr-val' },
                cell.value
              )
            ));
          } else {
            value = cell.value + (cell.type === _Cells.HeaderType.SUB_TOTAL ? ' Total' : '');
          }
          break;
        case 'cell-template-dataheader':
          value = cell.value.caption;
          break;
        default:
          break;
      }

      if (!headerPushed) {
        var headerClassName = void 0;
        if (cell.template !== 'cell-template-dataheader' && cell.type !== _Cells.HeaderType.GRAND_TOTAL) {
          headerClassName = 'hdr-val';
        }
        // var style = {}
        // if (cell.subheaders && cell.subheaders.length>0){
        //   const transfo = `translate(0px,${this.props.scrollTop}px`
        //   style={transform: transfo}
        // }
        divcontent.push(_react2.default.createElement(
          'div',
          { key: 'cell-value', ref: 'cellContent', className: headerClassName },
          value
        ));
      }

      return _react2.default.createElement(
        'div',
        {
          style: { width: '100%', height: '100%' },
          onDoubleClick: cellClick
        },
        divcontent
      );
    }
  }]);

  return HeaderCellComp;
}(_react.Component);

exports.default = HeaderCellComp;