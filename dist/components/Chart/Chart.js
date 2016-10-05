'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _recharts = require('recharts');

var _chromaJs = require('chroma-js');

var _chromaJs2 = _interopRequireDefault(_chromaJs);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var Chart = function (_Component) {
  _inherits(Chart, _Component);

  function Chart() {
    _classCallCheck(this, Chart);

    return _possibleConstructorReturn(this, (Chart.__proto__ || Object.getPrototypeOf(Chart)).apply(this, arguments));
  }

  _createClass(Chart, [{
    key: 'render',
    value: function render() {
      var _props = this.props;
      var store = _props.store;
      var type = _props.type;

      type = type || 'bar';
      var dimension = store.getChartAxe().root;
      var measures = store.config.dataFields;
      var colors = _chromaJs2.default.scale('Spectral').colors(store.config.dataFields.length);
      var data = Object.keys(dimension.subdimvals).map(function (key) {
        return dimension.subdimvals[key];
      }).map(function (dimension) {
        return measures.reduce(function (current, measure) {
          return Object.assign(current, _defineProperty({}, measure.caption, store.getData(measure.name, dimension, { isRoot: true })));
        }, { name: dimension.value });
      });
      return _react2.default.createElement(
        _recharts.ResponsiveContainer,
        null,
        _react2.default.createElement(
          ChartHOC,
          {
            type: type,
            data: data,
            margin: { top: 5, right: 30, left: 30, bottom: 5 } },
          _react2.default.createElement(_recharts.XAxis, { dataKey: 'name' }),
          _react2.default.createElement(_recharts.YAxis, null),
          _react2.default.createElement(_recharts.CartesianGrid, { strokeDasharray: '3 3' }),
          _react2.default.createElement(_recharts.Tooltip, null),
          _react2.default.createElement(_recharts.Legend, null),
          measures.map(function (mea, index) {
            // It's necessary to filter afterwards in order to keep the same couple field-color when changing the number of activated data fields
            if (store.config.activatedDataFields.map(function (field) {
              return field.name;
            }).indexOf(mea.name) > -1) {
              switch (type) {
                case 'bar':
                default:
                  return _react2.default.createElement(_recharts.Bar, { key: index, type: 'monotone', dataKey: mea.caption, fill: colors[index], stroke: colors[index] });
                case 'area':
                  return _react2.default.createElement(_recharts.Area, { key: index, type: 'monotone', dataKey: mea.caption, fill: colors[index], stroke: colors[index] });
                case 'line':
                  return _react2.default.createElement(_recharts.Line, { key: index, type: 'monotone', dataKey: mea.caption, stroke: colors[index] });
              }
            } else {
              return null;
            }
          })
        )
      );
    }
  }]);

  return Chart;
}(_react.Component);

exports.default = Chart;

var ChartHOC = function (_Component2) {
  _inherits(ChartHOC, _Component2);

  function ChartHOC() {
    _classCallCheck(this, ChartHOC);

    return _possibleConstructorReturn(this, (ChartHOC.__proto__ || Object.getPrototypeOf(ChartHOC)).apply(this, arguments));
  }

  _createClass(ChartHOC, [{
    key: 'render',
    value: function render() {
      var type = this.props.type;

      switch (type) {
        case 'line':
          return _react2.default.createElement(_recharts.LineChart, this.props);
        case 'area':
          return _react2.default.createElement(_recharts.AreaChart, this.props);
        case 'bar':
        default:
          return _react2.default.createElement(_recharts.BarChart, this.props);
      }
    }
  }]);

  return ChartHOC;
}(_react.Component);