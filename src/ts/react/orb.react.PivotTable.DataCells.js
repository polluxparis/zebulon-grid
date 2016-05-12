"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var React = require('react');
var orb_axe_1 = require('../orb.axe');
var orb_react_PivotRow_1 = require('./orb.react.PivotRow');
var react_virtualized_1 = require('react-virtualized');
var orb_react_PivotCell_1 = require('./orb.react.PivotCell');
var DataCellsComponent = (function (_super) {
    __extends(DataCellsComponent, _super);
    function DataCellsComponent() {
        _super.apply(this, arguments);
    }
    DataCellsComponent.prototype.render = function () {
        var _this = this;
        var pgridwidgetstore = this.props.pgridwidgetstore;
        var config = pgridwidgetstore.pgrid.config;
        var columnCount = pgridwidgetstore.dataRows[0].length;
        var cellHeight = this.props.pgridwidgetstore.layout.cell.height;
        var cellWidth = this.props.pgridwidgetstore.layout.cell.width;
        return (React.createElement(react_virtualized_1.AutoSizer, null, function (_a) {
            var height = _a.height, width = _a.width;
            return React.createElement(react_virtualized_1.Grid, {onScroll: _this.props.onScroll, scrollLeft: _this.props.scrollLeft, scrollTop: _this.props.scrollTop, width: width, height: height, columnWidth: cellWidth, rowHeight: cellHeight, columnCount: columnCount, rowCount: pgridwidgetstore.dataRows.length, cellRenderer: function (_a) {
                var columnIndex = _a.columnIndex, rowIndex = _a.rowIndex;
                return React.createElement(orb_react_PivotCell_1.default, {key: columnIndex, cell: pgridwidgetstore.dataRows[rowIndex][columnIndex], leftmost: true, topmost: true, pgridwidgetstore: _this.props.pgridwidgetstore});
            }});
        }));
    };
    DataCellsComponent.prototype._render = function () {
        var _this = this;
        var pgridwidgetstore = this.props.pgridwidgetstore;
        var layoutInfos = {
            lastLeftMostCellVSpan: 0,
            topMostCells: {}
        };
        var dataCells = pgridwidgetstore.dataRows.map(function (dataRow, index) {
            return React.createElement(orb_react_PivotRow_1.default, {key: index, row: dataRow, axetype: orb_axe_1.AxeType.DATA, layoutInfos: layoutInfos, pgridwidgetstore: _this.props.pgridwidgetstore});
        });
        return React.createElement("div", {className: "inner-table-container data-cntr"}, 
            React.createElement("table", {className: "inner-table"}, 
                React.createElement("colgroup", null), 
                React.createElement("tbody", null, dataCells))
        );
    };
    return DataCellsComponent;
}(React.Component));
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = DataCellsComponent;
;
