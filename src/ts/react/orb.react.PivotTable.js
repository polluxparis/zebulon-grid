"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
var React = require('react');
var ReactDOM = require('react-dom');
var orb_react_DragManager_1 = require('./orb.react.DragManager');
var orb_react_PivotTable_UpperButtons_1 = require('./orb.react.PivotTable.UpperButtons');
var orb_react_PivotTable_ColumnButtons_1 = require('./orb.react.PivotTable.ColumnButtons');
var orb_react_PivotTable_RowButtons_1 = require('./orb.react.PivotTable.RowButtons');
var orb_react_PivotTable_RowHeaders_1 = require('./orb.react.PivotTable.RowHeaders');
var orb_react_PivotTable_ColumnHeaders_1 = require('./orb.react.PivotTable.ColumnHeaders');
var orb_react_PivotTable_DataCells_1 = require('./orb.react.PivotTable.DataCells');
var domUtils = require('../orb.utils.dom');
var react_virtualized_1 = require('react-virtualized');
var mobx_react_1 = require('mobx-react');
var pivotId = 1;
var themeChangeCallbacks = {};
require('react-virtualized/styles.css');
var PivotTableComponent = (function (_super) {
    __extends(PivotTableComponent, _super);
    function PivotTableComponent(props) {
        _super.call(this, props);
        this.id = pivotId++;
        this.pgrid = null;
        this.pgridwidgetstore = null;
        this.fontStyle = null;
        orb_react_DragManager_1.default.init(this);
        themeChangeCallbacks[this.id] = [];
        this.registerThemeChanged(this.updateClasses);
        this.pgridwidgetstore = this.props.pgridwidgetstore;
        this.pgrid = this.pgridwidgetstore.pgrid;
    }
    PivotTableComponent.prototype.sort = function (axetype, field) {
        this.pgridwidgetstore.sort(axetype, field);
    };
    PivotTableComponent.prototype.moveButton = function (button, newAxeType, position) {
        this.pgridwidgetstore.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    };
    PivotTableComponent.prototype.toggleSubtotals = function (axetype) {
        this.pgridwidgetstore.toggleSubtotals(axetype);
    };
    PivotTableComponent.prototype.toggleGrandtotal = function (axetype) {
        this.pgridwidgetstore.toggleGrandtotal(axetype);
    };
    PivotTableComponent.prototype.applyFilter = function (fieldname, operator, term, staticValue, excludeStatic) {
        this.pgridwidgetstore.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
    };
    PivotTableComponent.prototype.registerThemeChanged = function (compCallback) {
        if (compCallback) {
            themeChangeCallbacks[this.id].push(compCallback);
        }
    };
    PivotTableComponent.prototype.unregisterThemeChanged = function (compCallback) {
        var i;
        if (compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
            themeChangeCallbacks[this.id].splice(i, 1);
        }
    };
    PivotTableComponent.prototype.changeTheme = function (newTheme) {
        if (this.pgridwidgetstore.pgrid.config.setTheme(newTheme)) {
            for (var i = 0; i < themeChangeCallbacks[this.id].length; i++) {
                themeChangeCallbacks[this.id][i]();
            }
        }
    };
    PivotTableComponent.prototype.updateClasses = function () {
        var thisnode = ReactDOM.findDOMNode(this);
        var classes = this.pgridwidgetstore.pgrid.config.theme.getPivotClasses();
        thisnode.className = classes.container;
        thisnode['children'][1].className = classes.table;
    };
    PivotTableComponent.prototype.componentDidMount = function () {
        var fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true);
        this.fontStyle = {
            fontFamily: fontInfos[0],
            fontSize: fontInfos[1]
        };
    };
    PivotTableComponent.prototype.shouldComponentUpdate = function (nextProps, nextState) {
        console.log("shouldComponentUpdate");
    };
    PivotTableComponent.prototype.render = function () {
        var _this = this;
        var cellHeight = this.pgridwidgetstore.layout.cell.height;
        var cellWidth = this.pgridwidgetstore.layout.cell.width;
        var rowVerticalCount = this.pgridwidgetstore.layout.rowHeaders.height;
        var rowHorizontalCount = this.pgridwidgetstore.layout.rowHeaders.width;
        var columnVerticalCount = this.pgridwidgetstore.layout.columnHeaders.height;
        var columnHorizontalCount = this.pgridwidgetstore.layout.columnHeaders.width;
        var config = this.pgridwidgetstore.pgrid.config;
        var classes = config.theme.getPivotClasses();
        var tblStyle = { width: undefined, height: undefined };
        if (config.width) {
            tblStyle.width = config.width;
        }
        if (config.height) {
            tblStyle.height = config.height;
        }
        return (React.createElement("div", {className: classes.container}, 
            React.createElement("div", {className: 'orb'}, 
                React.createElement(orb_react_PivotTable_UpperButtons_1.default, {pivotTableComp: this}), 
                React.createElement("div", {style: {
                    position: 'relative',
                    left: cellWidth * rowHorizontalCount,
                }}, 
                    React.createElement(orb_react_PivotTable_ColumnButtons_1.default, {pivotTableComp: this})
                ), 
                React.createElement("div", {style: { position: 'relative' }}, 
                    React.createElement(react_virtualized_1.ScrollSync, null, function (_a) {
                        var clientHeight = _a.clientHeight, clientWidth = _a.clientWidth, onScroll = _a.onScroll, scrollHeight = _a.scrollHeight, scrollLeft = _a.scrollLeft, scrollTop = _a.scrollTop, scrollWidth = _a.scrollWidth;
                        return React.createElement("div", {style: tblStyle, ref: "pivot"}, 
                            React.createElement("div", {style: {
                                position: 'absolute',
                                left: 0,
                                top: 0
                            }}, 
                                React.createElement(orb_react_PivotTable_RowButtons_1.default, {pivotTableComp: _this, ref: "rowButtons"}), 
                                React.createElement("div", {style: {
                                    backgroundColor: '#eef8fb',
                                    margin: '2px',
                                    position: 'absolute',
                                    left: 0,
                                    top: cellHeight * columnVerticalCount + 20,
                                    height: Math.min(config.height - cellHeight * columnVerticalCount, cellHeight * rowVerticalCount),
                                    width: rowHorizontalCount * cellWidth
                                }}, 
                                    React.createElement(orb_react_PivotTable_RowHeaders_1.default, {pgridwidgetstore: _this.props.pgridwidgetstore, onScroll: onScroll, scrollTop: scrollTop, ref: "rowHeaders"})
                                ), 
                                React.createElement("div", {style: {
                                    margin: '2px',
                                    position: 'absolute',
                                    left: cellWidth * rowHorizontalCount,
                                    top: 20,
                                    width: cellWidth * columnHorizontalCount
                                }}, 
                                    React.createElement("div", {style: {
                                        backgroundColor: '#eef8fb',
                                        width: Math.min(config.width - cellWidth * rowHorizontalCount, cellWidth * columnHorizontalCount),
                                        height: columnVerticalCount * cellHeight
                                    }}, 
                                        React.createElement(orb_react_PivotTable_ColumnHeaders_1.default, {pgridwidgetstore: _this.props.pgridwidgetstore, onScroll: onScroll, scrollLeft: scrollLeft, ref: "colHeaders"})
                                    ), 
                                    React.createElement("div", {style: {
                                        width: Math.min(config.width - cellWidth * rowHorizontalCount, cellWidth * columnHorizontalCount),
                                        height: Math.min(config.height - cellHeight * columnVerticalCount, cellHeight * rowVerticalCount)
                                    }}, 
                                        React.createElement(orb_react_PivotTable_DataCells_1.default, {pgridwidgetstore: _this.props.pgridwidgetstore, onScroll: onScroll, scrollTop: scrollTop, scrollLeft: scrollLeft, ref: "dataCells"})
                                    )))
                        );
                    }), 
                    React.createElement("div", {className: "orb-overlay orb-overlay-hidden", id: 'drilldialog' + this.id})))
        ));
    };
    PivotTableComponent = __decorate([
        mobx_react_1.observer, 
        __metadata('design:paramtypes', [Object])
    ], PivotTableComponent);
    return PivotTableComponent;
}(React.Component));
exports.PivotTableComponent = PivotTableComponent;
//# sourceMappingURL=orb.react.PivotTable.js.map