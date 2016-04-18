/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require, React, window */
/*jshint eqnull: true*/

import React = require('react');
import ReactDOM = require('react-dom');
import {Axe, AxeType}from './orb.axe';
import {PGrid, EVENT_UPDATED, EVENT_CONFIG_CHANGED, EVENT_SORT_CHANGED}from './orb.pgrid';
import {HeaderType, DataCell, CellBase} from './orb.ui.header';
import {UiRows} from './orb.ui.rows';
import {UiCols} from './orb.ui.cols';

const Dialog = require('./react/orb.react.Dialog.jsx'),
    PivotChart = require('./react/orb.react.PivotChart.jsx'),
    PivotTable = require('./react/orb.react.PivotTable.jsx'),
    Grid = require('./react/orb.react.Grid.jsx');

    // CSS files
    // Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
    require('../../dist/orb.css');
    require('../../deps/bootstrap-3.3.1/css/bootstrap.css');

/**
 * Creates a new instance of pivot grid control
 * @class
 * @memberOf orb.ui
 * @param  {object} pgrid - pivot grid instance
 */
export class  PGridWidget {

    /**
     * Parent pivot grid
     * @type {orb.pgrid}
     */
    public pgrid: PGrid;

    /**
     * Control rows headers
     * @type {orb.ui.rows}
     */
    public rows = null;
    /**
     * Control columns headers
     * @type {orb.ui.cols}
     */
    public columns = null;

    /**
     * Control data rows
     * @type {orb.ui.CellBase}
     */
    public dataRows;

    public layout = {
        rowHeaders: {
            /**
             * Total number of horizontal row headers.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical row headers.
             * @type {Number}
             */
            height: null
        },
        columnHeaders: {
            /**
             * Total number of horizontal column headers.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical column headers.
             * @type {Number}
             */
            height: null
        },
        pivotTable: {
            /**
             * Total number of horizontal cells of the whole pivot grid control.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical cells of the whole pivot grid control.
             * @type {Number}
             */
            height: null
        }
    };
    public renderElement;
    public pivotComponent;
    public dialog;

    constructor(config) {
        this.dialog = Dialog.create();

        this.pgrid = new PGrid(config);

        this.rows = null;
        this.columns = null;

        this.dataRows = [];

        this.layout = {
            rowHeaders: {
                width: null,
                height: null
            },
            columnHeaders: {
                width: null,
                height: null
            },
            pivotTable: {
                width: null,
                height: null
            }
        };
        this.init();
    };

    expandRow(cell) {
        cell.expand();
        this.render();
    };

    collapseRow(cell) {
        cell.subtotalHeader.collapse();
        this.render();
    };

    sort(axetype, field) {
        this.pgrid.sort(axetype, field);
    };

    refreshData(data) {
        this.pgrid.refreshData(data);
    };

    applyFilter(fieldname, operator, term, staticValue, excludeStatic) {
        this.pgrid.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
    };

    moveField(field, oldAxeType, newAxeType, position) {
        this.pgrid.moveField(field, oldAxeType, newAxeType, position);
    };

    toggleFieldExpansion (axetype, field, newState) {
        var axeToExpand =
            axetype === AxeType.ROWS
            ? this.rows
            : (axetype === AxeType.COLUMNS
            ? this.columns
            : null);

        if (axeToExpand && axeToExpand.toggleFieldExpansion(field, newState)) {
            this.render();
        }
    };

    toggleSubtotals(axetype) {
        this.pgrid.toggleSubtotals(axetype);
    };

    areSubtotalsVisible(axetype) {
        return this.pgrid.areSubtotalsVisible(axetype);
    };

    toggleGrandtotal(axetype) {
        this.pgrid.toggleGrandtotal(axetype);
    };

    isGrandtotalVisible(axetype) {
        return this.pgrid.isGrandtotalVisible(axetype);
    };

    changeTheme(newTheme) {
        this.pivotComponent.changeTheme(newTheme);
    };

    render(element?) {
        this.renderElement = element || this.renderElement;
        if(this.renderElement) {
            var pivotTableFactory = React.createFactory(
                this.pgrid.config.chartMode.enabled ?
                    PivotChart :
                    PivotTable);
            var pivottable = pivotTableFactory({
                pgridwidget: this
            });
            this.pivotComponent = ReactDOM.render(pivottable, this.renderElement);
        }
    };

    unmount() {
        ReactDOM.unmountComponentAtNode(this.renderElement);
    };

    drilldown(dataCell, pivotId) {
        if(dataCell) {
            var colIndexes = dataCell.columnDimension.getRowIndexes();
            var data = dataCell.rowDimension.getRowIndexes().filter(function(index) {
                return colIndexes.indexOf(index) >= 0;
            }).map(function(index) {
                return this.pgrid.filteredDataSource[index];
            });

            var title;
            if(dataCell.rowType === HeaderType.GRAND_TOTAL && dataCell.colType === HeaderType.GRAND_TOTAL) {
                title = 'Grand total';
            } else {
                if(dataCell.rowType === HeaderType.GRAND_TOTAL) {
                    title = dataCell.columnDimension.value + '/Grand total ';
                } else if(dataCell.colType === HeaderType.GRAND_TOTAL) {
                    title = dataCell.rowDimension.value + '/Grand total ';
                } else {
                    title = dataCell.rowDimension.value + '/' + dataCell.columnDimension.value;
                }
            }

            this.dialog.show({
                title: title,
                comp: {
                    type: Grid,
                    props: {
                        headers: this.pgrid.config.getDataSourceFieldCaptions(),
                        data: data,
                        theme: this.pgrid.config.theme
                    }
                },
                theme: this.pgrid.config.theme,
                style: this.pivotComponent.fontStyle
            });
        }
    };

    init() {
        this.pgrid.subscribe(EVENT_UPDATED, this.buildUiAndRender);
        this.pgrid.subscribe(EVENT_SORT_CHANGED, this.buildUiAndRender);
        this.pgrid.subscribe(EVENT_CONFIG_CHANGED, this.buildUiAndRender);

        this.buildUi();
    }

    buildUi() {

        // build row and column headers
        this.rows = new UiRows(this.pgrid.rows);
        this.columns = new UiCols(this.pgrid.columns);

        var rowsHeaders = this.rows.headers;
        var columnsLeafHeaders = this.columns.leafsHeaders;

        // set control layout infos
        this.layout.rowHeaders = {
            width: (this.pgrid.rows.fields.length || 1) +
            (this.pgrid.config.dataHeadersLocation === 'rows' && this.pgrid.config.dataFieldsCount > 1 ? 1 : 0),
            height: rowsHeaders.length
        };
        this.layout.columnHeaders = {
            width: this.columns.leafsHeaders.length,
            height: (this.pgrid.columns.fields.length || 1) +
            (this.pgrid.config.dataHeadersLocation === 'columns' && this.pgrid.config.dataFieldsCount > 1 ? 1 : 0)
        };

        this.layout.pivotTable = {
            width: this.layout.rowHeaders.width + this.layout.columnHeaders.width,
            height: this.layout.rowHeaders.height + this.layout.columnHeaders.height
        };

        var dataRows = [];
        var arr;

        function createVisibleFunc(rowvisible, colvisible) {
            return function() {
                return rowvisible() && colvisible();
            };
        }
        if(rowsHeaders.length > 0) {
            for (var ri = 0; ri < rowsHeaders.length; ri++) {
                var rowHeadersRow = rowsHeaders[ri];
                var rowLeafHeader = rowHeadersRow[rowHeadersRow.length - 1];

                arr = [];
                for (var colHeaderIndex = 0; colHeaderIndex < columnsLeafHeaders.length; colHeaderIndex++) {
                    var columnLeafHeader = columnsLeafHeaders[colHeaderIndex];
                    var isvisible = createVisibleFunc(rowLeafHeader.visible, columnLeafHeader.visible);
                    arr[colHeaderIndex] = new DataCell(this.pgrid, isvisible, rowLeafHeader, columnLeafHeader);
                }
                dataRows.push(arr);
            }
        }
        this.dataRows = dataRows;
    }

    buildUiAndRender() {
        this.buildUi();
        this.render();
    }

};
