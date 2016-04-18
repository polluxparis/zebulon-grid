/**
 * @fileOverview Pivot Grid columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

import {Axe, AxeType} from './orb.axe';
import {AxeUi} from './orb.ui.axe';
import {Header, DataHeader, HeaderType} from './orb.ui.header';
import {Dimension} from './orb.dimension';

/**
 * Creates a new instance of columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} columnsAxe - axe containing all columns dimensions.
 */
export class UiCols extends AxeUi{

    public leafsHeaders: any[];

    constructor(columnsAxe) {

        super(columnsAxe);

        this.leafsHeaders = null;
        this.build();
    }

    build() {
        this.headers = [];

        if (this.axe != null) {
            // Fill columns layout infos
            if(this.axe.root.values.length > 0 || this.axe.pgrid.config.grandTotal.columnsvisible) {
                for (var depth = this.axe.root.depth; depth > 1; depth--) {
                    this.headers.push([]);
                    this.getUiInfo(depth, this.headers);
                }

                if (this.axe.pgrid.config.grandTotal.columnsvisible) {
                    // add grandtotal header
                    this.headers[0] = this.headers[0] || [];
                    this.headers[0].push(new Header(AxeType.COLUMNS, HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount()));
                }
            }

            if (this.headers.length === 0) {
                this.headers.push([new Header(AxeType.COLUMNS, HeaderType.INNER, this.axe.root, null, this.dataFieldsCount())]);
            }

            // generate leafs headers
            this.generateLeafsHeaders();
        }
    };

    generateLeafsHeaders() {

        var leafsHeaders = [];

        function pushsubtotal(pheader) {
            if (pheader && pheader.dim.field.subTotal.visible) {
                leafsHeaders.push(pheader.subtotalHeader);
            }
        }

        if (this.headers.length > 0) {
            // last headers row
            var infos = this.headers[this.headers.length - 1];
            var header = infos[0];

            if(header) {
                var currparent,
                    prevpar = header.parent;

                for (var i = 0; i < infos.length; i++) {
                    header = infos[i];
                    currparent = header.parent;
                    // if current header parent is different than previous header parent,
                    // add previous parent
                    if (currparent != prevpar) {
                        pushsubtotal(prevpar);
                        if (currparent != null) {
                            // walk up parent hierarchy and add grand parents if different
                            // than current header grand parents
                            var grandpar = currparent.parent;
                            var prevgrandpar = prevpar ? prevpar.parent : null;
                            while (grandpar != prevgrandpar && prevgrandpar != null) {
                                pushsubtotal(prevgrandpar);
                                grandpar = grandpar ? grandpar.parent : null;
                                prevgrandpar = prevgrandpar ? prevgrandpar.parent : null;
                            }
                        }
                        // update previous parent variable
                        prevpar = currparent;
                    }
                    // push current header
                    leafsHeaders.push(infos[i]);

                    // if it's the last header, add all of its parents up to the top
                    if (i === infos.length - 1) {
                        while (prevpar != null) {
                            pushsubtotal(prevpar);
                            prevpar = prevpar.parent;
                        }
                    }
                }
                // grandtotal is visible for columns and if there is more than one dimension in this axe
                if (this.axe.pgrid.config.grandTotal.columnsvisible && this.axe.dimensionsCount > 1) {
                    // push also grand total header
                    leafsHeaders.push(this.headers[0][this.headers[0].length - 1]);
                }
            }
        }

        // add data headers if more than 1 data field and they willbe the leaf headers
        if (this.isMultiDataFields()) {
            this.leafsHeaders = [];
            for (var leafIndex = 0; leafIndex < leafsHeaders.length; leafIndex++) {
                for (var datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
                    this.leafsHeaders.push(new DataHeader(this.axe.pgrid.config.dataFields[datafieldindex], leafsHeaders[leafIndex]));
                }
            }
            this.headers.push(this.leafsHeaders);
        } else {
            this.leafsHeaders = leafsHeaders;
        }
    }

    /**
     * Fills the infos array given in argument with the dimension layout infos as column.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {int}  depth - the depth of the dimension that it's subdimensions will be returned
     * @param  {object}  infos - array to fill with ui dimension info
     */
    getUiInfo(depth, headers) {

        var infos = headers[headers.length - 1];
        var parents = this.axe.root.depth === depth ? [null] :
            headers[this.axe.root.depth - depth - 1].filter(function(p) {
                return p.type !== HeaderType.SUB_TOTAL;
            });

        for (var pi = 0; pi < parents.length; pi++) {

            var parent = parents[pi];
            var parentDim = parent == null ? this.axe.root : parent.dim;

            for (var di = 0; di < parentDim.values.length; di++) {

                var subvalue = parentDim.values[di];
                var subdim = parentDim.subdimvals[subvalue];

                var subtotalHeader;
                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    subtotalHeader = new Header(AxeType.COLUMNS, HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount());
                } else {
                    subtotalHeader = null;
                }

                var header = new Header(AxeType.COLUMNS, null, subdim, parent, this.dataFieldsCount(), subtotalHeader);
                infos.push(header);

                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    infos.push(subtotalHeader);
                }
            }
        }
    }
};
