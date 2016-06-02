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
export class UiCols extends AxeUi {
    leafsHeaders = [];

    constructor(columnsAxe: Axe) {
        super(columnsAxe);
        this.build();
    }

    build() {
        var headers = [];
        var grandtotalHeader;

        if (this.axe != null) {
            if (this.axe.root.values.length > 0 || this.axe.pgrid.config.grandTotal.columnsvisible) {
                headers.push([]);

                // Fill Columns layout infos
                this.getUiInfo(headers, this.axe.root);

                if (this.axe.pgrid.config.grandTotal.columnsvisible) {
                    var lastrow = headers[headers.length - 1];
                    grandtotalHeader = new Header(AxeType.COLUMNS, HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount());
                    if (lastrow.length === 0) {
                        lastrow.push(grandtotalHeader);
                    } else {
                        headers.push([grandtotalHeader]);
                    }
                }
            }

            if (headers.length === 0) {
                headers.push([grandtotalHeader = new Header(AxeType.COLUMNS, HeaderType.INNER, this.axe.root, null, this.dataFieldsCount())]);
            }

            if (grandtotalHeader) {
                // add grand-total data headers if more than 1 data field and they will be the leaf headers
                this.addDataHeaders(headers, grandtotalHeader);
            }
        }
        this.headers = headers;

    };

    addDataHeaders(infos, parent) {
        if (this.isMultiDataFields()) {
            var lastInfosArray = infos[infos.length - 1];
            for (let datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
                lastInfosArray.push(new DataHeader(this.axe.pgrid.config.dataFields[datafieldindex], parent));
                if (datafieldindex < this.dataFieldsCount() - 1) {
                    infos.push((lastInfosArray = []));
                }
            }
        }
    }

    /**
     * Fills the infos array given in argument with the dimension layout infos as row.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {object}  infos - array to fill with ui dimension info
     */
    getUiInfo(infos, dimension: Dimension) {
        if (dimension.values.length > 0) {

            var infosMaxIndex = infos.length - 1;
            var lastInfosArray = infos[infosMaxIndex];
            var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

            for (let valIndex = 0; valIndex < dimension.values.length; valIndex++) {
                var subvalue = dimension.values[valIndex];
                var subdim = dimension.subdimvals[subvalue];

                var subTotalHeader;
                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    subTotalHeader = new Header(AxeType.COLUMNS, HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount());
                } else {
                    subTotalHeader = null;
                }

                var newHeader = new Header(AxeType.COLUMNS, null, subdim, parent, this.dataFieldsCount(), subTotalHeader);

                if (valIndex > 0) {
                    infos.push((lastInfosArray = []));
                }

                lastInfosArray.push(newHeader);

                if (!subdim.isLeaf) {
                    this.getUiInfo(infos, subdim);
                    if (subdim.field.subTotal.visible) {
                        infos.push([subTotalHeader]);

                        // add sub-total data headers if more than 1 data field and they will be the leaf headers
                        this.addDataHeaders(infos, subTotalHeader);
                    }
                } else {
                    // add data headers if more than 1 data field and they will be the leaf headers
                    this.addDataHeaders(infos, newHeader);
                }
            }
        }
    }

    // generateLeafsHeaders() {
    //
    //     var leafsHeaders = [];
    //
    //     function pushsubtotal(pheader) {
    //         if (pheader && pheader.dim.field.subTotal.visible) {
    //             leafsHeaders.push(pheader.subtotalHeader);
    //         }
    //     }
    //
    //     if (this.headers.length > 0) {
    //         // last headers row
    //         var infos = this.headers[this.headers.length - 1];
    //         var header = infos[0];
    //
    //         if(header) {
    //             var currparent,
    //                 prevpar = header.parent;
    //
    //             for (let i = 0; i < infos.length; i++) {
    //                 header = infos[i];
    //                 currparent = header.parent;
    //                 // if current header parent is different than previous header parent,
    //                 // add previous parent
    //                 if (currparent != prevpar) {
    //                     pushsubtotal(prevpar);
    //                     if (currparent != null) {
    //                         // walk up parent hierarchy and add grand parents if different
    //                         // than current header grand parents
    //                         var grandpar = currparent.parent;
    //                         var prevgrandpar = prevpar ? prevpar.parent : null;
    //                         while (grandpar != prevgrandpar && prevgrandpar != null) {
    //                             pushsubtotal(prevgrandpar);
    //                             grandpar = grandpar ? grandpar.parent : null;
    //                             prevgrandpar = prevgrandpar ? prevgrandpar.parent : null;
    //                         }
    //                     }
    //                     // update previous parent variable
    //                     prevpar = currparent;
    //                 }
    //                 // push current header
    //                 leafsHeaders.push(infos[i]);
    //
    //                 // if it's the last header, add all of its parents up to the top
    //                 if (i === infos.length - 1) {
    //                     while (prevpar != null) {
    //                         pushsubtotal(prevpar);
    //                         prevpar = prevpar.parent;
    //                     }
    //                 }
    //             }
    //             // grandtotal is visible for columns and if there is more than one dimension in this axe
    //             if (this.axe.pgrid.config.grandTotal.columnsvisible && this.axe.dimensionsCount > 1) {
    //                 // push also grand total header
    //                 leafsHeaders.push(this.headers[0][this.headers[0].length - 1]);
    //             }
    //         }
    //     }
    //
    //     // add data headers if more than 1 data field and they willbe the leaf headers
    //     if (this.isMultiDataFields()) {
    //         this.leafsHeaders = [];
    //         for (let leafIndex = 0; leafIndex < leafsHeaders.length; leafIndex++) {
    //             for (let datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
    //                 this.leafsHeaders.push(new DataHeader(this.axe.pgrid.config.dataFields[datafieldindex], leafsHeaders[leafIndex]));
    //             }
    //         }
    //         this.headers.push(this.leafsHeaders);
    //     } else {
    //         this.leafsHeaders = leafsHeaders;
    //     }
    // }
};
