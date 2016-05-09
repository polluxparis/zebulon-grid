/**
 * @fileOverview Pivot Grid rows viewmodel
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
 * Creates a new instance of rows ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} rowsAxe - axe containing all rows dimensions.
 */
export class UiRows extends AxeUi{

    constructor(rowsAxe: Axe) {
        super(rowsAxe);
        this.build();
    }

    build() {
        var grandtotalHeader;

        if (this.axe != null) {
            if(this.axe.root.values.length > 0 || this.axe.pgrid.config.grandTotal.rowsvisible) {
              for (var depth = this.axe.root.depth; depth > 1; depth--) {
                  this.headers.push([]);
                  this.getUiInfo(depth, this.headers);
              }

                if (this.axe.pgrid.config.grandTotal.rowsvisible) {
                    var lastrow = this.headers[this.headers.length - 1];
                    grandtotalHeader = new Header(AxeType.ROWS, HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount());
                    if (lastrow.length === 0) {
                        lastrow.push(grandtotalHeader);
                    } else {
                        this.headers.push([grandtotalHeader]);
                    }
                }
            }

            if (this.headers.length === 0) {
                this.headers.push([grandtotalHeader = new Header(AxeType.ROWS, HeaderType.INNER, this.axe.root, null, this.dataFieldsCount())]);
            }

            if(grandtotalHeader) {
                // add grand-total data headers if more than 1 data field and they will be the leaf headers
                this.addDataHeaders(this.headers, grandtotalHeader);
            }
        }
        this.headers = this.headers;
    };

    addDataHeaders(infos, parent) {
        if (this.isMultiDataFields()) {
            var lastInfosArray = infos[infos.length - 1];
            for (var datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
                lastInfosArray.push(new DataHeader(this.axe.pgrid.config.dataFields[datafieldindex], parent));
                if (datafieldindex < this.dataFieldsCount() - 1) {
                    infos.push((lastInfosArray = []));
                }
            }
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
                    subtotalHeader = new Header(AxeType.ROWS, HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount());
                } else {
                    subtotalHeader = null;
                }

                var header = new Header(AxeType.ROWS, null, subdim, parent, this.dataFieldsCount(), subtotalHeader);
                infos.push(header);

                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    infos.push(subtotalHeader);
                }
            }
        }
    }
};
