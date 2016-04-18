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
        var headers = [];
        var grandtotalHeader;

        if (this.axe != null) {
            if(this.axe.root.values.length > 0 || this.axe.pgrid.config.grandTotal.rowsvisible) {
                headers.push([]);

                // Fill Rows layout infos
                this.getUiInfo(headers, this.axe.root);

                if (this.axe.pgrid.config.grandTotal.rowsvisible) {
                    var lastrow = headers[headers.length - 1];
                    grandtotalHeader = new Header(AxeType.ROWS, HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount());
                    if (lastrow.length === 0) {
                        lastrow.push(grandtotalHeader);
                    } else {
                        headers.push([grandtotalHeader]);
                    }
                }
            }

            if (headers.length === 0) {
                headers.push([grandtotalHeader = new Header(AxeType.ROWS, HeaderType.INNER, this.axe.root, null, this.dataFieldsCount())]);
            }

            if(grandtotalHeader) {
                // add grand-total data headers if more than 1 data field and they will be the leaf headers
                this.addDataHeaders(headers, grandtotalHeader);
            }
        }
        this.headers = headers;
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
     * Fills the infos array given in argument with the dimension layout infos as row.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {object}  infos - array to fill with ui dimension info
     */
    getUiInfo(infos, dimension: Dimension) {
        if (dimension.values.length > 0) {

            var infosMaxIndex = infos.length - 1;
            var lastInfosArray = infos[infosMaxIndex];
            var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

            for (var valIndex = 0; valIndex < dimension.values.length; valIndex++) {
                var subvalue = dimension.values[valIndex];
                var subdim = dimension.subdimvals[subvalue];

                var subTotalHeader;
                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    subTotalHeader = new Header(AxeType.ROWS, HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount());
                } else {
                    subTotalHeader = null;
                }

                var newHeader = new Header(AxeType.ROWS, null, subdim, parent, this.dataFieldsCount(), subTotalHeader);

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
};
