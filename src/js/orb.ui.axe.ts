/**
 * @fileOverview Pivot Grid rows/columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

import {Axe, AxeType} from './orb.axe';
import {Header, HeaderType} from './orb.ui.header';

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
export class AxeUi{

    /**
     * Dimensions axe
     * @type {orb.axe}
     */
    public axe: Axe;

    /**
     * Headers render properties
     * @type {Array}
     */
    public headers: Array<Array<Header>>;
    constructor(axeModel){
        this.axe = axeModel;
        this.headers = [];
    }

    dataFieldsCount() {
        return (this.axe.pgrid.config.dataHeadersLocation === 'columns' && this.axe.type === AxeType.COLUMNS) ||
               (this.axe.pgrid.config.dataHeadersLocation === 'rows' && this.axe.type === AxeType.ROWS) ?
                     this.axe.pgrid.config.dataFieldsCount :
                     1;
    };

    isMultiDataFields() {
        return this.dataFieldsCount() > 1;
    };

    toggleFieldExpansion(field, newState) {
        var toToggle = [];
        var allExpanded = true;
        var hIndex;

        for(var i = 0; i < this.headers.length; i++) {
            for(hIndex = 0; hIndex < this.headers[i].length; hIndex++) {
                var header = this.headers[i][hIndex];
                if(header.type === HeaderType.SUB_TOTAL && (field == null || header.dim.field.name == field.name)) {
                    toToggle.push(header);
                    allExpanded = allExpanded && header.expanded;
                }
            }
        }

        if(newState !== undefined) {
            allExpanded = !newState;
        }

        if(toToggle.length > 0) {
            for(hIndex = 0; hIndex < toToggle.length; hIndex++) {
                if(allExpanded) {
                    toToggle[hIndex].collapse();
                } else {
                    toToggle[hIndex].expand();
                }
            }
            return true;
        }

        return false;
    };
};
