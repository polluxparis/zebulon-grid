import { AxisType } from './Axis';
import { Header, DataHeader, DimensionHeader, HeaderType } from './Cells';

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
export default class AxisUi {

  constructor(axis, config) {
    const { dataHeadersLocation, activatedDataFieldsCount } = config;
    this.datafieldsCount = this.getDataFieldsCount({ axisType: axis.type, dataHeadersLocation, activatedDataFieldsCount });

    this.hasDataFields = this.datafieldsCount >= 1;
    /**
     * Headers render properties
     * @type {Array}
     */
    this.headers = [];

    /**
     * Dimension headers render properties
     * @type {Array}
     */
    this.dimensionHeaders = [];

    this._x = 0;

    this.build({ axis, config });
  }

  build({ axis, config }) {
    const { dataHeadersLocation, activatedDataFieldsCount, activatedDataFields } = config;
    let crossAxisFieldsCode;
    if ((axis.type) === 1) {
      crossAxisFieldsCode = config.rowFields.map(field => field.code);
    } else {
      crossAxisFieldsCode = config.columnFields.map(field => field.code);
    }
    const headers = [];
    let grandtotalHeader;
    let y;

    if (axis != null) {
      if (axis.root.values.length > 0 || config.grandTotal.rowsvisible) {
        headers.push([]);
        // Fill Rows layout infos
        y = this.getUiInfo({ infos: headers, dimension: axis.root, axisType: axis.type, dataHeadersLocation, activatedDataFields, activatedDataFieldsCount });

        if (config.grandTotal.rowsvisible) {
          const lastrow = headers[headers.length - 1];
          grandtotalHeader = new Header(axis.type, HeaderType.GRAND_TOTAL, axis.root, null, this.datafieldsCount, this._x, y, null, crossAxisFieldsCode);
          if (lastrow.length === 0) {
            lastrow.push(grandtotalHeader);
          } else {
            headers.push([grandtotalHeader]);
          }
        }
      }

      if (headers.length === 0) {
        headers.push([grandtotalHeader = new Header(axis.type, HeaderType.GRAND_TOTAL, axis.root, null, this.datafieldsCount, this._x, y = 0, null, crossAxisFieldsCode)]);
      }

      if (grandtotalHeader) {
        // add grand-total data headers if more than 1 data field and they will be the leaf headers
        this.addDataHeaders({ axisType: axis.type, infos: headers, parent: grandtotalHeader, y: y + 1, dataHeadersLocation, activatedDataFields, activatedDataFieldsCount });
      }
    }
    this.headers = headers;
    this.dimensionHeaders = axis.fields.map((field, index) => new DimensionHeader(axis.type, field));
  }

  /**
  * Fills the infos array given in argument with the dimension layout infos as row.
  * @param  {orb.dimension}  dimension - the dimension to get ui info for
  * @param  {object}  infos - array to fill with ui dimension info
  * @param  {number}  axisType - type of the axe (rows or columns)
  */
  getUiInfo({ infos, dimension, axisType, dataHeadersLocation, activatedDataFields, activatedDataFieldsCount, y = 0 }) {
    if (dimension.values.length > 0) {
      const infosMaxIndex = infos.length - 1;
      let lastInfosArray = infos[infosMaxIndex];
      const parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

      for (let valIndex = 0; valIndex < dimension.values.length; valIndex++) {
        const subvalue = dimension.values[valIndex];
        const subdim = dimension.subdimvals[subvalue];

        let subTotalHeader;
        if (!subdim.isLeaf && subdim.field.subTotal.visible) {
          // x here will probably create bugs. To change when necessary
          subTotalHeader = new Header(axisType, HeaderType.SUB_TOTAL, subdim, parent, this.datafieldsCount, this._x, y);
        } else {
          subTotalHeader = null;
        }

        const newHeader = new Header(axisType, null, subdim, parent, this.datafieldsCount, this._x, y, subTotalHeader);

        if (valIndex > 0) {
          infos.push((lastInfosArray = []));
        }

        lastInfosArray.push(newHeader);

        if (!subdim.isLeaf) {
          this.getUiInfo({ infos, dimension: subdim, axisType, y: y + 1, dataHeadersLocation, activatedDataFields, activatedDataFieldsCount });
          if (subdim.field.subTotal.visible) {
            infos.push([subTotalHeader]);

            // add sub-total data headers if more than 1 data field and they will be the leaf headers
            this.addDataHeaders({ axisType, infos, activatedDataFields, activatedDataFieldsCount, dataHeadersLocation, parent: subTotalHeader, y: y + 1 });
          }
        } else {
          // add data headers if more than 1 data field and they will be the leaf headers
          this.addDataHeaders({ axisType, infos, activatedDataFields, activatedDataFieldsCount, dataHeadersLocation, parent: newHeader, y: y + 1 });
        }
      }
    }
    return y;
  }

  addDataHeaders({ axisType, infos, parent, dataHeadersLocation, activatedDataFieldsCount, activatedDataFields, y }) {
    if (this.hasDataFields) {
      let lastInfosArray = infos[infos.length - 1];
      for (let [index, datafield] of activatedDataFields.entries()) {
        lastInfosArray.push(new DataHeader(
          dataHeadersLocation === 'columns' ? AxisType.COLUMNS : AxisType.ROWS,
          datafield,
          parent,
          this._x++,
          y,
        ));
        if (index < this.datafieldsCount - 1) {
          infos.push((lastInfosArray = []));
        }
      }
    } else {
      this._x++;
    }
  }

  getDataFieldsCount({ axisType, dataHeadersLocation, activatedDataFieldsCount }) {
    if ((dataHeadersLocation === 'columns' && axisType === AxisType.COLUMNS) || (dataHeadersLocation === 'rows' && axisType === AxisType.ROWS)) {
      return activatedDataFieldsCount;
    } else {
      return 0;
    }
  }

  toggleFieldExpansion(field, newState) {
    const toToggle = [];
    let allExpanded = true;
    let hIndex;

    for (let i = 0; i < this.headers.length; i++) {
      for (hIndex = 0; hIndex < this.headers[i].length; hIndex++) {
        const header = this.headers[i][hIndex];
        if (header.type === HeaderType.SUB_TOTAL && (field == null || header.dim.field.name === field.name)) {
          toToggle.push(header);
          allExpanded = allExpanded && header.expanded;
        }
      }
    }

    if (newState !== undefined) {
      allExpanded = !newState;
    }

    if (toToggle.length > 0) {
      for (hIndex = 0; hIndex < toToggle.length; hIndex++) {
        if (allExpanded) {
          toToggle[hIndex].collapse();
        } else {
          toToggle[hIndex].expand();
        }
      }
      return true;
    }

    return false;
  }
}
