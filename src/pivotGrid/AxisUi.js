import { AxisType } from './Axis';
import { Header, DataHeader, DimensionHeader, HeaderType } from './Cells';
import { KEY_SEPARATOR } from './constants';

function getDataFieldsCount({ axisType, dataHeadersLocation, activatedDataFieldsCount }) {
  if ((dataHeadersLocation === 'columns' && axisType === AxisType.COLUMNS) || (dataHeadersLocation === 'rows' && axisType === AxisType.ROWS)) {
    return activatedDataFieldsCount;
  }
  return 0;
}

const findHeader = (headers, keys) => {
  if (keys.length === 1) {
    return headers.find(header => header.value === keys[0]);
  }
  const parentHeader = headers.find(header => header.value === keys[0]);
  if (!parentHeader) throw new Error('header not found');
  return findHeader(parentHeader.subheaders, keys.slice(1));
};

export const keyToIndex = (headers, key) => {
  const keys = key.split(KEY_SEPARATOR);
  const depth = headers[0].length;
  const ancestorHeaders = headers
    .filter(headersRow => headersRow.length === depth)
    .map(headersRow => headersRow[0]);
  try {
    return findHeader(ancestorHeaders, keys).x;
  } catch (e) {
    // console.error(`Header with key ${key} not found in following headers`, headers);
    return -1;
  }
};

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
export default class AxisUi {

  constructor(axis, config, crossAxisFieldsCode) {
    const { dataHeadersLocation, activatedDataFieldsCount, activatedDataFields } = config;
    this.datafieldsCount = getDataFieldsCount({
      axisType: axis.type,
      dataHeadersLocation,
      activatedDataFieldsCount,
    });

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

    this.x = 0;
    const headers = [];
    let grandtotalHeader;
    let y;

    if (axis != null) {
      if (axis.root.values.length > 0) {
        headers.push([]);
        // Fill Rows layout infos
        y = this.getUiInfo({
          infos: headers,
          dimension: axis.root,
          axisType: axis.type,
          dataHeadersLocation,
          activatedDataFields,
          activatedDataFieldsCount,
        });
      }

      if (headers.length === 0) {
        headers.push([grandtotalHeader = new Header(
          axis.type,
          HeaderType.GRAND_TOTAL,
          axis.root,
          null,
          this.datafieldsCount,
          this.x,
          y = 0,
          null,
          crossAxisFieldsCode,
        )]);
      }

      if (grandtotalHeader) {
        // add grand-total data headers if more than 1 data field and they will be the leaf headers
        this.addDataHeaders({
          axisType: axis.type,
          infos: headers,
          parent: grandtotalHeader,
          y: y + 1,
          dataHeadersLocation,
          activatedDataFields,
          activatedDataFieldsCount,
        });
      }
    }
    this.headers = headers;
    this.dimensionHeaders = axis.fields.map(field => new DimensionHeader(axis.type, field));
  }


  /**
  * Fills the infos array given in argument with the dimension layout infos as row.
  * @param  {orb.dimension}  dimension - the dimension to get ui info for
  * @param  {object}  infos - array to fill with ui dimension info
  * @param  {number}  axisType - type of the axe (rows or columns)
  */
  getUiInfo({
    infos,
    dimension,
    axisType,
    dataHeadersLocation,
    activatedDataFields,
    activatedDataFieldsCount,
    y = 0,
  }) {
    if (dimension.values.length > 0) {
      const infosMaxIndex = infos.length - 1;
      let lastInfosArray = infos[infosMaxIndex];
      const parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

      for (let valIndex = 0; valIndex < dimension.values.length; valIndex += 1) {
        const subvalue = dimension.values[valIndex];
        const subdim = dimension.subdimvals[subvalue];

        let subTotalHeader;
        if (!subdim.isLeaf && subdim.field.subTotal.visible) {
          // x here will probably create bugs. To change when necessary
          subTotalHeader = new Header(
            axisType,
            HeaderType.SUB_TOTAL,
            subdim,
            parent,
            this.datafieldsCount,
            this.x,
            y,
          );
        } else {
          subTotalHeader = null;
        }

        const newHeader = new Header(
          axisType,
          null,
          subdim,
          parent,
          this.datafieldsCount,
          this.x,
          y,
          subTotalHeader,
        );

        if (valIndex > 0) {
          infos.push((lastInfosArray = []));
        }

        lastInfosArray.push(newHeader);

        if (!subdim.isLeaf) {
          this.getUiInfo({
            infos,
            dimension: subdim,
            axisType,
            y: y + 1,
            dataHeadersLocation,
            activatedDataFields,
            activatedDataFieldsCount,
          });
          if (subdim.field.subTotal.visible) {
            infos.push([subTotalHeader]);

            // add sub-total data headers
            // if more than 1 data field and they will be the leaf headers
            this.addDataHeaders({ axisType,
              infos,
              activatedDataFields,
              activatedDataFieldsCount,
              dataHeadersLocation,
              parent: subTotalHeader,
              y: y + 1,
            });
          }
        } else {
          // add data headers if more than 1 data field and they will be the leaf headers
          this.addDataHeaders({ axisType,
            infos,
            activatedDataFields,
            activatedDataFieldsCount,
            dataHeadersLocation,
            parent: newHeader,
            y: y + 1,
          });
        }
      }
    }
    return y;
  }

  addDataHeaders({
    infos,
    parent,
    dataHeadersLocation,
    activatedDataFields,
    y,
  }) {
    if (this.hasDataFields) {
      let lastInfosArray = infos[infos.length - 1];
      for (let [index, datafield] of activatedDataFields.entries()) {
        lastInfosArray.push(new DataHeader(
          dataHeadersLocation === 'columns' ? AxisType.COLUMNS : AxisType.ROWS,
          datafield,
          parent,
          this.x += 1,
          y,
        ));
        if (index < this.datafieldsCount - 1) {
          infos.push((lastInfosArray = []));
        }
      }
    } else {
      this.x += 1;
    }
  }

  // toggleFieldExpansion(field, newState) {
  //   const toToggle = [];
  //   let allExpanded = true;
  //   let hIndex;
  //
  //   for (let i = 0; i < this.headers.length; i += 1) {
  //     for (hIndex = 0; hIndex < this.headers[i].length; hIndex += 1) {
  //       const header = this.headers[i][hIndex];
  //       if (header.type === HeaderType.SUB_TOTAL && (field == null || header.dim.field.name === field.name)) {
  //         toToggle.push(header);
  //         allExpanded = allExpanded && header.expanded;
  //       }
  //     }
  //   }
  //
  //   if (newState !== undefined) {
  //     allExpanded = !newState;
  //   }
  //
  //   if (toToggle.length > 0) {
  //     for (hIndex = 0; hIndex < toToggle.length; hIndex += 1) {
  //       if (allExpanded) {
  //         toToggle[hIndex].collapse();
  //       } else {
  //         toToggle[hIndex].expand();
  //       }
  //     }
  //     return true;
  //   }
  //
  //   return false;
  // }
}
