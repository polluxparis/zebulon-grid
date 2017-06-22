import { Header, DataHeader, DimensionHeader, HeaderType } from "./Cells";
import { KEY_SEPARATOR } from "./constants";

const findHeader = (headers, keys) => {
  if (keys.length === 1) {
    return headers.find(header => header.key === keys[0]);
  }
  const parentHeader = headers.find(header => header.key === keys[0]);
  if (!parentHeader) throw new Error("header not found");
  return findHeader(parentHeader.subheaders, [
    [keys[0], keys[1]].join(KEY_SEPARATOR),
    ...keys.slice(2)
  ]);
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
 * @memberOf pivotgrid.ui
 * @param  {pivotgrid.axe} axe - axe containing all dimensions.
 */

function buildHeader(data, node, dimensions, measures, depth) {
  const { id, children, dataIndexes } = node;
  const currentDimension = dimensions[depth];
  const row = data[dataIndexes[0]];
  const header = {
    sortKey: currentDimension.sort.keyAccessor(row),
    id,
    dataIndexes,
    type: "dimension",
    children: Object.keys(node.children).reduce(
      (acc, nodeId) => ({
        ...acc,
        [nodeId]: buildHeader(
          node.children[nodeId],
          dimensions,
          dimensionValues,
          depth + 1
        )
      }),
      {}
    )
  };

  if (header.children.length === 0) {
    if (measures.length !== 0) {
      // measure headers
      measures.forEach((measure, index) => {
        header.children.push({
          id: measure.id,
          type: "measure"
        });
      });
    }
  } else {
    // sort children
    const mapOrder = Object.keys(header.children).map(id => ({
      id: id,
      sortKey: header.children[id].sortKey
    }));
    const sortFunction = (a, b) => header.sort.custom(a.sortKey, b.sortKey);
    mapOrder.sort(sortFunction);
    header.mapOrder = mapOrder.map(obj => obj.id);
    if (currentDimension.sort.direction === "desc") {
      header.mapOrder.reverse();
    }
    header.hasSubTotal = currentDimension.hasSubTotal;
  }
  // x value
  x += measures.length || 1;
  return header;
}

export function buildAxisHeaders(data, axisTree, dimensions, measures) {
  axisTree.children.map(node =>
    buildHeader(node, dimensions, measures, data, 0)
  );
}

export default class AxisUi {
  constructor(axis, config, crossAxisDimensionsCode) {
    const { activatedMeasures } = config;
    this.measuresCount = activatedMeasures.length;
    // this.measuresCount = getMeasuresCount({
    //   axisType: axis.type,
    //   dataHeadersLocation,
    //
    // });

    this.hasMeasures = this.measuresCount >= 1;
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
          activatedMeasures
        });
      }

      if (headers.length === 0) {
        headers.push([
          (grandtotalHeader = new Header(
            axis.type,
            HeaderType.GRAND_TOTAL,
            axis.root,
            null,
            this.measuresCount,
            this.x,
            (y = 0),
            null,
            crossAxisDimensionsCode
          ))
        ]);
      }

      if (grandtotalHeader) {
        // add grand-total data headers if more than 1 data dimension and they will be the leaf headers
        this.addDataHeaders({
          axisType: axis.type,
          infos: headers,
          parent: grandtotalHeader,
          y: y + 1,
          activatedMeasures
        });
      }
    }
    this.headers = headers;
    this.dimensionHeaders = axis.dimensions.map(
      dimension => new DimensionHeader(axis.type, dimension)
    );
  }

  /**
  * Fills the infos array given in argument with the dimension layout infos as row.
  * @param  {pivotgrid.dimension}  dimension - the dimension to get ui info for
  * @param  {object}  infos - array to fill with ui dimension info
  * @param  {number}  axisType - type of the axe (rows or columns)
  */
  getUiInfo({ infos, dimension, axisType, activatedMeasures, y = 0 }) {
    if (dimension.values.length > 0) {
      const infosMaxIndex = infos.length - 1;
      let lastInfosArray = infos[infosMaxIndex];
      const parent = lastInfosArray.length > 0
        ? lastInfosArray[lastInfosArray.length - 1]
        : null;

      for (
        let valIndex = 0;
        valIndex < dimension.values.length;
        valIndex += 1
      ) {
        const subvalue = dimension.values[valIndex];
        const subdim = dimension.subdimvals[subvalue];

        let subTotalHeader;
        if (!subdim.isLeaf && subdim.dimension.subTotal.visible) {
          // x here will probably create bugs. To change when necessary
          subTotalHeader = new Header(
            axisType,
            HeaderType.SUB_TOTAL,
            subdim,
            parent,
            this.measuresCount,
            this.x,
            y
          );
        } else {
          subTotalHeader = null;
        }

        const newHeader = new Header(
          axisType,
          null,
          subdim,
          parent,
          this.measuresCount,
          this.x,
          y,
          subTotalHeader
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
            activatedMeasures
          });
          if (subdim.dimension.subTotal.visible) {
            infos.push([subTotalHeader]);

            // add sub-total data headers
            // if more than 1 data dimension and they will be the leaf headers
            this.addDataHeaders({
              axisType,
              infos,
              activatedMeasures,

              parent: subTotalHeader,
              y: y + 1
            });
          }
        } else {
          // add data headers if more than 1 data dimension and they will be the leaf headers
          this.addDataHeaders({
            axisType,
            infos,
            activatedMeasures,

            parent: newHeader,
            y: y + 1
          });
        }
      }
    }
    return y;
  }

  addDataHeaders({ axisType, infos, parent, activatedMeasures, y }) {
    if (this.hasMeasures) {
      let lastInfosArray = infos[infos.length - 1];
      activatedMeasures.forEach((measure, index) => {
        lastInfosArray.push(
          new DataHeader(axisType, measure, parent, this.x, y)
        );
        this.x += 1;
        if (index < this.measuresCount - 1) {
          infos.push((lastInfosArray = []));
        }
      });
    } else {
      this.x += 1;
    }
  }

  // toggleDimensionExpansion(dimension, newState) {
  //   const toToggle = [];
  //   let allExpanded = true;
  //   let hIndex;
  //
  //   for (let i = 0; i < this.headers.length; i += 1) {
  //     for (hIndex = 0; hIndex < this.headers[i].length; hIndex += 1) {
  //       const header = this.headers[i][hIndex];
  //       if (header.type === HeaderType.SUB_TOTAL && (dimension == null || header.dim.dimension.name === dimension.name)) {
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
