import {
    // isString,
    isStringOrNumber,
    isNullOrUndefined,
    toAccessorFunction
} from "./utils/generic";
import * as aggregations from "./Aggregation";

// initialisation of dimensions from configuration
function dimensionFactory(dimensionConfig) {
    const {
        id,
        caption,
        keyAccessor,
        labelAccessor,
        sort,
        format,
        subTotal
    } = dimensionConfig;
    const dimSort = sort
        ? {
              direction: sort.direction,
              custom: sort.custom,
              keyAccessor: toAccessorFunction(
                  sort.keyAccessor || labelAccessor || keyAccessor
              )
          }
        : {};
    return {
        id,
        caption: caption || id,
        keyAccessor: toAccessorFunction(keyAccessor),
        labelAccessor: toAccessorFunction(labelAccessor || keyAccessor),
        format: format || (value => value),
        sort: dimSort,
        subTotal
    };
}
// initialisation of measures from configuration
function measureFactory(measureConfig) {
    const {
        id,
        caption,
        valueAccessor,
        format,
        aggregation,
        aggregationCaption
    } = measureConfig;
    return {
        id,
        caption: caption || id,
        valueAccessor: toAccessorFunction(valueAccessor),
        format: format || (value => value),
        aggregation: isStringOrNumber(aggregation)
            ? aggregations[aggregation]
            : aggregation,
        aggregationCaption
    };
}

// occurrence of dimensions referencing data rows indexes linked to this value
class DimensionValue {
    constructor(id, value, sortingValue) {
        this.id = id;
        this.value = value;
        this.sortingValue = sortingValue;
        // references to dataset row indexes
        this.dataIndexes = [];
    }
}

class Axis {
    constructor(type, dimensions) {}
    adddimension(dimension, position) {}
    removedimension(dimensionId) {}
}
// add child node to a node
function buildNode(id, node) {
    if (node.children[id] !== undefined) {
        return node.children[id];
    } else {
        node.children[id] = { id, children: {} };
        return node.children[id];
    }
}
// build one axis (colums or rows) tree
function buildAxisTree(data, dimensions) {
    return buildAxisTrees(data, { columns: dimensions, rows: [] }).columns;
}

// build colums and rows trees
function buildAxisTrees(data, { columns, rows }) {
    const rowRoot = { id: null, children: {} };
    const columnRoot = { id: null, children: {} };
    // Create sorting accessors
    data.forEach((row, index) => {
        let columnNode = columnRoot;
        let rowNode = rowRoot;
        columns.forEach(dimension => {
            columnNode = buildNode(dimension.accessor(row), columnNode);
        });
        rows.forEach(dimension => {
            rowNode = buildNode(dimension.accessor(row), rowNode);
        });
    });
    return { columns: columnNode, rows: rowNode };
}
// build a dictionary dimension:[dimension values]
function buildDimensionValues(data, dimensions, currentDimensionValues) {
    let dimensionValues;
    if (currentDimensionValues === undefined) {
        dimensionValues = dimensions.reduce(
            (acc, dimension) => ({
                ...acc,
                [dimension.id]: {}
            }),
            {}
        );
    } else {
        dimensionValues = currentDimensionValues;
    }
    data.forEach((row, index) => {
        dimensions.forEach(dimension => {
            const id = dimension.accessor(row);
            if (dimensionValues[dimension.id][id] === undefined) {
                dimensionValues[dimension.id][id] = new DimensionValue(
                    id,
                    dimension.labelAccessor(row),
                    dimension.sort.accessor(row)
                );
            }
            dimensionValues[dimension.id][id].dataIndexes.push(index);
        });
    });
    return dimensionValues;
}
class Grid {
    constructor(data, dimensions) {
        this.columns = new Axis("column", dimensions);
        this.rows = new Axis("row", dimensions);

        if (data != null && columns.length + rows.length > 0) {
            if (Array.isArray(data) && data.length > 0) {
                dims = dims(this.dimensions, data);

                // dims.forEach(dim => dim.sort);

                this.dims = dims;
            }
        }
    }
}
