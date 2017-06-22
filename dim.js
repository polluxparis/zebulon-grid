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

class Axis {
    constructor(type, dimensions) {}
    adddimension(dimension, position) {}
    removedimension(dimensionId) {}
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
