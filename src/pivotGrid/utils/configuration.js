import {
  fetchData,
  fetchFailure,
  fetchSuccess,
  pushData,
  setDimensions,
  setMeasures,
  setConfigProperty,
  moveDimension,
  moveMeasure,
  setCollapses,
  setSizes
} from "../actions";
import {
  isPromise,
  isObservable,
  isStringOrNumber,
  isNullOrUndefined,
  toAccessorFunction
} from "./generic";
import { MEASURE_ID } from "../constants";
import * as aggregations from "./aggregation";
export const setData = (store, data) => {
  store.dispatch(fetchData());
  if (Array.isArray(data)) {
    if (data.length === 0) {
      store.dispatch(fetchFailure({ message: "No rows retrieved" }));
    } else {
      store.dispatch(fetchSuccess(data));
    }
  } else if (isPromise(data)) {
    data
      .then(data => {
        if (Array.isArray(data) && data.length !== 0) {
          store.dispatch(fetchSuccess(data));
        } else {
          store.dispatch(fetchFailure({ message: "No rows retrieved" }));
        }
      })
      .catch(error => store.dispatch(fetchFailure(error)));
  } else if (isObservable(data)) {
    data.subscribe(
      data => {
        store.dispatch(pushData(data));
      },
      error => store.dispatch(fetchFailure({ message: error }))
    );
  } else if (typeof data === "object") {
    store.dispatch(
      fetchFailure({ type: "Query definition", message: data.error })
    );
  } else {
    throw new Error(
      "datasource type is not supported, datasource must be an array, a promise or an observable, got ",
      data
    );
  }
};
export const applyConfigToStore = (
  store,
  config,
  configurationFunctions,
  data
) => {
  if (!isNullOrUndefined(data)) {
    setData(store, data);
  }
  //  global configuration
  let measureHeadersAxis = "columns",
    height = 600,
    width = 800,
    zoom = 1;
  if (config.config) {
    measureHeadersAxis = config.config.measureHeadersAxis || measureHeadersAxis;
    height = config.config.height || height;
    width = config.config.width || width;
    zoom = config.config.zoom || zoom;
  }

  store.dispatch(
    setConfigProperty(config, "measureHeadersAxis", measureHeadersAxis)
  );
  store.dispatch(setConfigProperty(config, "height", height));
  store.dispatch(setConfigProperty(config, "width", width));
  store.dispatch(setConfigProperty(config, "zoom", zoom));
  store.dispatch(setConfigProperty(config, "cellHeight", 30));
  store.dispatch(setConfigProperty(config, "cellWidth", 100));
  // dimensions and measure configuration
  let activeMeasures = config.activeMeasures,
    columns = config.columns,
    rows = config.rows;
  if (config.axis) {
    activeMeasures = config.axis.measures || activeMeasures;
    columns = config.axis.columns || columns;
    rows = config.axis.rows || rows;
  }
  config.dimensions.push({ id: MEASURE_ID, caption: "" });
  store.dispatch(setDimensions(config, configurationFunctions));
  store.dispatch(setMeasures(config, configurationFunctions));

  const dimensionIdsPositioned = [];
  // le dimensionIdsPositioned;
  rows.forEach((dimensionId, index) => {
    store.dispatch(moveDimension(dimensionId, "dimensions", "rows", index));
    dimensionIdsPositioned.push(dimensionId);
  });
  columns.forEach((dimensionId, index) => {
    store.dispatch(moveDimension(dimensionId, "dimensions", "columns", index));
    dimensionIdsPositioned.push(dimensionId);
  });
  if (!dimensionIdsPositioned.includes(MEASURE_ID)) {
    if (measureHeadersAxis === "columns") {
      store.dispatch(
        moveDimension(MEASURE_ID, "dimensions", "columns", columns.length)
      );
    } else {
      store.dispatch(
        moveDimension(MEASURE_ID, "dimensions", "rows", rows.length)
      );
    }
  }
  config.dimensions
    .filter(dimension => !dimensionIdsPositioned.includes(dimension.id))
    .forEach((dimension, index) => {
      store.dispatch(
        moveDimension(dimension.id, "dimensions", "dimensions", index)
      );
    });

  activeMeasures.forEach(measureId => {
    store.dispatch(moveMeasure(measureId));
  });
  if (config.collapses) {
    store.dispatch(setCollapses(config.collapses));
  }
  if (config.sizes) {
    store.dispatch(setSizes(config.sizes));
  }
};

// initialisation of dimensions from configuration
export function dimensionFactory(dimensionConfig, configurationFunctions) {
  const {
    id,
    caption,
    keyAccessor,
    labelAccessor,
    sort,
    format,
    subTotal,
    attributeParents
  } = dimensionConfig;
  const { formats, accessors, sorts } = configurationFunctions;
  const dimSort = !isNullOrUndefined(sort)
    ? {
        direction: sort.direction,
        custom: sorts[sort.custom] || sort.custom,
        keyAccessor: toAccessorFunction(
          accessors[sort.keyAccessor] ||
            sort.keyAccessor ||
            accessors[labelAccessor] ||
            labelAccessor ||
            accessors[keyAccessor] ||
            keyAccessor
        )
      }
    : {
        direction: "asc",
        keyAccessor: toAccessorFunction(
          accessors[labelAccessor] ||
            labelAccessor ||
            accessors[keyAccessor] ||
            keyAccessor
        )
      };
  return {
    id,
    caption: id === MEASURE_ID ? "Measures" : caption || id,
    keyAccessor: toAccessorFunction(accessors[keyAccessor] || keyAccessor),
    labelAccessor: toAccessorFunction(
      accessors[labelAccessor] ||
        labelAccessor ||
        accessors[keyAccessor] ||
        keyAccessor
    ),
    format: formats[format] || (value => value),
    sort: dimSort,
    subTotal,
    attributeParents: attributeParents || []
  };
}
// initialisation of measures from configuration
export function measureFactory(measureConfig, configurationFunctions) {
  const {
    formats,
    accessors,
    aggregations: customAggregations
  } = configurationFunctions;
  const {
    id,
    caption,
    valueAccessor,
    format,
    aggregation,
    aggregationCaption
  } = measureConfig;
  const aggs = { ...aggregations, ...customAggregations };
  return {
    id,
    caption: caption || id,
    valueAccessor: toAccessorFunction(
      accessors[valueAccessor] || valueAccessor
    ),
    format: formats[format] || (value => value),
    aggregation: isStringOrNumber(aggregation)
      ? aggs[aggregation]
      : aggregation,
    aggregationCaption
  };
}
