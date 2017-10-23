import {
  fetchData,
  fetchFailure,
  fetchSuccess,
  applyPushedData,
  setDimensions,
  setMeasures,
  setConfigurationProperty,
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
// import { mergeData } from "../selectors";
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
        pushData(store, data);
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
export const pushData = (store, data) => store.dispatch(applyPushedData(data));

export const applyConfigurationToStore = (
  store,
  configuration,
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
  if (configuration.configuration) {
    measureHeadersAxis =
      configuration.configuration.measureHeadersAxis || measureHeadersAxis;
    height = configuration.configuration.height || height;
    width = configuration.configuration.width || width;
    zoom = configuration.configuration.zoom || zoom;
  }

  store.dispatch(
    setConfigurationProperty(
      configuration,
      "measureHeadersAxis",
      measureHeadersAxis
    )
  );
  store.dispatch(setConfigurationProperty(configuration, "height", height));
  store.dispatch(setConfigurationProperty(configuration, "width", width));
  store.dispatch(setConfigurationProperty(configuration, "zoom", zoom));
  store.dispatch(setConfigurationProperty(configuration, "cellHeight", 30));
  store.dispatch(setConfigurationProperty(configuration, "cellWidth", 100));
  // dimensions and measure configuration
  let activeMeasures = configuration.activeMeasures,
    columns = configuration.columns,
    rows = configuration.rows;
  if (configuration.axis) {
    activeMeasures = configuration.axis.measures || activeMeasures;
    columns = configuration.axis.columns || columns;
    rows = configuration.axis.rows || rows;
  }
  // configuration.dimensions.push({ id: MEASURE_ID, caption: "" });
  store.dispatch(setDimensions(configuration, configurationFunctions));
  store.dispatch(setMeasures(configuration, configurationFunctions));

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
  // if (!dimensionIdsPositioned.includes(MEASURE_ID)) {
  //   if (measureHeadersAxis === "columns") {
  //     store.dispatch(
  //       moveDimension(MEASURE_ID, "dimensions", "columns", columns.length)
  //     );
  //   } else {
  //     store.dispatch(
  //       moveDimension(MEASURE_ID, "dimensions", "rows", rows.length)
  //     );
  //   }
  // }
  configuration.dimensions
    .filter(dimension => !dimensionIdsPositioned.includes(dimension.id))
    .forEach((dimension, index) => {
      store.dispatch(
        moveDimension(dimension.id, "dimensions", "dimensions", index)
      );
    });

  activeMeasures.forEach(measureId => {
    store.dispatch(moveMeasure(measureId));
  });
  if (configuration.collapses) {
    store.dispatch(
      setCollapses({
        ...configuration.collapses,
        configurationRows: configuration.collapses.rows,
        configurationColumns: configuration.collapses.columns
      })
    );
  }
  if (configuration.sizes) {
    store.dispatch(setSizes(configuration.sizes));
  }
};

// initialisation of dimensions from configuration
export function dimensionFactory(
  dimensionConfiguration,
  configurationFunctions
) {
  const {
    id,
    caption,
    keyAccessor,
    labelAccessor,
    sort,
    format,
    subTotal,
    attributeParents
  } = dimensionConfiguration;
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
export function measureFactory(measureConfiguration, configurationFunctions) {
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
  } = measureConfiguration;
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
