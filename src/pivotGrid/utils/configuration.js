import {
  fetchData,
  fetchFailure,
  fetchSuccess,
  loadingConfig,
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
export const defaultConfigurationFunctions = {
  formats: {},
  accessors: {},
  sorts: {},
  aggregations: {}
};
export const defaultMenuFunctions = {
  dataCellFunctions: {},
  rangeFunctions: {},
  gridFunctions: {}
};
export const defaultSizes = {
  height: 800,
  width: 1000,
  cellHeight: 30,
  cellWidth: 100,
  zoom: 1
};
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
export const applySizesToStore = (store, sizes) => {
  const prevSizes = store.getState().configuration;
  const newSizes = { ...prevSizes, ...sizes };
  store.dispatch(setConfigurationProperty(newSizes, "height", 800));
  store.dispatch(setConfigurationProperty(newSizes, "width", 1000));
  store.dispatch(setConfigurationProperty(newSizes, "zoom", 1));
  store.dispatch(setConfigurationProperty(newSizes, "cellHeight", 30));
  store.dispatch(setConfigurationProperty(newSizes, "cellWidth", 100));
};
export const applyConfigurationToStore = (
  store,
  configuration,
  configurationFunctions = defaultConfigurationFunctions,
  data
) => {
  store.dispatch(loadingConfig(true));
  if (!isNullOrUndefined(data)) {
    setData(store, data);
  }
  //  global configuration
  const measureHeadersAxis = configuration.measureHeadersAxis || "columns";
  store.dispatch(
    setConfigurationProperty(
      configuration,
      "measureHeadersAxis",
      measureHeadersAxis
    )
  );
  let activeMeasures = configuration.activeMeasures,
    columns = configuration.columns,
    rows = configuration.rows;
  if (configuration.axis) {
    activeMeasures = configuration.axis.measures || activeMeasures;
    columns = configuration.axis.columns || columns;
    rows = configuration.axis.rows || rows;
  }
  store.dispatch(setDimensions(configuration, configurationFunctions));
  store.dispatch(setMeasures(configuration, configurationFunctions));
  const dimensionIdsPositioned = [];
  rows.forEach((dimensionId, index) => {
    store.dispatch(moveDimension(dimensionId, "dimensions", "rows", index));
    dimensionIdsPositioned.push(dimensionId);
  });
  columns.forEach((dimensionId, index) => {
    store.dispatch(moveDimension(dimensionId, "dimensions", "columns", index));
    dimensionIdsPositioned.push(dimensionId);
  });
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
  store.dispatch(loadingConfig(false));
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
