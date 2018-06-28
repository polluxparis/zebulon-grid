import {
  fetchData,
  fetchFailure,
  fetchSuccess,
  loadingConfig,
  applyPushedData,
  setDimensions,
  setMeasures,
  setConfigurationProperty,
  setProperty,
  moveDimension,
  moveMeasure,
  setCollapses,
  setSizes,
  addFilter,
  toggleSubTotal
} from "../actions";
import { resetLeaves, resetDimensions } from "../utils/headers";
import { getAxisTreesSelector } from "../selectors";
import { constants, utils } from "zebulon-controls";
import * as aggregations from "./aggregation";
const { MEASURE_ID } = constants;

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
  cellHeight: 25,
  cellWidth: 100,
  zoom: 1
};
export const setData = (store, data, meta) => {
  const axisTrees = getAxisTreesSelector(store.getState());
  if (axisTrees.rows) {
    resetLeaves(axisTrees.rows);
  }
  if (axisTrees.columns) {
    resetLeaves(axisTrees.columns);
  }
  resetDimensions(store.getState().dimensions);
  store.dispatch(fetchData());
  if (Array.isArray(data)) {
    if (data.length === 0) {
      store.dispatch(fetchFailure({ message: "No rows retrieved" }));
    } else {
      store.dispatch(fetchSuccess(data, meta));
    }
  } else if (utils.isPromise(data)) {
    data
      .then(data => {
        if (Array.isArray(data) && data.length !== 0) {
          store.dispatch(fetchSuccess(data));
        } else {
          store.dispatch(fetchFailure({ message: "No rows retrieved" }));
        }
      })
      .catch(error => store.dispatch(fetchFailure(error)));
  } else if (utils.isObservable(data)) {
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
  store.dispatch(setConfigurationProperty(newSizes, "cellHeight", 25));
  store.dispatch(setConfigurationProperty(newSizes, "cellWidth", 100));
};
export const applyConfigurationToStore = (
  store,
  configuration,
  functions,
  data
) => {
  store.dispatch(loadingConfig(true));
  if (!utils.isNullOrUndefined(data)) {
    setData(store, data);
  }
  const object = configuration.object || "dataset";
  //  global configuration
  store.dispatch(setProperty("object", object));
  const measureHeadersAxis =
    (configuration.configuration || {}).measureHeadersAxis ||
    configuration.measureHeadersAxis ||
    "columns";
  store.dispatch(setProperty("measureHeadersAxis", measureHeadersAxis));
  const totalsFirst =
    (configuration.configuration || {}).totalsFirst ||
    configuration.totalsFirst ||
    false;
  store.dispatch(setProperty("totalsFirst", totalsFirst));
  store.dispatch(setProperty("edition", configuration.edition || {}));
  store.dispatch(
    setConfigurationProperty(configuration, "features", {
      dimensions: "enabled",
      measures: "enabled",
      resize: "enabled",
      expandCollapse: "enabled",
      totals: "enabled",
      filters: "enabled",
      sorting: "enabled",
      configuration: "enabled"
    })
  );
  let activeMeasures = configuration.activeMeasures,
    columns = configuration.columns,
    rows = configuration.rows;
  if (configuration.axis) {
    activeMeasures = configuration.axis.measures || activeMeasures;
    columns = configuration.axis.columns || columns;
    rows = configuration.axis.rows || rows;
  }
  store.dispatch(setDimensions(configuration, functions, object));
  store.dispatch(setMeasures(configuration, functions, object));
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
  if (activeMeasures) {
    activeMeasures.forEach(measureId => {
      store.dispatch(moveMeasure(measureId));
    });
  }
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
  if (configuration.filters) {
    Object.values(
      configuration.filters
    ).forEach(({ dimensionId, operator, term, exclude, values }) =>
      store.dispatch(addFilter(dimensionId, operator, term, exclude, values))
    );
  }
  if (configuration.subtotals) {
    Object.keys(configuration.subtotals).forEach(dimensionId =>
      store.dispatch(toggleSubTotal(dimensionId))
    );
  }
  if (configuration.configuration) {
    if (configuration.configuration.zoom) {
      store.dispatch(
        setConfigurationProperty(configuration.configuration, "zoom", 1)
      );
    }
  }
  store.dispatch(setProperty("callbacks", configuration.callbacks || {}));
  store.dispatch(loadingConfig(false));
};
export function dimensionFactory(dimensionConfiguration, functions, object) {
  const {
    id,
    caption,
    keyAccessor,
    labelAccessor,
    sort,
    format,
    subTotal,
    attributeParents,
    isLocal
  } = dimensionConfiguration;
  const _sort = sort || {};
  const keyAccessor_ = keyAccessor || `row.${id}`,
    labelAccessor_ = labelAccessor || keyAccessor,
    keyAccessors_ = _sort.keyAccessor || labelAccessor || keyAccessor;
  const kAccessor = functions.getAccessorFunction(
      object,
      "accessors",
      keyAccessor
    ),
    lAccessor = functions.getAccessorFunction(
      object,
      "accessors",
      labelAccessor_
    ),
    sAccessor = functions.getAccessorFunction(
      object,
      "accessors",
      keyAccessors_
    );

  const dimSort = {
    direction: _sort.direction || "asc",
    custom: functions.getAccessorFunction(object, "sorts", _sort.custom),
    custom_: _sort.custom,
    keyAccessor: sAccessor,
    keyAccessor_: keyAccessors_
  };
  return {
    id,
    caption: id === MEASURE_ID ? "Measures" : caption || id,
    keyAccessor: kAccessor,
    keyAccessor_,
    labelAccessor: lAccessor,
    labelAccessor_,
    format: functions.getAccessorFunction(object, "formats", format),
    format_: format,
    sort: dimSort,
    subTotal,
    attributeParents: attributeParents || [],
    isLocal
  };
}
// initialisation of measures from configuration
export function measureFactory(measureConfiguration, functions, object) {
  const {
    id,
    caption,
    valueAccessor,
    format,
    aggregation,
    aggregationCaption,
    isLocal
  } = measureConfiguration;
  return {
    id,
    caption: caption || id,
    valueAccessor: functions.getAccessorFunction(
      object,
      "accessors",
      valueAccessor
    ),
    valueAccessor_: valueAccessor,
    format: functions.getAccessorFunction(object, "formats", format || id),
    format_: format || id,
    aggregation: functions.getAccessorFunction(
      object,
      "aggregations",
      aggregation
    ),
    aggregation_: aggregation,
    aggregationCaption,
    isLocal
  };
}
