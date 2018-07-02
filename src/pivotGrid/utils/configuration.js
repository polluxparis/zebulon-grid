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
  setAxis,
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
export const setData = (store, data, meta, loadingConfig) => {
  if (!loadingConfig) {
    const axisTrees = getAxisTreesSelector(store.getState());
    if (axisTrees.rows) {
      resetLeaves(axisTrees.rows);
    }
    if (axisTrees.columns) {
      resetLeaves(axisTrees.columns);
    }
    resetDimensions(store.getState().dimensions);
  }
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
  Object.keys(sizes).forEach(key =>
    store.dispatch(setProperty(key, sizes[key]))
  );
};
export const applyConfigurationToStore = (
  store,
  newConfiguration,
  functions,
  data
) => {
  store.dispatch(loadingConfig(true));
  // data
  if (!utils.isNullOrUndefined(data)) {
    setData(store, data, undefined, true);
  }
  const configuration = { ...store.getState(), ...newConfiguration };
  //  global configuration
  Object.keys(configuration.configuration).forEach(key =>
    store.dispatch(setProperty(key, configuration.configuration[key]))
  );
  const axis = configuration.axis || {
    rows: configuration.dimensions.map(d => d.id),
    columns: [],
    measures: configuration.measure.map(m => m.id),
    measuresAxis: "columns"
  };
  store.dispatch(setDimensions(configuration, functions, configuration.object));
  store.dispatch(setMeasures(configuration, functions, configuration.object));
  const positionedDimensions = axis.rows
    .concat(axis.columns)
    .reduce((acc, dimensionId) => {
      acc[dimensionId] = true;
      return acc;
    }, {});
  axis.dimensions = configuration.dimensions
    .filter(dimension => !positionedDimensions[dimension.id])
    .map(dimension => dimension.id);
  store.dispatch(setAxis(axis));
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
  store.dispatch(loadingConfig(false));
};
export function dimensionFactory(dimensionConfiguration, functions, object) {
  const {
    id,
    caption,
    keyAccessor,
    labelAccessor,
    localLabelAccessor,
    sort,
    format,
    localFormat,
    subTotal,
    attributeParents,
    isLocal
  } = dimensionConfiguration;
  const sort_ = sort || {};

  const keyAccessor_ = keyAccessor || `row.${id}`,
    labelAccessor_ = labelAccessor || keyAccessor;
  let orderFunction,
    keyAccessorFunction,
    sortKeyAccessor_ =
      sort_.keyAccessor || localLabelAccessor || labelAccessor_;
  // if the key is multiple
  if (Array.isArray(keyAccessor_)) {
    const keys = keyAccessor_.map(key => key.replace("row.", ""));
    keyAccessorFunction = ({ row }) =>
      keys.reduce((acc, key) => `${acc}||${row[key]}`, "");
  } else {
    keyAccessorFunction = functions.getAccessorFunction(
      object,
      "accessors",
      keyAccessor_
    );
  }
  if (
    Array.isArray(sort_.localKeyAccessor || sortKeyAccessor_) &&
    !(sort_.localOrderFunctionAccessor || sort_.orderFunctionAccessor)
  ) {
    const keys = (sort_.localKeyAccessor || sortKeyAccessor_)
      .map(key => key.replace("row.", ""));
    orderFunction = (a, b) => {
      return keys.reduce((acc, key) => {
        return acc !== 0 ? acc : (a[key] > b[key]) - (b[key] > a[key]);
      }, 0);
    };
  } else if (sort_.localOrderFunctionAccessor || sort_.orderFunctionAccessor) {
    orderFunction = functions.getAccessorFunction(
      object,
      "sorts",
      sort_.localOrderFunctionAccessor || sort_.orderFunctionAccessor
    );
  }

  const dimSort = {
    direction: sort_.direction || "asc",
    orderFunction,
    keyAccessorFunction: functions.getAccessorFunction(
      object,
      "accessors",
      sort_.localKeyAccessor || sortKeyAccessor_
    ),
    keyAccessor: sortKeyAccessor_,
    localKeyAccessor: sort_.localKeyAccessor,
    orderFunctionAccessor: sort_.orderFunctionAccessor,
    localOrderFunctionAccessor: sort_.localOrderFunctionAccessor
  };
  return {
    id,
    caption: id === MEASURE_ID ? "Measures" : caption || id,
    keyAccessorFunction,
    keyAccessor: keyAccessor_,
    labelAccessorFunction: functions.getAccessorFunction(
      object,
      "accessors",
      localLabelAccessor || labelAccessor_
    ),
    labelAccessor: labelAccessor_,
    localLabelAccessor,
    formatFunction: functions.getAccessorFunction(
      object,
      "formats",
      localFormat || format
    ),
    format,
    localFormat,
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
    localFormat,
    aggregation,
    aggregationCaption,
    isLocal
  } = measureConfiguration;
  return {
    id,
    caption: caption || id,
    valueAccessorFunction: functions.getAccessorFunction(
      object,
      "accessors",
      valueAccessor
    ),
    valueAccessor,
    formatFunction: functions.getAccessorFunction(
      object,
      "formats",
      localFormat || format || id
    ),
    format: format || id,
    localFormat,
    aggregationFunction: functions.getAccessorFunction(
      object,
      "aggregations",
      aggregation
    ),
    aggregation,
    aggregationCaption,
    isLocal
  };
}
