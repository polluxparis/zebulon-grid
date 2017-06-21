import {
  isString,
  isStringOrNumber,
  isNullOrUndefined,
  toAccessorFunction
} from "./utils/generic";
import * as aggregations from "./Aggregation";

export function dimensionFactory(dimensionConfig) {
  const {
    id: idConfig,
    accessor,
    label,
    caption,
    sort,
    format
  } = dimensionConfig;
  let id;
  if (isNullOrUndefined(accessor)) {
    throw new Error(
      "Pivot grid configuration error: dimension definition needs an accessor.",
      dimensionConfig
    );
  }
  if (isNullOrUndefined(idConfig) && isStringOrNumber(accessor)) {
    id = accessor;
  } else {
    id = idConfig;
  }
  if (!isStringOrNumber(id)) {
    throw new Error(
      "Pivot grid configuration error: dimension definition needs an accessor of type string or an id.",
      dimensionConfig
    );
  }
  const accessorFunction = toAccessorFunction(accessor);
  const dimension = { id, accessor: accessorFunction };
  dimension.labelAccessor = toAccessorFunction(label || dimension.id);
  dimension.caption = caption;
  dimension.format = format || (value => value);

  let sortValue;
  if (sort) {
    let accessor;
    // If the accessor is the same that the id (the default), no need to write the accessor
    // This will save memory
    if (!isNullOrUndefined(sort.accessor) && sort.accessor !== dimension.id) {
      accessor = sort.accessor;
    }
    sortValue = {
      order: sort.order || (sort.custom ? "asc" : null),
      custom: sort.custom,
      accessor: toAccessorFunction(accessor)
    };
  } else {
    sortValue = { order: null };
  }
  dimension.sort = sortValue;

  dimension.subTotal = {};

  return dimension;
}

export function measureFactory(dimensionConfig) {
  const {
    accessor,
    id: idConfig,
    name,
    caption,
    aggregationName,
    aggregation,
    format
  } = dimensionConfig;
  let id;
  if (isNullOrUndefined(accessor)) {
    throw new Error(
      "Pivot grid configuration error: measure definition needs an accessor.",
      dimensionConfig
    );
  }
  if (isNullOrUndefined(idConfig) && isStringOrNumber(accessor)) {
    id = accessor;
  } else {
    id = idConfig;
  }
  if (!isStringOrNumber(id)) {
    throw new Error(
      "Pivot grid configuration error: measure definition needs an accessor of type string or an id.",
      dimensionConfig
    );
  }
  const accessorFunction = toAccessorFunction(accessor);
  const measure = { id, accessor: accessorFunction, format };
  measure.name = name || measure.id;
  measure.caption = caption || measure.name;

  if (aggregationName) {
    measure.aggregationName = aggregationName;
  } else if (aggregation) {
    if (isString(aggregation)) {
      measure.aggregationName = aggregation;
    } else {
      measure.aggregationName = "custom";
    }
  } else {
    measure.aggregationName = null;
  }
  if (isStringOrNumber(aggregation)) {
    measure.aggregation = aggregations[aggregation] || null;
  } else if (typeof aggregation === "function") {
    measure.aggregation = aggregation;
  } else {
    throw new Error(
      "Pivot grid configuration error: measure aggregation must be a string referencing an already implemented function (cf documentation) or a function with signature (accessor, intersection, data) => number"
    );
  }

  return measure;
}
