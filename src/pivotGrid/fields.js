import { isString, isStringOrNumber, isNullOrUndefined } from './utils/generic';

export function fieldFactory(fieldConfig) {
  const { id: idConfig, accessor, name, caption, sort } = fieldConfig;
  let id;
  if (isNullOrUndefined(accessor)) {
    throw new Error(
      'Configuration error: field definition needs an accessor.',
      fieldConfig
    );
  }
  if (isNullOrUndefined(idConfig)) {
    id = accessor;
  } else {
    id = idConfig;
  }
  if (!isStringOrNumber(id)) {
    throw new Error(
      'Configuration error: field definition needs an accessor of type string or an id.',
      fieldConfig
    );
  }
  const field = { id, accessor };
  field.name = name || field.id;
  field.caption = caption || field.name;

  let sortValue;
  if (sort) {
    let accessor;
    // If the accessor is the same that the id (the default), no need to write the accessor
    // This will save memory
    if (sort.accessor && sort.accessor !== field.id) {
      accessor = sort.accessor;
    }
    sortValue = {
      order: sort.order || (sort.custom ? 'asc' : null),
      custom: sort.custom,
      accessor
    };
  } else {
    sortValue = { order: null };
  }
  field.sort = sortValue;

  field.subTotal = {};

  return field;
}

export function datafieldFactory(fieldConfig) {
  const {
    accessor,
    id: idConfig,
    name,
    caption,
    aggregationName,
    aggregation
  } = fieldConfig;
  let id;
  if (isNullOrUndefined(accessor)) {
    throw new Error(
      'Configuration error: datafield definition needs an accessor.',
      fieldConfig
    );
  }
  if (isNullOrUndefined(idConfig)) {
    id = accessor;
  } else {
    id = idConfig;
  }
  if (!isStringOrNumber(id)) {
    throw new Error(
      'Configuration error: datafield definition needs an accessor of type string or an id.',
      fieldConfig
    );
  }
  const datafield = { id, accessor };
  datafield.name = name || datafield.id;
  datafield.caption = caption || datafield.name;

  if (aggregationName) {
    datafield.aggregationName = aggregationName;
  } else if (aggregation) {
    if (isString(aggregation)) {
      datafield.aggregationName = aggregation;
    } else {
      datafield.aggregationName = 'custom';
    }
  } else {
    datafield.aggregationName = null;
  }
  return datafield;
}
