import { isString, isStringOrNumber, isNullOrUndefined } from './utils/generic';

export function fieldFactory(fieldConfig) {
  const {
    id: idConfig,
    accessor,
    name,
    caption,
    sort
  } = fieldConfig;
  let id;
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
  const field = { id };
  field.name = name || field.id;
  field.caption = caption || field.name;

  let sortValue;
  if (sort) {
    sortValue = {
      order: sort.order || (sort.customfunc ? 'asc' : null),
      customfunc: sort.customfunc
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
