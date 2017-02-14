import { isString } from './utils/generic';

export function fieldFactory(fieldConfig) {
  const {
    id,
    name,
    caption,
    sort,
  } = fieldConfig;
  if (!id) {
    console.error(
      'Configuration error: field definition needs an id.',
      fieldConfig,
    );
  }
  const field = { id };
  field.name = name || field.id;
  field.caption = caption || field.name;

  let sortValue;
  if (sort) {
    sortValue = {
      order: sort.order || (sort.customfunc ? 'asc' : null),
      customfunc: sort.customfunc,
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
    id,
    name,
    caption,
    aggregateFuncName,
    aggregateFunc,
  } = fieldConfig;
  if (!id) {
    console.error(
      'Configuration error: datafield definition needs an id.',
      fieldConfig,
    );
  }
  const datafield = { id };
  datafield.name = name || datafield.id;
  datafield.caption = caption || datafield.name;

  if (aggregateFuncName) {
    datafield.aggregateFuncName = aggregateFuncName;
  } else if (aggregateFunc) {
    if (isString(aggregateFunc)) {
      datafield.aggregateFuncName = aggregateFunc;
    } else {
      datafield.aggregateFuncName = 'custom';
    }
  } else {
    datafield.aggregateFuncName = null;
  }
  return datafield;
}
