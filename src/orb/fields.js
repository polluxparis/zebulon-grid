import { isString } from './utils/generic';

export function fieldFactory(fieldConfig) {
  const {
    id,
    name,
    caption,
    sort,
  } = fieldConfig;
  if (!id) {
    console.error('Configuration error: field definition needs an id.');
  }
  const res = { id };
  res.name = name || res.id;
  res.caption = caption || res.name;

  let sortValue;
  if (sort) {
    sortValue = {
      order: sort.order || (sort.customfunc ? 'asc' : null),
      customfunc: sort.customfunc,
    };
  } else {
    sortValue = { order: null };
  }
  res.sort = sortValue;

  res.subTotal = {};

  return res;
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
    console.error('Configuration error: field definition needs an id.', fieldConfig);
  }
  const res = { id };
  res.name = name || res.id;
  res.caption = caption || res.name;

  if (aggregateFuncName) {
    res.aggregateFuncName = aggregateFuncName;
  } else if (aggregateFunc) {
    if (isString(aggregateFunc)) {
      res.aggregateFuncName = aggregateFunc;
    } else {
      res.aggregateFuncName = 'custom';
    }
  } else {
    res.aggregateFuncName = null;
  }
  return res;
}
