import {
  pushData,
  setFields,
  setDatafields,
  setConfigProperty,
  moveField,
  toggleDatafield,
} from './actions';
import { toAggregateFunc } from './Aggregation';

export default function hydrateStore(store, config, datasource) {
  store.dispatch(pushData(datasource));

  store.dispatch(setFields(config));
  store.dispatch(setDatafields(config));

  store.dispatch(setConfigProperty(config, 'dataHeadersLocation', 'columns'));
  store.dispatch(setConfigProperty(config, 'height', 600));
  store.dispatch(setConfigProperty(config, 'width', 800));
  store.dispatch(setConfigProperty(config, 'cellHeight', 30));
  store.dispatch(setConfigProperty(config, 'cellWidth', 100));
  store.dispatch(setConfigProperty(config, 'zoom', 1));

  config.rows.forEach((fieldCaption, index) => {
    const fieldId = config.fields.find(
      field => field.caption === fieldCaption,
    ).id;
    store.dispatch(moveField(fieldId, 'fields', 'rows', index));
  });
  config.columns.forEach((fieldCaption, index) => {
    const fieldId = config.fields.find(
      field => field.caption === fieldCaption,
    ).id;
    store.dispatch(moveField(fieldId, 'fields', 'columns', index));
  });
  Object.values(config.fields)
    .filter(field => {
      const state = store.getState();
      const rows = state.axis.rows;
      const columns = state.axis.columns;
      return !(rows.includes(field.id) || columns.includes(field.id));
    })
    .forEach((field, index) => {
      store.dispatch(moveField(field.id, 'fields', 'fields', index));
    });

  config.data.forEach(fieldCaption => {
    const fieldId = config.datafields.find(
      field => field.caption === fieldCaption,
    ).id;
    store.dispatch(toggleDatafield(fieldId));
  });

  const customFunctions = {
    sort: config.fields.reduce(
      (acc, field) => ({
        ...acc,
        [field.id]: field.sort && field.sort.customfunc,
      }),
      {},
    ),
    format: config.datafields.reduce(
      (acc, field) => ({
        ...acc,
        [field.id]: field.formatFunc,
      }),
      {},
    ),
    aggregation: config.datafields.reduce(
      (acc, field) => ({
        ...acc,
        [field.id]: toAggregateFunc(field.aggregateFunc),
      }),
      {},
    ),
  };
  return customFunctions;
}
