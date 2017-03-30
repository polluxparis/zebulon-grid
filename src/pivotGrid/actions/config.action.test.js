import {
  SET_FIELDS,
  SET_DATAFIELDS,
  SET_CONFIG_PROPERTY,
  TOGGLE_DATAFIELD,
  MOVE_FIELD
} from '../constants';
import {
  setConfigProperty,
  toggleDatafield,
  moveField,
  setFields,
  setDatafields
} from './config.action';

describe('setConfigProperty creates an action to set a config property', () => {
  test('when a property  value is given', () => {
    const configObject = { toto: 33 };
    expect(setConfigProperty(configObject, 'toto', 666)).toEqual({
      type: SET_CONFIG_PROPERTY,
      property: 'toto',
      value: 33
    });
  });
  test('when no property  value is given', () => {
    const configObject = {};
    expect(setConfigProperty(configObject, 'toto', 666)).toEqual({
      type: SET_CONFIG_PROPERTY,
      property: 'toto',
      value: 666
    });
  });
});

describe('toggleDatafield create correct action', () => {
  test('in normal case', () => {
    expect(toggleDatafield('toto')).toEqual({
      type: TOGGLE_DATAFIELD,
      id: 'toto'
    });
  });
});

describe('moveField creates correct action', () => {
  test('in normal case', () => {
    expect(moveField('toto', 'fields', 'rows', 0)).toEqual({
      type: MOVE_FIELD,
      id: 'toto',
      oldAxis: 'fields',
      newAxis: 'rows',
      position: 0
    });
  });
});

describe('setFields creates correct action', () => {
  test('with three fields', () => {
    const fields = [
      {
        name: 'toto_lb',
        id: 'toto',
        caption: 'Toto',
        sort: {
          order: 'asc'
        }
      },
      {
        id: 'titi',
        caption: 'Titi'
      },
      {
        id: 'tutu',
        caption: 'Tutu'
      }
    ];
    expect(setFields({ fields })).toEqual({
      type: SET_FIELDS,
      fields: [
        {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: {
            order: 'asc'
          },
          subTotal: {}
        },
        {
          id: 'titi',
          name: 'titi',
          caption: 'Titi',
          sort: {
            order: null
          },
          subTotal: {}
        },
        {
          id: 'tutu',
          name: 'tutu',
          caption: 'Tutu',
          sort: {
            order: null
          },
          subTotal: {}
        }
      ]
    });
  });
});

describe('setDatafields creates correct action', () => {
  test('with two datafields', () => {
    const datafields = [
      {
        id: 'qty',
        caption: 'Quantity',
        aggregation: 'sum'
      },
      {
        id: 'amt',
        caption: 'Amount',
        aggregation: 'sum',
        aggregationName: 'whatever',
        render: value => {
          if (value || value === 0) {
            return `${Number(value).toFixed(0)} $`;
          }
          return '';
        }
      }
    ];
    expect(setDatafields({ datafields })).toEqual({
      type: SET_DATAFIELDS,
      datafields: [
        {
          id: 'qty',
          name: 'qty',
          caption: 'Quantity',
          aggregationName: 'sum'
        },
        {
          id: 'amt',
          name: 'amt',
          caption: 'Amount',
          aggregationName: 'whatever'
        }
      ]
    });
  });
});
