import {
  getActivatedDatafields,
  getAvailableFields,
  getRowFields,
  getColumnFields
} from './fields.selector';

describe('axis fields are computed correctly', () => {
  const fields = {
    field1: { id: 'field1' },
    field2: { id: 'field2' }
  };
  test('with 1 field on each axis', () => {
    const axis = { rows: ['field1'], columns: ['field2'] };

    const rowFields = getRowFields({ axis, fields });
    expect(rowFields.length).toEqual(1);
    expect(rowFields[0].id).toEqual('field1');

    const columnFields = getColumnFields({ axis, fields });
    expect(columnFields.length).toEqual(1);
    expect(columnFields[0].id).toEqual('field2');
  });

  test('with 2 fields on one axis and 0 on the other', () => {
    const axis = { rows: ['field1', 'field2'], columns: [] };

    const rowFields = getRowFields({ axis, fields });
    expect(rowFields.length).toEqual(2);

    const columnFields = getColumnFields({ axis, fields });
    expect(columnFields.length).toEqual(0);
  });
});

describe('activated datafields are computed correctly', () => {
  test('with one activated field', () => {
    const datafields = {
      datafield1: { id: 'datafield1', activated: true },
      datafield2: { id: 'datafield2', activated: false }
    };
    const actual = getActivatedDatafields({ datafields });
    expect(actual.length).toEqual(1);
    expect(actual[0]).toEqual(datafields.datafield1);
  });
  test('with no activated field', () => {
    const datafields = {
      datafield1: { id: 'datafield1', activated: false },
      datafield2: { id: 'datafield2', activated: false }
    };
    const actual = getActivatedDatafields({ datafields });
    expect(actual.length).toEqual(0);
  });
});

describe('available datafields are computed correctly', () => {
  const fields = {
    field1: { id: 'field1' },
    field2: { id: 'field2' }
  };
  test('with one available field', () => {
    const axis = { fields: ['field2'] };
    const actual = getAvailableFields({ fields, axis });
    expect(actual.length).toEqual(1);
    expect(actual[0]).toEqual({ id: 'field2' });
  });
  test('with no available field', () => {
    const axis = { fields: [] };
    const actual = getAvailableFields({ fields, axis });
    expect(actual.length).toEqual(0);
  });
  test('with all available field', () => {
    const axis = { fields: ['field2', 'field1'] };
    const actual = getAvailableFields({ fields, axis });
    expect(actual.length).toEqual(2);
    expect(actual).toEqual([{ id: 'field2' }, { id: 'field1' }]);
  });
});
