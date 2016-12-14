import reducer from './axisReducer';
import { MOVE_FIELD } from '../constants';

describe('axis reducer can move field', () => {
  it('to another axis', () => {
    const state = { rows: ['a'], columns: ['b'] };
    const newState = reducer(state, { type: MOVE_FIELD, id: 'a', oldAxis: 'rows', newAxis: 'columns', position: 0 });
    expect(newState).toEqual({ rows: [], columns: ['a', 'b'] });
  });

  it('to an empty axis', () => {
    const state = { rows: ['a'], columns: [] };
    const newState = reducer(state, { type: MOVE_FIELD, id: 'a', oldAxis: 'rows', newAxis: 'columns', position: 0 });
    expect(newState).toEqual({ rows: [], columns: ['a'] });
  });

  it('to another axis with correct position', () => {
    const state = { rows: ['a'], columns: ['b'] };
    const newState = reducer(state, { type: MOVE_FIELD, id: 'a', oldAxis: 'rows', newAxis: 'columns', position: 1 });
    expect(newState).toEqual({ rows: [], columns: ['b', 'a'] });
  });

  it('to the same axis', () => {
    const state = { rows: ['a', 'c'], columns: ['b'] };
    const newState = reducer(state, { type: MOVE_FIELD, id: 'a', oldAxis: 'rows', newAxis: 'rows', position: 2 });
    expect(newState).toEqual({ rows: ['c', 'a'], columns: ['b'] });
  });

  it('to the same position', () => {
    const state = { rows: ['a', 'c'], columns: ['b'] };
    const newState = reducer(state, { type: MOVE_FIELD, id: 'a', oldAxis: 'rows', newAxis: 'rows', position: 0 });
    expect(newState).toEqual({ rows: ['a', 'c'], columns: ['b'] });
  });
});
