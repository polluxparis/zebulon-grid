import { AxisType } from '../Axis';
import { UPDATE_CELL_SIZE, MEASURE_ID } from '../constants';
import { updateCellSize } from './sizes.action';

describe('updateCellSize creates the correct action', () => {
  test('when making dimension wider', () => {
    const handle = { axis: AxisType.ROWS, position: 'right', id: 'tutu' };
    const offset = { x: 230 };
    const initialOffset = { x: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { dimensions: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      id: 'tutu',
      size: 225,
      axis: 'rows',
      direction: 'dimensions'
    });
  });
  test('when making dimension less wide', () => {
    const handle = { axis: AxisType.ROWS, position: 'right', id: 'tutu' };
    const offset = { x: 180 };
    const initialOffset = { x: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { dimensions: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      id: 'tutu',
      size: 175,
      axis: 'rows',
      direction: 'dimensions'
    });
  });
  test('when making dimension too narrow', () => {
    const handle = { axis: AxisType.ROWS, position: 'right', id: 'tutu' };
    const offset = { x: 0 };
    const initialOffset = { x: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { dimensions: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      id: 'tutu',
      size: 10,
      axis: 'rows',
      direction: 'dimensions'
    });
  });
  test('with dimension of custom width', () => {
    const handle = { axis: AxisType.ROWS, position: 'right', id: 'tutu' };
    const offset = { x: 230 };
    const initialOffset = { x: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { dimensions: { tutu: 400 } } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      id: 'tutu',
      size: 425,
      axis: 'rows',
      direction: 'dimensions'
    });
  });
  test('when making dimension taller', () => {
    const handle = {
      axis: AxisType.COLUMNS,
      position: 'bottom',
      id: MEASURE_ID
    };
    const offset = { y: 230 };
    const initialOffset = { y: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { columns: { dimensions: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      axis: 'columns',
      direction: 'dimensions',
      id: MEASURE_ID,
      size: 55
    });
  });
  test('when making leaf header taller', () => {
    const handle = {
      axis: AxisType.ROWS,
      position: 'bottom',
      id: '0-/-0'
    };
    const offset = { y: 230 };
    const initialOffset = { y: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { leafs: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      axis: 'rows',
      direction: 'leafs',
      id: '0-/-0',
      size: 55
    });
  });
  test('when making leaf header taller with custom size', () => {
    const handle = {
      axis: AxisType.ROWS,
      position: 'bottom',
      id: '0-/-0'
    };
    const offset = { y: 230 };
    const initialOffset = { y: 205 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { rows: { leafs: { '0-/-0': 60 } } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      axis: 'rows',
      direction: 'leafs',
      id: '0-/-0',
      size: 85
    });
  });
  test('when making leaf header wider', () => {
    const handle = {
      axis: AxisType.COLUMNS,
      position: 'right',
      id: '0-/-qty'
    };
    const offset = { x: 530 };
    const initialOffset = { x: 505 };
    const defaultCellSizes = { height: 30, width: 200 };
    const sizes = { columns: { leafs: {} } };
    expect(
      updateCellSize({ handle, offset, initialOffset, defaultCellSizes, sizes })
    ).toEqual({
      type: UPDATE_CELL_SIZE,
      axis: 'columns',
      direction: 'leafs',
      id: '0-/-qty',
      size: 225
    });
  });
});
