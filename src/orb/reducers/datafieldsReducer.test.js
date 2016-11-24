import datafields from './datafieldsReducer';
import { basicConfig } from '../../utils/mock';
import { toggleDatafield, setConfig } from '../actions';

const state = datafields({}, setConfig(basicConfig));

describe('datafields', () => {
  it('can be toggled', () => {
    expect(datafields(state, toggleDatafield('qty'))).toMatchSnapshot();
  });
});
