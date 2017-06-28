import { EXPAND_COLLAPSE } from '../constants';
import { AxisType, toAxisType } from '../Axis';
export default (
  state = { rows: { '1': true }, columns: { 'titi 1': true } },
  action
) => {
  const { type, axisType, isCollapsed, id } = action;
  switch (type) {
    case EXPAND_COLLAPSE:
      return {
        ...state,
        [axisType]: {
          ...state[axisType],
          // [axis]: {
          //   ...state[direction][axis],
          [id]: isCollapsed
          // }
        }
      };
    default:
      return state;
  }
};
