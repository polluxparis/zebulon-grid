import { MOVE_DIMENSION, SET_AXIS } from '../constants';
import { isNullOrUndefined } from '../utils/generic';
export default (state = { rows: [], columns: [], dimensions: [] }, action) => {
  const { type, id, position, oldAxis, newAxis, axis } = action;
  let newAxisValue;
  let oldAxisValue;
  let oldPosition;
  let positionToRemove;
  let newPosition = position;
  switch (type) {
    case MOVE_DIMENSION:
      if (isNullOrUndefined(newPosition) && newAxis !== oldAxis) {
        newPosition = state[newAxis].length;
      }

      if (oldAxis !== newAxis) {
        oldAxisValue = state[oldAxis].filter(dimension => dimension !== id);
        newAxisValue = [
          ...state[newAxis].slice(0, newPosition),
          id,
          ...state[newAxis].slice(newPosition)
        ];
        return { ...state, [oldAxis]: oldAxisValue, [newAxis]: newAxisValue };
      }
      oldPosition = state[oldAxis].indexOf(id);
      if (oldPosition === newPosition) {
        return state;
      }
      newAxisValue = [
        ...state[oldAxis].slice(0, newPosition),
        id,
        ...state[oldAxis].slice(newPosition)
      ];
      if (oldPosition < newPosition) {
        positionToRemove = oldPosition;
      } else {
        positionToRemove = oldPosition + 1;
      }
      newAxisValue = [
        ...newAxisValue.slice(0, positionToRemove),
        ...newAxisValue.slice(positionToRemove + 1)
      ];
      return { ...state, [newAxis]: newAxisValue };
    case SET_AXIS:
      return { ...state, ...axis };
    default:
      return state;
  }
};
