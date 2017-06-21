import { MOVE_DIMENSION, SET_AXIS } from "../constants";

export default (state = { rows: [], columns: [], dimensions: [] }, action) => {
  const { type, id, position, oldAxis, newAxis, axis } = action;
  let newAxisValue;
  let oldAxisValue;
  let oldPosition;
  let positionToRemove;
  switch (type) {
    case MOVE_DIMENSION:
      if (oldAxis !== newAxis) {
        oldAxisValue = state[oldAxis].filter(dimension => dimension !== id);
        newAxisValue = [
          ...state[newAxis].slice(0, position),
          id,
          ...state[newAxis].slice(position)
        ];
        return { ...state, [oldAxis]: oldAxisValue, [newAxis]: newAxisValue };
      }
      oldPosition = state[oldAxis].indexOf(id);
      if (oldPosition === position) {
        return state;
      }
      newAxisValue = [
        ...state[oldAxis].slice(0, position),
        id,
        ...state[oldAxis].slice(position)
      ];
      if (oldPosition < position) {
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
