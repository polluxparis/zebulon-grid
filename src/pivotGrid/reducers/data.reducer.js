import {
	PUSH_DATA,
	FETCH_SUCCESS,
	DELETE_FILTER,
	ADD_FILTER,
	SET_MEASURES,
	MOVE_DIMENSION,
	TOGGLE_MEASURE
} from "../constants";

export default (state = { data: [], pushedData: [] }, action) => {
	switch (action.type) {
		// case DELETE_FILTER:
		// case ADD_FILTER:
		// case SET_MEASURES:
		// case MOVE_DIMENSION:
		// 	// case TOGGLE_MEASURE:
		// 	return {
		// 		...state,
		// 		pushedData: []
		// 	};
		case PUSH_DATA:
			// state.pushedData.push(...action.pushedData);
			return {
				...state,
				pushedData: action.pushedData
			};
		case FETCH_SUCCESS:
			return { data: action.data, pushedData: [] };
		default:
			return state;
	}
};
