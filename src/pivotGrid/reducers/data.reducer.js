import { FETCH_DATA, PUSH_DATA, FETCH_SUCCESS } from "../constants";

export default (
	state = { data: [], pushedData: [], meta: undefined },
	action
) => {
	switch (action.type) {
		case PUSH_DATA:
			return {
				...state,
				pushedData: action.pushedData
			};
		case FETCH_DATA:
			return { data: [], pushedData: [] };
		case FETCH_SUCCESS:
			// console.log(FETCH_SUCCESS, action.data);
			return {
				data: action.data,
				pushedData: [],
				meta: action.meta || state.meta
			};
		default:
			return state;
	}
};
