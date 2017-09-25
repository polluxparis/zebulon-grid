import { PUSH_DATA, FETCH_SUCCESS, FETCH_DATA } from "../constants";

export default (state = [], action) => {
	switch (action.type) {
		case FETCH_DATA:
			return [];
		case PUSH_DATA:
			return [...state, ...action.payload];
		case FETCH_SUCCESS:
			return action.payload;
		default:
			return state;
	}
};
