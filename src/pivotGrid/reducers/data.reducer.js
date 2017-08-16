import { PUSH_DATA, FETCH_SUCCESS } from '../constants';

export default (state = [], action) => {
	switch (action.type) {
		case PUSH_DATA:
			return [...state, ...action.payload];
		case FETCH_SUCCESS:
			return action.payload;
		default:
			return state;
	}
};
