import { TOGGLE_SUBTOTAL } from "../constants";

export const toggleSubTotal = dimensionId => {
	return {
		type: TOGGLE_SUBTOTAL,
		dimensionId
	};
};

