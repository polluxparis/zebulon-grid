// import React from "react";
// export function id(value) {
// 	return (
// 		<div style={{ color: "blue", textAlign: "right" }}>
// 			{value}
// 		</div>
// 	);
// }

// export function date(value) {
// 	if (value instanceof Date) {
// 		return (
// 			<div style={{ color: "black", textAlign: "center" }}>
// 				{value.toString()}
// 			</div>
// 		);
// 	}
// 	return value;
// }
// export function price(value) {
// 	if (Number.isFinite(value)) {
// 		return (
// 			<div style={{ color: "black", textAlign: "right" }}>
// 				{`${Number(value).toFixed(2)} $`}
// 			</div>
// 		);
// 	}
// 	return value;
// }
// export function quantity(value) {
// 	if (Number.isFinite(value)) {
// 		return (
// 			<div style={{ color: "black", textAlign: "right" }}>
// 				{Number(value).toFixed(0)}
// 			</div>
// 		);
// 	}
// 	return value;
// }
// export function amount(value) {
// 	if (Number.isFinite(value)) {
// 		return (
// 			<div style={{ color: "black", textAlign: "right" }}>
// 				{Number(value).toFixed(0)}
// 			</div>
// 		);
// 	}
// 	return value;
// }
import React from 'react';
export const configFunctions = {
	formats: {
		id: value =>
			<div style={{ color: 'blue', textAlign: 'right' }}>
				{value}
			</div>,
		date: value => {
			if (value instanceof Date) {
				return (
					<div style={{ color: 'black', textAlign: 'center' }}>
						{value.toString()}
					</div>
				);
			}
			return value;
		},
		price: value => {
			if (Number.isFinite(value)) {
				return (
					<div style={{ color: 'black', textAlign: 'right' }}>
						{`${Number(value).toFixed(2)} $`}
					</div>
				);
			}
			return value;
		},
		quantity: value => {
			if (Number.isFinite(value)) {
				return (
					<div style={{ color: 'black', textAlign: 'right' }}>
						{Number(value).toFixed(0)}
					</div>
				);
			}
			return value;
		},
		amount: value => {
			if (Number.isFinite(value)) {
				return (
					<div style={{ color: 'black', textAlign: 'right' }}>
						{Number(value).toFixed(0)}
					</div>
				);
			}
			return value;
		}
	},
	accessors: {
		price: row => [row.amt, row.qty]
	},
	sorts: {},
	aggregations: {}
};
