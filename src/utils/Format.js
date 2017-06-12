import React from "react";
export function id(value) {
	return (
		<div style={{ color: "blue", textAlign: "right" }}>
			{value}
		</div>
	);
}

export function date(value) {
	if (value instanceof Date) {
		return (
			<div style={{ color: "black", textAlign: "center" }}>
				{value.toString()}
			</div>
		);
	}
	return value;
}
export function price(value) {
	if (Number.isFinite(value)) {
		return (
			<div style={{ color: "black", textAlign: "right" }}>
				{`${Number(value).toFixed(2)} $`}
			</div>
		);
	}
	return value;
}
export function quantity(value) {
	if (Number.isFinite(value)) {
		return (
			<div style={{ color: "black", textAlign: "right" }}>
				{Number(value).toFixed(0)}
			</div>
		);
	}
	return value;
}
