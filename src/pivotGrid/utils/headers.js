import {
	KEY_SEPARATOR,
	AXIS_SEPARATOR,
	ROOT_ID,
	TOTAL_ID,
	HeaderType
} from '../constants';
import { isNullOrUndefined } from './generic';

export function getHeaderSize(sizeAndPositionManager, index, span) {
	let res = 0;
	for (let i = 0; i < span; i += 1) {
		res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size;
	}
	return res;
}

const findHeader = (headers, keys) => {
	if (keys.length === 1) {
		return headers.find(header => header.key === keys[0]);
	}
	const parentHeader = headers.find(header => header.key === keys[0]);
	if (!parentHeader) throw new Error('header not found');
	return findHeader(parentHeader.children, [
		[keys[0], keys[1]].join(KEY_SEPARATOR),
		...keys.slice(2)
	]);
};

export const keyToIndex = (headers, key) => {
	const keys = key.split(KEY_SEPARATOR);
	const depth = headers[0].length;
	const ancestorHeaders = headers
		.filter(headersRow => headersRow.length === depth)
		.map(headersRow => headersRow[0]);
	try {
		return findHeader(ancestorHeaders, keys).x;
	} catch (e) {
		// console.error(`Header with key ${key} not found in following headers`, headers);
		return -1;
	}
};

/* eslint-enable */
export function getNotCollapsedLeaf(header) {
	if (
		(header.isCollapsed || header.hasCollapsedParent) &&
		header.key !== ROOT_ID
	) {
		return header.parent;
	} else {
		return header;
	}
}

export function getLeaves(header) {
	if (
		isNullOrUndefined(header.orderedChildrenIds) ||
		header.orderedChildrenIds.length === 0
	) {
		return [header];
	}
	return [].concat(
		...header.orderedChildrenIds.map(headerId =>
			getLeaves(header.children[headerId])
		)
	);
}

export function getKey({
	headerType,
	parent,
	measureId,
	crossAxisDimensionsCode,
	value,
	dimension
}) {
	switch (headerType) {
		case HeaderType.DATA_HEADER:
			if (parent.type === HeaderType.GRAND_TOTAL) {
				// If the parent is a Total header, split to include the measure id
				// on the right side of the axis separator
				const [totalID, crossAxisDimensionsCode] = parent.key.split(
					AXIS_SEPARATOR
				);
				return `${totalID}${KEY_SEPARATOR}${measureId}${AXIS_SEPARATOR}${crossAxisDimensionsCode}`;
			}
			return `${parent.key}${KEY_SEPARATOR}${measureId}`;
		case HeaderType.GRAND_TOTAL:
			return `${TOTAL_ID}${AXIS_SEPARATOR}${crossAxisDimensionsCode.join(
				KEY_SEPARATOR
			)}`;
		case HeaderType.SUB_TOTAL:
			return parent ? `${parent.key}${KEY_SEPARATOR}${value}` : value;
		case HeaderType.INNER:
		case HeaderType.WRAPPER:
			return parent
				? `${parent.key}${KEY_SEPARATOR}${dimension.id}`
				: String(dimension.id);
		default:
			throw new Error(`Header type ${headerType} is unknown`);
	}
}

export function getNextKey(current, next) {
	const firstLeafHeader =
		current.firstHeaderRow[current.firstHeaderRow.length - 1];
	const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
	let nextKey = '';
	if (current.dimensions.length > next.dimensions.length) {
		const nextDimensionIds = next.dimensions.map(dimension => dimension.id);
		const missingDimensionPosition = current.dimensions.findIndex(
			dimension => !nextDimensionIds.includes(dimension.id)
		);
		nextKey = keys.slice(0, missingDimensionPosition).join(KEY_SEPARATOR);
	} else if (current.dimensions.length < next.dimensions.length) {
		const previousDimensionIds = current.dimensions.map(
			dimension => dimension.id
		);
		const newDimensionPosition = next.dimensions.findIndex(
			dimension => !previousDimensionIds.includes(dimension.id)
		);
		nextKey = keys.slice(0, newDimensionPosition).join(KEY_SEPARATOR);
	} else if (current.dataDimensionsCount !== next.dataDimensionsCount) {
		// A data dimension has been toggled
		nextKey = keys.slice(0, -1).join(KEY_SEPARATOR);
	} else {
		// A filter has been modified
		// For the moment, do nothing
		nextKey = '';
	}
	return nextKey;
}
