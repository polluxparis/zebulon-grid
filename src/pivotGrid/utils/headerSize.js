function getHeaderSize(sizeAndPositionManager, index, span) {
	let res = 0;
	for (let i = 0; i < span; i += 1) {
		res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size;
	}
	return res;
}

export default getHeaderSize;
