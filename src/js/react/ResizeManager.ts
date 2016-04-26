import * as utils from '../orb.utils';

export class ResizeManager {
	public minContainerWidth = 301;
	public minContainerHeight = 223;

	public mousedownpos = {
		x: 0, y: 0
	};
	public isMouseDown = false;

	public outerContainerElem;
	public  valuesTableElem;
	public resizeGripElem;


	constructor(outerContainerElem, valuesTableElem, resizeGripElem){
		this.outerContainerElem = outerContainerElem;
		this.valuesTableElem = valuesTableElem;
		this.resizeGripElem = resizeGripElem;

		utils.addEventListener(this.resizeGripElem, 'mousedown', this.resizeMouseDown.bind(this));
		utils.addEventListener(document, 'mouseup', this.resizeMouseUp.bind(this));
		utils.addEventListener(document, 'mousemove', this.resizeMouseMove.bind(this));
	}


	resizeMouseDown(e) {
		console.log('resizeMouseDown');
		// drag/sort with left mouse button
		if (utils.getEventButton(e) !== 0) return;

		const mousePageXY = utils.getMousePageXY(e);

		this.isMouseDown = true;
		document.body.style.cursor = 'se-resize';

		this.mousedownpos.x = mousePageXY.pageX;
		this.mousedownpos.y = mousePageXY.pageY;

		// prevent event bubbling (to prevent text selection while dragging for example)
		utils.stopPropagation(e);
		utils.preventDefault(e);
	};

	resizeMouseUp() {
		console.log('resizeMouseUp');
		this.isMouseDown = false;
		document.body.style.cursor = 'auto';
		return true;
	};

	resizeMouseMove(e) {
		// if the mouse is not down while moving, return (no drag)
		if (!this.isMouseDown) return;
		console.log('resizeMouseMove');
		console.log(this);
		const mousePageXY = utils.getMousePageXY(e);

		const resizeGripSize = this.resizeGripElem.getBoundingClientRect();
		const outerContainerSize = this.outerContainerElem.getBoundingClientRect();
		const valuesTableSize = this.valuesTableElem.tBodies[0].getBoundingClientRect();

		const outerContainerWidth = outerContainerSize.right - outerContainerSize.left;
		const outerContainerHeight = outerContainerSize.bottom - outerContainerSize.top;

		const offset = {
			x: outerContainerWidth <= this.minContainerWidth && mousePageXY.pageX < resizeGripSize.left ? 0 : mousePageXY.pageX - this.mousedownpos.x,
			y: outerContainerHeight <= this.minContainerHeight && mousePageXY.pageY < resizeGripSize.top ? 0 : mousePageXY.pageY - this.mousedownpos.y
		};
		console.log(offset);

		const newContainerWidth = outerContainerWidth  + offset.x;
		const newContainerHeight = outerContainerHeight  + offset.y;
		console.log(newContainerHeight);

		this.mousedownpos.x = mousePageXY.pageX;
		this.mousedownpos.y = mousePageXY.pageY;

		if(newContainerWidth >= this.minContainerWidth) {
			this.outerContainerElem.style.width = `${newContainerWidth}px`;
		}

		if(newContainerHeight >= this.minContainerHeight) {
			this.outerContainerElem.style.height = `${newContainerHeight}px`;
			this.valuesTableElem.tBodies[0].style.height = `${valuesTableSize.bottom - valuesTableSize.top}${offset.y}px`;
		}

		utils.stopPropagation(e);
		utils.preventDefault(e);
	};


}
