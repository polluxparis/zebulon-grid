import * as ReactDOM from 'react-dom';
import * as utils from '../orb.utils';

export default (() => {

	let _pivotComp = null;

	let _currDragElement = null;
	let _currDropTarget = null;
	let _currDropIndicator = null;

	let _dragNode = null;
	const _dropTargets = [];
	const _dropIndicators = [];

	function doElementsOverlap(elem1Rect, elem2Rect) {
		return !(elem1Rect.right < elem2Rect.left ||
                elem1Rect.left > elem2Rect.right ||
                elem1Rect.bottom < elem2Rect.top ||
                elem1Rect.top > elem2Rect.bottom);
	}

	function setCurrDropTarget(dropTarget, callback?) {
		if(_currDropTarget) {
			signalDragEnd(_currDropTarget, () => {
				_currDropTarget = dropTarget;
				signalDragOver(dropTarget, callback);
			});
		} else {
			_currDropTarget = dropTarget;
			signalDragOver(dropTarget, callback);
		}
	}

	function setCurrDropIndicator(dropIndicator) {
		if(_currDropIndicator) {
			signalDragEnd(_currDropIndicator, () => {
				_currDropIndicator = dropIndicator;
				signalDragOver(dropIndicator);
			});
		} else {
			_currDropIndicator = dropIndicator;
			signalDragOver(dropIndicator);
		}
	}

	function signalDragOver(target, callback?) {
		if(target && target.onDragOver) {
			target.onDragOver(callback);
		} else if(callback) {
			callback();
		}
	}

	function signalDragEnd(target, callback) {
		if(target && target.onDragEnd) {
			target.onDragEnd(callback);
		} else if(callback) {
			callback();
		}
	}

	function getDropTarget() {
		return utils.forEach(_dropTargets, target => {
			if(target.component.state.isover) {
				return target;
			}
		});
	}

	function getDropIndicator() {
		return utils.forEach(_dropIndicators, indicator => {
			if(indicator.component.state.isover) {
				return indicator;
			}
		});
	}

	let _initialized = false;

	return {
		init(pivotComp) {
			_initialized = true;
			_pivotComp = pivotComp;
		},
		setDragElement(elem) {
			const prevDragElement = _currDragElement;
			_currDragElement = elem;
			if(_currDragElement != prevDragElement) {
				if(elem == null) {

					if(_currDropTarget) {
						const position = _currDropIndicator != null ? _currDropIndicator.position : null;
						_pivotComp.moveButton(prevDragElement, _currDropTarget.component.props.axetype, position);
					}

					_dragNode = null;
					setCurrDropTarget(null);
					setCurrDropIndicator(null);

				} else {
				    _dragNode = ReactDOM.findDOMNode(_currDragElement);
				}
			}
		},
		registerTarget(target, axetype, dragOverHandler, dargEndHandler) {
			_dropTargets.push({
				component: target,
				axetype,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterTarget(target) {
			let tindex;
			for(let i = 0; i < _dropTargets.length; i++) {
				if(_dropTargets[i].component == target) {
					tindex = i;
					break;
				}
			}
			if(tindex != null) {
				_dropTargets.splice(tindex, 1);
			}
		},
		registerIndicator(indicator, axetype, position, dragOverHandler, dargEndHandler) {
			_dropIndicators.push({
				component: indicator,
				axetype,
				position,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterIndicator(indicator) {
			let iindex;
			for(let i = 0; i < _dropIndicators.length; i++) {
				if(_dropIndicators[i].component == indicator) {
					iindex = i;
					break;
				}
			}
			if(iindex != null) {
				_dropIndicators.splice(iindex, 1);
			}
		},
		elementMoved() {
			// console.log(`elementMoved`);
			if(_currDragElement != null) {
				const dragNodeRect = _dragNode.getBoundingClientRect();
				let foundTarget;

				utils.forEach(_dropTargets, target => {
					if(!foundTarget) {
					    const tnodeRect = ReactDOM.findDOMNode(target.component).getBoundingClientRect();
						const isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
						if(isOverlap) {
							foundTarget = target;
							return;
						}
					}
				});

				if(foundTarget) {
					setCurrDropTarget(foundTarget, () => {
						let foundIndicator = null;

						utils.forEach(_dropIndicators, (indicator, index) => {
							if(!foundIndicator) {
								const elementOwnIndicator = indicator.component.props.axetype === _currDragElement.props.axetype &&
														  indicator.component.props.position === _currDragElement.props.position;

								const targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
								if(targetIndicator && !elementOwnIndicator) {
								    const tnodeRect = ReactDOM.findDOMNode(indicator.component).getBoundingClientRect();
									const isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
									if(isOverlap) {
										foundIndicator = indicator;
										return true;
									}
								}
							}
						});

						if(!foundIndicator) {
							const axeIndicators = _dropIndicators.filter(indicator => indicator.component.props.axetype === foundTarget.component.props.axetype);
							if(axeIndicators.length > 0) {
								foundIndicator = axeIndicators[axeIndicators.length - 1];
							}
						}
						setCurrDropIndicator(foundIndicator);
					});
				}
			}
		}
	};
})();
