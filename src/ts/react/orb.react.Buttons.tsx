import * as React from 'react';
import * as ReactDOM from 'react-dom';
import DragManager from './orb.react.DragManager';
import FilterPanel from './orb.react.FilterPanel';
import * as utils from '../orb.utils';
import {AxeType} from '../orb.axe';
import * as domUtils from '../orb.utils.dom';
let pbid = 0;

export class PivotButton extends React.Component<any,any>{
	constructor(props){
    super(props)
    this.state = {
      pbid: pbid+1,
    // initial state, all zero.
      pos: { x: 0, y: 0 },
      startpos: { x: 0, y: 0 },
      mousedown: false,
      dragging: false
    }
		this.onFilterMouseDown = this.onFilterMouseDown.bind(this);
		this.onMouseDown = this.onMouseDown.bind(this);
		this.onMouseUp = this.onMouseUp.bind(this);
		this.onMouseMove = this.onMouseMove.bind(this);
		};

	onFilterMouseDown(e){
		// left mouse button only
		if (e.button !== 0) return;

		const filterButton = this.refs['filterButton'];
		const filterButtonPos = domUtils.getOffset(filterButton);
		const filterContainer = document.createElement('div');

    filterContainer.className = this.props.pivotTableComp.pgrid.config.theme.getFilterClasses().container;
		filterContainer.style.position = 'fixed';
    filterContainer.style.top = `${filterButtonPos.y}px`;
    filterContainer.style.left = `${filterButtonPos.x}px`;
		filterContainer.style.backgroundColor= 'white';
		filterContainer.style.fontSize= '90%';
		filterContainer.style.width= '301px';
		filterContainer.style.height= '223px';
		filterContainer.style.padding= '3px';
		filterContainer.style.border= 'solid 1px';
		filterContainer.style.boxShadow= '0 5px 15px #9d9d9d';
    document.body.appendChild(filterContainer);

    ReactDOM.render(<FilterPanel field={this.props.field.name} pivotTableComp={this.props.pivotTableComp}/>, filterContainer);

		// prevent event bubbling (to prevent text selection while dragging for example)
		utils.stopPropagation(e);
		utils.preventDefault(e);
	}

	componentDidUpdate() {
		if (this.props.pivotTableComp.pgrid.config.canMoveFields) {
			if (!this.state.mousedown) {
				// mouse not down, don't care about mouse up/move events.
				DragManager.setDragElement(null);
				utils.removeEventListener(document, 'mousemove', this.onMouseMove);
			} else if (this.state.mousedown) {
				// mouse down, interested by mouse up/move events.
				DragManager.setDragElement(this);
				utils.addEventListener(document, 'mousemove', this.onMouseMove);
			}
		}
	}
	componentDidMount() {
		this.props.pivotTableComp.registerThemeChanged(this.updateClasses);
	}
	componentWillUnmount() {
		this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses);
		utils.removeEventListener(document, 'mousemove', this.onMouseMove);
	}
	onMouseDown(e) {
		// drag/sort with left mouse button
		if (e.button !== 0) return;

		if(e.ctrlKey) {
		    this.props.pivotTableComp.pgridwidgetstore.toggleFieldExpansion(this.props.axetype, this.props.field);
		} else {

		    const thispos = domUtils.getOffset(ReactDOM.findDOMNode(this));
			const mousePageXY = utils.getMousePageXY(e);

			// inform mousedown, save start pos
			this.setState({
				mousedown: true,
				mouseoffset: {
					x: thispos.x - mousePageXY.pageX,
					y: thispos.y - mousePageXY.pageY
				},
				startpos: {
					x: mousePageXY.pageX,
					y: mousePageXY.pageY
				}
			});
		}

		// prevent event bubbling (to prevent text selection while dragging for example)
		utils.stopPropagation(e);
		utils.preventDefault(e);
	}
	onMouseUp(e) {

		const isdragged = this.state.dragging;

		this.setState({
			mousedown: false,
			dragging: false,
			size: null,
			pos: {
				x: 0,
				y: 0
			}
		});

		if(!e.ctrlKey && !isdragged) {
			// if button was not dragged, proceed as a click
			this.props.pivotTableComp.sort(this.props.axetype, this.props.field);
		}
	}
	onMouseMove(e) {
		// if the mouse is not down while moving, return (no drag)
		if (!this.props.pivotTableComp.pgrid.config.canMoveFields || !this.state.mousedown) return;

		let size = null;
		const mousePageXY = utils.getMousePageXY(e);

		if(!this.state.dragging) {
		    size = domUtils.getSize(ReactDOM.findDOMNode(this));
		} else {
			size = this.state.size;
		}

		const newpos = {
			x: mousePageXY.pageX + this.state.mouseoffset.x,
			y: mousePageXY.pageY + this.state.mouseoffset.y
		};

		if(!this.state.dragging || newpos.x != this.state.pos.x || newpos.y != this.state.pos.y) {
			this.setState({
				dragging: true,
				size,
				pos: newpos
			});

			DragManager.elementMoved();
		}

		utils.stopPropagation(e);
		utils.preventDefault(e);
	}
	updateClasses() {
	    ReactDOM.findDOMNode(this).className = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton;
	}
	render() {
		const divstyle = {
			left: `${this.state.pos.x}px`,
			top: `${this.state.pos.y}px`,
			position: this.state.dragging ? 'fixed' : '',
			zIndex: 101,
      width:'',
			backgroundColor: '#5bc0de',
			borderRadius: 4
		};

		if(this.state.size) {
			divstyle.width = `${this.state.size.width}px`;
		}

		const sortDirectionClass = this.props.field.sort.order === 'asc' ?
			'sort-asc' :
			//' \u2191' :
			(this.props.field.sort.order === 'desc' ?
				'sort-desc' :
				//' \u2193' :
				'' );
		const filterClass = (this.state.dragging ? '' : 'fltr-btn') + (this.props.pivotTableComp.pgrid.isFieldFiltered(this.props.field.name) ? ' fltr-btn-active' : '');
		const filterImage = `url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAMUlEQVQYlWP4//9/I7GYgSzFDHgAVsX/sQCsirFpQFaI1c0wDegKB0AxeihQFs7EYAAT8WYwzt7jxgAAAABJRU5ErkJggg==) no-repeat 0px 0px`;

		return <div key={this.props.field.name}
		            className={this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton}
		            onMouseDown={this.onMouseDown}
		            onMouseUp={this.onMouseUp}
		            style={divstyle}>
		            <table>
		            	<tbody>
		            		<tr>
		            			<td className="caption">{this.props.field.caption}</td>
		            			<td><div className={'sort-indicator ' + sortDirectionClass}></div></td>
		            			<td className="filter">
		            				<div 	ref="filterButton"
															className={filterClass}
															onMouseDown={this.state.dragging ? null : this.onFilterMouseDown}
															style={{
																width: 11,
																height: 11,
																background: filterImage
															}}>
												</div>
		            			</td>
		            		</tr>
		            	</tbody>
		            </table>
		        </div>;
	}
}

export class DataButton extends React.Component<any,{}>{

		constructor(props){
			super(props);
			this.onClick = this.onClick.bind(this);
			this.state = {
				active: true
			}
		}
		onClick(){
			this.props.pgrid.toggleDataField(this.props.field.name);
			this.setState({active: !this.state['active']});
		}

		render(){
			const fieldAggFunc = <small>{' (' + this.props.field.aggregateFuncName + ')' }</small>;
			const activeStyle = {
				backgroundColor: '#5bc0de',
				borderRadius: 4,
				padding: 4
			};
			const inactiveStyle = {
				border: 'solid #5bc0de',
				borderRadius: 4,
				padding: 4
			};
			return <div style={this.state['active'] ? activeStyle : inactiveStyle}
									onClick={this.onClick}
			>{this.props.field.caption}{fieldAggFunc}</div>
		}
}
