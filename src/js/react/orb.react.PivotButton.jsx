import React from 'react';
import ReactDOM from 'react-dom';
import FilterPanel from './orb.react.FilterPanel.jsx';
import DragManager from './orb.react.DragManager.jsx';
import utils from '../orb.utils';
import axe from '../orb.axe';
import domUtils from '../orb.utils.dom';
let pbid = 0;

export default React.createClass({
	displayName: 'PivotButton',
	getInitialState() {
		this.pbid = ++pbid;

		// initial state, all zero.
		return {
			pos: { x: 0, y: 0 },
			startpos: { x: 0, y: 0 },
			mousedown: false,
			dragging: false
		};
	},
	onFilterMouseDown(e) {
		// left mouse button only
		if (e.button !== 0) return;

		const filterButton = this.refs.filterButton;
		const filterButtonPos = domUtils.getOffset(filterButton);
		const filterContainer = document.createElement('div');

        const filterPanelFactory = React.createFactory(FilterPanel);
        const filterPanel = filterPanelFactory({
            field: this.props.field.name,
            pivotTableComp: this.props.pivotTableComp
        });

        filterContainer.className = this.props.pivotTableComp.pgrid.config.theme.getFilterClasses().container;
        filterContainer.style.top = `${filterButtonPos.y}px`;
        filterContainer.style.left = `${filterButtonPos.x}px`;
        document.body.appendChild(filterContainer);

        ReactDOM.render(filterPanel, filterContainer);

		// prevent event bubbling (to prevent text selection while dragging for example)
		utils.stopPropagation(e);
		utils.preventDefault(e);
	},
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
	},
	componentDidMount() {
		this.props.pivotTableComp.registerThemeChanged(this.updateClasses);
	},
	componentWillUnmount() {
		this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses);
		utils.removeEventListener(document, 'mousemove', this.onMouseMove);
	},
	onMouseDown(e) {
		// drag/sort with left mouse button
		if (e.button !== 0) return;

		if(e.ctrlKey) {
		    this.props.pivotTableComp.pgridwidget.toggleFieldExpansion(this.props.axetype, this.props.field);
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
	},
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
	},
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
	},
	updateClasses() {
	    ReactDOM.findDOMNode(this).className = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton;
	},
	render() {
		const self = this;
		const divstyle = {
			left: `${self.state.pos.x}px`,
			top: `${self.state.pos.y}px`,
			position: self.state.dragging ? 'fixed' : '',
			zIndex: 101
		};

		if(self.state.size) {
			divstyle.width = `${self.state.size.width}px`;
		}

		const sortDirectionClass = self.props.field.sort.order === 'asc' ?
			'sort-asc' :
			//' \u2191' :
			(self.props.field.sort.order === 'desc' ?
				'sort-desc' :
				//' \u2193' :
				'' );
		const filterClass = (self.state.dragging ? '' : 'fltr-btn') + (this.props.pivotTableComp.pgrid.isFieldFiltered(this.props.field.name) ? ' fltr-btn-active' : '');
		var fieldAggFunc = '';
		if(self.props.axetype === axe.Type.DATA) {
			fieldAggFunc = <small>{' (' + self.props.field.aggregateFuncName + ')' }</small>;
		};

		return <div key={self.props.field.name}
		            className={this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton}
		            onMouseDown={this.onMouseDown}
		            onMouseUp={this.onMouseUp}
		            style={divstyle}>
		            <table>
		            	<tbody>
		            		<tr>
		            			<td className="caption">{self.props.field.caption}{fieldAggFunc}</td>
		            			<td><div className={'sort-indicator ' + sortDirectionClass}></div></td>
		            			<td className="filter">
		            				<div ref="filterButton" className={filterClass} onMouseDown={self.state.dragging ? null : this.onFilterMouseDown}></div>
		            			</td>
		            		</tr>
		            	</tbody>
		            </table>
		        </div>;
	}
});