import React from 'react';
import DragManager from './orb.react.DragManager.jsx';
import DropIndicator from './orb.react.DropIndicator.jsx';
import axe from '../orb.axe';

let dtid = 0;

export default React.createClass({
	getInitialState() {
		this.dtid = ++dtid;
		return {
			isover: false
		};
	},
  	componentDidMount() {
  		DragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
  	},
	componentWillUnmount() {
		DragManager.unregisterTarget(this);
	},
	onDragOver(callback) {
		if(this.isMounted()) {
			this.setState({
				isover: true
			}, callback);
		} else if(callback) {
			callback();
		}
	},
	onDragEnd(callback) {
		if(this.isMounted()) {
			this.setState({
				isover: false
			}, callback);
		} else if(callback) {
			callback();
		}
	},
	render() {
		const self = this;

		const buttons = this.props.buttons.map((button, index) => {
			if(index < self.props.buttons.length - 1) {
				// return [
				// 	<td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator></td>,
				// 	<td>{ button }</td>
				// ];
			} else {
				// return [
				// 	<td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator></td>,
				// 	<td>{ button }</td>,
				// 	<td><DropIndicator isLast={true} position={null} axetype={self.props.axetype}></DropIndicator></td>
				// ];
			}
		});

		const style = self.props.axetype === axe.Type.ROWS ? { position: 'absolute', left: 0, bottom: 11 } : null;

		// return <div className={'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-empty' : '')} style={style}>
		// 	<table>
		// 	<tbody>
		// 		<tr>
		// 			{buttons}
		// 		</tr>
		// 	</tbody>
		// 	</table>
		// </div>;
	}
});