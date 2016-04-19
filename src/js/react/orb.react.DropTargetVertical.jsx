import React from 'react';
import DragManager from './orb.react.DragManager.jsx';
import DropIndicator from './orb.react.DropIndicator.jsx';
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
			var currButton = [
					<tr><td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype} isVertical={true}></DropIndicator></td></tr>,
					<tr><td>{ button }</td></tr>
				];

			if(index == self.props.buttons.length - 1) {
				currButton.push(
					<tr><td><DropIndicator isLast={true} position={null} axetype={self.props.axetype} isVertical={true}></DropIndicator></td></tr>
				);
			}

			return currButton;
		});

		return <div className={'drp-trgt-vertical' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-vertical-empty' : '')}>
			<table>
			<tbody>
				{buttons}
			</tbody>
			</table>
		</div>;
	}
});