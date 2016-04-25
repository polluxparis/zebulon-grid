import * as React from 'react';
import DragManager from './orb.react.DragManager';
import DropIndicator from './orb.react.DropIndicator';
let dtid = 0;

export default class DropTargetVertical extends React.Component<any,any>{
	dtid: number;
	_isMounted: boolean;
	getInitialState() {
		this.dtid = ++dtid;
		return {
			isover: false
		};
	}
  	componentDidMount() {
			this._isMounted = true;
  		DragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
  	}
	componentWillUnmount() {
		this._isMounted = false;
		DragManager.unregisterTarget(this);
	}
	onDragOver(callback) {
		if(this._isMounted) {
			this.setState({
				isover: true
			}, callback);
		} else if(callback) {
			callback();
		}
	}
	onDragEnd(callback) {
		if(this._isMounted) {
			this.setState({
				isover: false
			}, callback);
		} else if(callback) {
			callback();
		}
	}
	render() {

		const buttons = this.props.buttons.map((button, index) => {
			var currButton = [
					<tr><td><DropIndicator isFirst={index === 0} position={index} axetype={this.props.axetype} isVertical={true}></DropIndicator></td></tr>,
					<tr><td>{ button }</td></tr>
				];

			if(index == this.props.buttons.length - 1) {
				currButton.push(
					<tr><td><DropIndicator isLast={true} position={null} axetype={this.props.axetype} isVertical={true}></DropIndicator></td></tr>
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
};
