import * as React from 'react';
import DragManager from './orb.react.DragManager';
import DropIndicator from './orb.react.DropIndicator';
import {AxeType} from '../orb.axe';

let dtid = 0;

export default class DropTarget extends React.Component<any,any>{
    _isMounted: boolean;
    dtid: number;
	constructor(props) {
		super(props);
		this.dtid = ++dtid;
		this.state = {
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
			if(index < this.props.buttons.length - 1) {
				return [
					<td><DropIndicator isFirst={index === 0} position={index} axetype={this.props.axetype}></DropIndicator></td>,
					<td>{ button }</td>
				];
			} else {
				return [
					<td><DropIndicator isFirst={index === 0} position={index} axetype={this.props.axetype}></DropIndicator></td>,
					<td>{ button }</td>,
					<td><DropIndicator isLast={true} position={null} axetype={this.props.axetype}></DropIndicator></td>
				];
			}
		});

		const style = this.props.axetype === AxeType.ROWS ? { position: 'absolute', left: 0, bottom: 11 } : null;

		return <div className={'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-empty' : '')} style={style}>
			<table>
			<tbody>
				<tr>
					{buttons}
				</tr>
			</tbody>
			</table>
		</div>;
	}
}
