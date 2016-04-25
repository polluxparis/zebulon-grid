import * as React from 'react';
import DragManager from './orb.react.DragManager';

export default class DropIndicatorComponent extends React.Component<any,any>{
	displayName = 'DropIndicator';
	_isMounted: boolean;

	constructor(props) {
		super(props);
		DragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
		this.state = {
			isover: false
		};
	}
	componentDidMount(){
		this._isMounted = true;
	}
	componentWillUnmount() {
		this._isMounted = false;
		DragManager.unregisterIndicator(this);
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
		let classname = `drp-indic${this.props.isVertical ? '-vertical' : ''}`;

		if(this.props.isFirst) {
			classname += ' drp-indic-first';
		}

		if(this.props.isLast) {
			classname += ' drp-indic-last';
		}

		const style = {};
		if(this.state.isover) {
			classname += ' drp-indic-over';
		}

		return <div style={style} className={classname}></div>;
	}
};
