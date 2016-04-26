import * as React from 'react';
import {addEventListener, removeEventListener} from '../orb.utils';

export default class DropdownComponent extends React.Component<any,any>{

	constructor(props){
		super(props);
		this.onMouseEnter = this.onMouseEnter.bind(this);
		this.onMouseLeave = this.onMouseLeave.bind(this);
		this.selectValue = this.selectValue.bind(this);
		this.openOrClose = this.openOrClose.bind(this);
	}

	openOrClose(e) {
		console.log(this);
		const valueNode = this.refs['valueElement'];
		const valuesListNode = this.refs['valuesList'];
		const target = e.target || e.srcElement;

		if(target === valueNode && valuesListNode['style'].display === 'none') {
			valuesListNode['style'].display = 'block';
		} else {
			valuesListNode['style'].display = 'none';
		}
	}
	onMouseEnter() {
		const valueNode = this.refs['valueElement'];
		valueNode['className'] = "orb-tgl-btn-down";
		valueNode['style'].backgroundPosition = 'right center';
	}
	onMouseLeave() {
		this.refs['valueElement']['className'] = "";
	}
	componentDidMount() {
		addEventListener(document, 'click', this.openOrClose);
	}
	componentWillUnmount() {
		removeEventListener(document, 'click', this.openOrClose);
	}
	selectValue(e) {
		const listNode = this.refs['valuesList'];
		let target = e.target || e.srcElement;
		let isli = false;
		while(!isli && target != null) {
			if(target.parentNode == listNode) {
				isli = true;
				break;
			}
			target = target.parentNode;
		}

		if(isli) {
			const value = target.textContent;
			const valueElement = this.refs['valueElement'];
			if(valueElement['textContent'] != value) {
				valueElement['textContent'] = value;
				if(this.props.onValueChanged) {
					this.props.onValueChanged(value);
				}
			}
		}
	}
	render() {
		function createSelectValueFunc(value) {
			return function() {
				this.selectValue(value);
			};
		}

		const values = [];
		for(let i=0; i < this.props.values.length; i++) {
			values.push(<li key={'item' + i} dangerouslySetInnerHTML={{__html: this.props.values[i]}}></li>);
		}

		return <div className="orb-select">
				<div ref="valueElement" dangerouslySetInnerHTML={{__html: this.props.selectedValue}} onMouseEnter={this.onMouseEnter} onMouseLeave={this.onMouseLeave}></div>
				<ul ref="valuesList" style={{ display: 'none' }} onClick={ this.selectValue }>
					{values}
				</ul>
			</div>;
	}
};
