import React, { Component } from 'react';

export default class DataButton extends Component {

  constructor(props) {
    super(props);
    this.onClick = this.onClick.bind(this);
  }

  onClick() {
    const { field } = this.props;
    this.props.handleClick(field.id);
  }

  render() {
    const { active, field } = this.props;
    const fieldAggFunc = <small>{` (${field.aggregateFuncName}) `}</small>;
    const inactiveStyle = {
      backgroundColor: '#cccccc',
      border: 'solid #cccccc 1px',
      borderRadius: 4,
      padding: 4,
      cursor: 'default',
      outline: 'none',
      fontSize: 'medium',
    };
    const activeStyle = {
      backgroundColor: 'white',
      border: 'solid #cccccc 1px',
      borderRadius: 4,
      padding: 4,
      cursor: 'default',
      outline: 'none',
      fontSize: 'medium',
    };
    return (
      <button
        className="Orb-datafield-button"
        style={active ? activeStyle : inactiveStyle}
        onClick={this.onClick}
      >
        {field.caption}
        {fieldAggFunc}
      </button>
    );
  }
}
