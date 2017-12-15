import React, { Component } from "react";

export default class Chart extends Component {
  constructor(props) {
    super(props);
  }
  componentDidMount() {
    this.props.getRef(this);
  }
  render() {
    return <div>Chart...</div>;
  }
}
