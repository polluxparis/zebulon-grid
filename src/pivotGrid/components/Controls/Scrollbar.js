import React, { Component } from "react";

class ScrollbarInner extends Component {
  render() {
    // if (this.props.direction === "horizontal") {
    //   console.log("scrollbar", this.props.style);
    // }
    return (
      <div
        className="zebulon-scrollbar-thumb"
        id={"thumb-" + this.props.id}
        style={this.props.style}
      />
    );
  }
}

export class Scrollbar extends Component {
  constructor(props) {
    super(props);
    this.state = { innerStyle: this.computeScrollbar(props) };
  }
  componentWillReceiveProps(nextProps) {
    const innerStyle = this.computeScrollbar(nextProps);
    // if (!this.isDragging) {
    this.setState({ innerStyle });
    // }
  }
  componentDidMount() {
    const element = document.getElementById(this.props.id);
    if (element) {
      this.scrollbarPositions = element.getBoundingClientRect();
    }
  }
  computeScrollbar = props => {
    const {
      direction,
      length,
      width,
      positionRatio,
      displayRatio,
      left,
      top
    } = props;

    let innerStyle;
    this.innerSize = Math.max(30, length * displayRatio);
    this.position = Math.min(length - this.innerSize, length * positionRatio);
    this.positionRatio = positionRatio;
    if (direction === "horizontal") {
      this.style = {
        // position: "absolute",
        height: width,
        width: length
        // left,
        // top
      };
      innerStyle = {
        position: "relative",
        height: width - 1,
        width: this.innerSize,
        left: this.position
      };
    } else {
      this.style = {
        height: length,
        width
      };
      innerStyle = {
        position: "relative",
        height: this.innerSize,
        width: width - 1,
        top: this.position
      };
    }
    return innerStyle;
  };

  _handleMouseDown = e => {
    const event = {
      type: "scrollbar",
      direction: this.props.direction,
      initiator: e.target.id.startsWith("thumb") ? "thumb" : "bar",
      position: this.props.direction === "horizontal" ? e.clientX : e.clientY
    };
    e.preventDefault();
    if (event.initiator === "thumb") {
      this.isDragging = true;
      this.props._handleDrag("startDrag", event);
    } else if (!this.isDragging) {
      const { left, top } = e.target.getBoundingClientRect();
      event.relativePosition =
        event.position - (this.props.direction === "horizontal" ? left : top);
      // console.log("click", left, e.clientX, event);
      return this.props._handleDrag("click", event);
    }
  };
  _handleMouseUp = e => {
    this.isDragging = false;
    this.props._handleDrag("endDrag", { direction: this.props.direction });
  };
  _handleMouseMove = e => {
    if (this.isDragging && e.buttons) {
      const event = {
        type: "scrollbar",
        direction: this.props.direction,
        position: this.props.direction === "horizontal" ? e.clientX : e.clientY
      };
      e.preventDefault();
      this.props._handleDrag("drag", event);
    } else if (this.isDragging && !e.buttons) {
      this.isDragging = false;
      this.props._handleDrag("endDrag", { direction: this.props.direction });
    }
  };

  render() {
    const { width, id } = this.props;
    if (!width) {
      return null;
    }
    // if (this.props.direction === "horizontal") {
    //   console.log("scrollbar", this.props.positionRatio);
    // }
    return (
      <div
        className="zebulon-scrollbar-bar"
        id={id}
        style={this.style}
        onMouseDown={this._handleMouseDown}
        onMouseUp={this._handleMouseUp}
        onMouseMove={this._handleMouseMove}
      >
        <ScrollbarInner
          id={"thumb-" + id}
          style={this.state.innerStyle}
          direction={this.props.direction}
          length={this.props.length}
          positionRatio={this.props.positionRatio}
          onScroll={this.props.onScroll}
        />
      </div>
    );
  }
}
