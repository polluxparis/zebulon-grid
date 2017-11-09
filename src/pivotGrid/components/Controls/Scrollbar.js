import React, { Component } from "react";

class ScrollbarInner extends Component {
  render() {
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
    const { direction, length, width, positionRatio, displayRatio } = props;

    let innerStyle;
    this.innerSize = Math.max(30, length * displayRatio);
    this.position = Math.min(length - this.innerSize, length * positionRatio);
    this.positionRatio = positionRatio;
    if (direction === "horizontal") {
      this.style = {
        height: width,
        width: length
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

  handleMouseDown = e => {
    const event = {
      type: "scrollbar",
      direction: this.props.direction,
      initiator: e.target.id.startsWith("thumb") ? "thumb" : "bar",
      position: this.props.direction === "horizontal" ? e.clientX : e.clientY
    };
    if (event.initiator === "thumb") {
      this.isDragging = true;
      this.props.handleDrag("startDrag", event);
    } else if (!this.isDragging) {
      const { left, top } = e.target.getBoundingClientRect();
      event.relativePosition =
        event.position - (this.props.direction === "horizontal" ? left : top);
      return this.props.handleDrag("click", event);
    }
  };
  handleMouseUp = e => {
    this.isDragging = false;
    this.props.handleDrag("endDrag", { direction: this.props.direction });
  };
  handleMouseMove = e => {
    if (this.isDragging && e.buttons) {
      const event = {
        type: "scrollbar",
        direction: this.props.direction,
        position: this.props.direction === "horizontal" ? e.clientX : e.clientY
      };
      e.preventDefault();
      this.props.handleDrag("drag", event);
    } else if (this.isDragging && !e.buttons) {
      this.isDragging = false;
      this.props.handleDrag("endDrag", { direction: this.props.direction });
    }
  };

  render() {
    const { width, id } = this.props;
    if (!width) {
      return null;
    }
    return (
      <div
        className="zebulon-scrollbar-bar"
        id={id}
        style={this.style}
        onMouseDown={this.handleMouseDown}
        onMouseUp={this.handleMouseUp}
        onMouseMove={this.handleMouseMove}
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
