import React, { Component } from "react";
class ScrollBarInner extends Component {
  constructor(props) {
    super(props);
  }
  // componentDidMount() {
  //   this.document = document.getElementById("thumb-" + this.props.id);
  //   this.document.addEventListener("mousedown", this.handleMouseDown);
  // }

  // componentDidUnMount() {
  //   this.document.removeEventListener("mousedown", this.handleMouseDown);
  // }
  // handleMouseDown = e => {
  //   console.log("thumb handleMouseDown", e);
  //   e.preventDefault();
  // };
  render() {
    return <div id={"thumb-" + this.props.id} style={this.props.style} />;
  }
}

export class ScrollBar extends Component {
  constructor(props) {
    super(props);
  }
  // componentDidMount() {
  //   this.document = document.getElementById(this.props.id);
  //   this.document.addEventListener("mouseenter", this.handleMouseEnter);
  //   this.document.addEventListener("mouseleave", this.handleMouseLeave);
  //   this.document.addEventListener("mousedown", this.handleMouseDown);
  // }

  // componentDidUnMount() {
  //   this.document.removeEventListener("mouseenter", this.handleMouseEnter);
  //   this.document.removeEventListener("mouseleave", this.handleMouseLeave);
  //   this.document.removeEventListener("mousedown", this.handleMouseDown);
  // }
  collect = e => {
    const { button, shiftKey, target, clientX, clientY } = e;
    const { left, top } = target.getBoundingClientRect();
    const x = clientX - left,
      y = clientY - top,
      position = this.props.direction === "horizontal" ? x : y;
    return {
      type: "scrollbar",
      direction: this.props.direction,
      button,
      shiftKey,
      x,
      y,
      positionRatio: position / this.props.length,
      initiator:
        position < this.position || position > this.position + this.innerSize
          ? "bar"
          : "thumb"
    };
  };
  handleMouseDown = e => this.props.handleMouseDown(this.collect(e));
  handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  render() {
    const {
      direction,
      visible,
      length,
      width,
      offset,
      positionRatio,
      displayedRatio,
      id
    } = this.props;
    if (!visible) {
      return null;
    }
    let style = {
      position: "absolute",
      backgroundColor: "lightgrey",
      border: "solid 0.03em grey",
      borderRadius: " 0.25rem"
    };
    let innerStyle = {
      ...style,
      position: "relative",
      backgroundColor: "grey"
    };
    this.innerSize = Math.max(30, length * displayedRatio);
    this.position = (length - this.innerSize) * positionRatio;
    if (direction === "horizontal") {
      style = {
        ...style,
        height: width,
        width: length,
        top: offset
      };
      innerStyle = {
        ...innerStyle,
        height: width - 2,
        width: this.innerSize,
        left: this.position
      };
    } else {
      // const height = height - scrollbarSize;
      style = {
        ...style,
        height: length,
        width,
        left: offset
      };
      innerStyle = {
        ...innerStyle,
        height: this.innerSize,
        width: width - 2,
        top: this.position
      };
    }
    return (
      <div
        id={id}
        style={style}
        onMouseDown={this.handleMouseDown}
        onMouseUp={this.props.handleMouseUp}
      >
        <div id={"thumb-" + id} style={innerStyle} />
      </div>
    );
  }
}
