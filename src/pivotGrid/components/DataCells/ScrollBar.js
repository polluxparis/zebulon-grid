import React, { Component } from "react";
class ScrollBarInner extends Component {
  constructor(props) {
    super(props);
  }
  collect = e => {
    console.log(e);
    e.preventDefault();
  };

  handleMouseDown = e => this.props.handleMouseDown(this.collect(e));
  handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  handleMouseMove = e => this.props.handleMouseUp(this.collect(e));
  render() {
    return (
      <div
        id={"thumb-" + this.props.id}
        style={this.props.style}
        onMouseDown={this.handleMouseDown}
        onMouseUp={this.props.handleMouseUp}
        onMouseMove={this.props.handleMouseMove}
      />
    );
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
    const initiator = e.target.id.startsWith("thumb") ? "thumb" : "bar";
    const { left, top } = target.getBoundingClientRect();
    const x = clientX - left,
      y = clientY - top,
      position = this.props.direction === "horizontal" ? x : y;
    const event = {
      type: "scrollbar",
      direction: this.props.direction,
      button,
      shiftKey,
      x,
      y,
      positionRatio: Math.max(0, Math.min(1, position / this.props.length)),
      initiator
    };
    this.position = position;
    return event;
  };
  handleMouseDown = e => {
    const event = this.collect(e);
    if (event.initiator === "thumb") {
      this.isDragging = true;
    } else {
      return this.props.handleMouseDown(event);
    }
  };
  handleMouseUp = e => (this.isDragging = false);
  handleMouseOver = e => {
    if (this.isDragging) {
      console.log("mouse over", e);
    }
  };
  handleDragStart(event) {
    // this.dragging = true;
    // event.stopImmediatePropagation();
    // this.setupDragging();
  }
  handleDrag(event) {
    // if (this.prevPageX) {
    //     const { clientX } = event;
    //     const { left: trackLeft } = this.trackHorizontal.getBoundingClientRect();
    //     const thumbWidth = this.getThumbHorizontalWidth();
    //     const clickPosition = thumbWidth - this.prevPageX;
    //     const offset = -trackLeft + clientX - clickPosition;
    //     this.view.scrollLeft = this.getScrollLeftForOffset(offset);
    // }
    // if (this.prevPageY) {
    //     const { clientY } = event;
    //     const { top: trackTop } = this.trackVertical.getBoundingClientRect();
    //     const thumbHeight = this.getThumbVerticalHeight();
    //     const clickPosition = thumbHeight - this.prevPageY;
    //     const offset = -trackTop + clientY - clickPosition;
    //     this.view.scrollTop = this.getScrollTopForOffset(offset);
    // }
    // return false;
  }

  handleDragEnd() {
    // this.dragging = false;
    // this.prevPageX = this.prevPageY = 0;
    // this.teardownDragging();
    // this.handleDragEndAutoHide();
  }

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
        onMouseUp={this.handleMouseUp}
        onMouseOver={this.handleMouseOver}
      >
        <div id={"thumb-" + id} style={innerStyle} />
      </div>
    );
  }
}
