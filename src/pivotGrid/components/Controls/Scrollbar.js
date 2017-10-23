import React, { Component } from "react";
class ScrollbarInner extends Component {
  // handleMouseDown = e => this.props.handleMouseDown(this.collect(e));
  // handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  // handleMouseMove = e => this.props.handleMouseUp(this.collect(e));
  render() {
    return (
      <div
        className="zebulon-scrollbar-thumb"
        id={"thumb-" + this.props.id}
        style={this.props.style}
        // onMouseDown={this.handleMouseDown}
        // onMouseUp={this.props.handleMouseUp}
        // onMouseMove={this.props.handleMouseMove}
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
    if (!this.isDragging) {
      this.setState({ innerStyle });
    }
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
  collect = e => {
    const { button, shiftKey, target, clientX, clientY } = e;
    const initiator = e.target.id.startsWith("thumb") ? "thumb" : "bar";
    const rect = target.getBoundingClientRect();
    // console.log("collect", e.clientX, e.clientY, rect, e.target);
    const { left, top } = rect;
    let x = clientX - left,
      y = clientY - top,
      position = Math.max(
        0,
        Math.min(
          (this.props.direction === "horizontal" ? x : y) - this.innerSize / 2,
          this.props.length - this.innerSize
        )
      );

    const event = {
      type: "scrollbar",
      direction: this.props.direction,
      button,
      shiftKey,
      clientX,
      clientY,
      x,
      y,
      positionRatio: position / this.props.length,
      position,
      initiator
    };
    this.position = position;
    return event;
  };
  handleMouseDown = e => {
    const event = this.collect(e);
    if (event.initiator === "thumb") {
      // const { clientX, clientY } = e;
      // this.isDragging = true;
      // console.log("start dragging ", this.startDraggingPosition, clientY);
    } else {
      return this.props.onScroll(event);
    }
  };
  handleMouseUp = e => (this.isDragging = false);
  handleMouseMove = e => {
    // if (this.isDragging) {
    //   e.preventDefault();
    //   const event = this.collect(e);
    //   if (event.initiator === "bar") {
    //     // this.setState({ innerStyle });
    //     // return this.props.handleMouseDown(event);
    //   } else {
    //     const innerStyle = { ...this.state.innerStyle };
    //     if (event.direction === "horizontal") {
    //       innerStyle.left = event.position;
    //     } else {
    //       innerStyle.top = Math.round(
    //         event.position - this.startDraggingPosition
    //       );
    //     }
    //     console.log("mouse over", innerStyle.top);
    //     this.position = innerStyle.top;
    //     this.setState({ innerStyle });
    //   }
    // }
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
        <ScrollbarInner id={"thumb-" + id} style={this.state.innerStyle} />
      </div>
    );
  }
}
