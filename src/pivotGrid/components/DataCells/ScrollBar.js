import React, { Component } from "react";
class ScrollBarInner extends Component {
  constructor(props) {
    super(props);
  }
  // collect = e => {
  //   console.log(e);
  //   e.preventDefault();
  // };

  // handleMouseDown = e => this.props.handleMouseDown(this.collect(e));
  // handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  // handleMouseMove = e => this.props.handleMouseUp(this.collect(e));
  render() {
    return (
      <div
        id={"thumb-" + this.props.id}
        style={this.props.style}
        // onMouseDown={this.handleMouseDown}
        // onMouseUp={this.props.handleMouseUp}
        // onMouseMove={this.props.handleMouseMove}
      />
    );
  }
}

export class ScrollBar extends Component {
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
  computeScrollbar = props => {
    const {
      direction,
      length,
      width,
      offset,
      positionRatio,
      displayedRatio
    } = props;

    const style = {
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
    this.position = Math.min(length - this.innerSize, length * positionRatio);
    if (direction === "horizontal") {
      this.style = {
        ...style,
        height: width,
        width: length,
        top: offset
      };
      innerStyle = {
        ...innerStyle,
        height: width - 1,
        width: this.innerSize,
        left: this.position
      };
    } else {
      this.style = {
        ...style,
        height: length,
        width,
        left: offset
      };
      innerStyle = {
        ...innerStyle,
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
    const { left, top } = target.getBoundingClientRect();
    let x = clientX - left,
      y = clientY - top,
      position = this.props.direction === "horizontal" ? x : y;
    position = position - this.innerSize / 2 * (position / this.props.length);
    const event = {
      type: "scrollbar",
      direction: this.props.direction,
      button,
      shiftKey,
      x,
      y,
      positionRatio: Math.max(
        0,
        Math.min(this.props.length - this.innerSize, position) /
          this.props.length
      ),
      position,
      initiator
    };
    this.position = position;
    return event;
  };
  handleMouseDown = e => {
    const event = this.collect(e);
    if (event.initiator === "thumb") {
      this.isDragging = true;
      console.log("start dragging ", event);
    } else {
      return this.props.onScroll(event);
    }
  };
  handleMouseUp = e => (this.isDragging = false);
  // handleMouseMove = e => {
  //   if (this.isDragging) {
  //     e.preventDefault();
  //     const event = this.collect(e);
  //     if (event.initiator === "bar") {
  //       console.log("mouse over", event);
  //       const innerStyle = { ...this.state.innerStyle };
  //       if (event.direction === "horizontal") {
  //         innerStyle.left = event.position;
  //       } else {
  //         innerStyle.top = event.position;
  //       }
  //       this.setState({ innerStyle });
  //       return this.props.handleMouseDown(event);
  //     }
  //   }
  // };
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
    // let style = {
    //   // position: "relative",
    //   backgroundColor: "lightgrey",
    //   border: "solid 0.03em grey",
    //   borderRadius: " 0.25rem"
    // };
    // let innerStyle = {
    //   ...style,
    //   position: "relative",
    //   backgroundColor: "grey"
    // };
    // this.innerSize = Math.max(30, length * displayedRatio);
    // this.position = Math.min(length - this.innerSize, length * positionRatio);
    // if (direction === "horizontal") {
    //   style = {
    //     ...style,
    //     height: width,
    //     width: length,
    //     top: offset
    //   };
    //   innerStyle = {
    //     ...innerStyle,
    //     height: width - 1,
    //     // marginTop: 0.5,
    //     width: this.innerSize,
    //     left: this.position
    //   };
    // } else {
    //   // const height = height - scrollbarSize;
    //   style = {
    //     ...style,
    //     height: length,
    //     width,
    //     left: offset
    //   };
    //   innerStyle = {
    //     ...innerStyle,
    //     height: this.innerSize,
    //     width: width - 1,
    //     // marginLeft: 0.5,
    //     top: this.position
    //   };
    // }
    // this.setState({ innerStyle });
    return (
      <div
        id={id}
        style={this.style}
        onMouseDown={this.handleMouseDown}
        onMouseUp={this.handleMouseUp}
        onMouseMove={this.handleMouseMove}
      >
        <ScrollBarInner id={"thumb-" + id} style={this.state.innerStyle} />
      </div>
    );
  }
}
