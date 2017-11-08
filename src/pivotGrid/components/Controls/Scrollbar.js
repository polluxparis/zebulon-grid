import React, { Component } from "react";
import { DragSource, DropTarget } from "react-dnd";

const thumbSpec = {
  beginDrag(props) {
    return props;
  }
};

// const collectDragSource = (connect, monitor) => ({
//   connectDragSource: connect.dragSource(),
//   connectDragPreview: connect.dragPreview(),
//   isDragging: monitor.isDragging()
// });

class ScrollbarInner extends Component {
  handleMouseDown = e => {
    const event = this.collect(e);
    this.isDown = true;
    this.previous = this.props.direction === "horizontal" ? event.x : event.y;
    // console.log("down", event);
  }; //this.props.handleMouseDown(this.collect(e));
  handleMouseUp = e => {
    this.isDown = false;
    // console.log("up", this.collect(e));
  };
  // handleMouseMove = e => {
  //   if (this.isDown) {
  //     const event = this.collect(e);
  //     const x = this.props.direction === "horizontal" ? event.x : event.y;
  //     const delta = x - this.previous;
  //     if (delta) {
  //       event.positionRatio =
  //         this.props.positionRatio + delta / this.props.length;
  //       if (this.props.onScroll(event)) {
  //         this.previous = x;
  //         console.log(
  //           "move",
  //           delta,
  //           this.previous,
  //           x,
  //           event.positionRatio,
  //           event
  //         );
  //       }
  //     }
  //   }
  // };
  collect = e => {
    const { button, shiftKey, target, clientX, clientY } = e;
    // const initiator = e.target.id.startsWith("thumb") ? "thumb" : "bar";
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
    return {
      type: "scrollbar",
      id: this.props.id,
      button,
      shiftKey,
      clientX,
      clientY,
      x,
      y
    };
  };
  render() {
    return (
      <div
        className="zebulon-scrollbar-thumb"
        id={"thumb-" + this.props.id}
        style={this.props.style}
        // onMouseDown={this.handleMouseDown}
        // onMouseUp={this.handleMouseUp}
        // onMouseMove={this.handleMouseMove}
      />
    );
  }
}
// const scrollbarInner = ({ id, style, connectDragSource }) => (
//   // connectDragSource
//   <div className="zebulon-scrollbar-thumb" id={`thumb-${id}`} style={style} />
// );

// const barSpec = {
//   drop(props, monitor, component) {}
// };
// const collectDropTarget = connect => ({
//   connectDropTarget: connect.dropTarget()
// });

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
      // positionRatio: !this.isDragging
      //   ? this.props.positionRatio
      //   : position / this.props.length,
      positionRatio: position / this.props.length,
      position,
      initiator
    };
    // this.position = position;
    return event;
  };
  handleMouseDown = e => {
    const event = this.collect(e);
    if (event.initiator === "thumb") {
      this.isDragging = true;
      this.previous =
        this.props.direction === "horizontal" ? event.clientX : event.clientY;
      this.positionRatio = this.props.positionRatio;
      // console.log("down", event.y, event.positionRatio, event);
    } else if (!this.isDragging) {
      return this.props.onScroll(event);
    }
  };
  handleMouseUp = e => (this.isDragging = false);
  handleMouseMove = e => {
    if (this.isDragging) {
      const event = this.collect(e);
      e.preventDefault();
      const x =
        this.props.direction === "horizontal" ? event.clientX : event.clientY;
      const delta = x - this.previous;
      if (delta) {
        event.positionRatio = Math.min(
          Math.max(this.positionRatio + delta / this.props.length, 0),
          1 - this.innerSize / this.props.length
        );
        event.position = this.props.length * event.positionRatio;
        if (this.props.onScroll(event)) {
          this.previous = x;
          this.positionRatio = event.positionRatio;
          // console.log(
          //   "move",
          //   delta,
          //   this.previous,
          //   x,
          //   this.isDragging,
          //   event.position,
          //   this.props.length,
          //   event.positionRatio,
          //   event
          // );
        }
      }
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
