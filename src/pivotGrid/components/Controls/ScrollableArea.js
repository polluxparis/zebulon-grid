import React, { Component } from "react";

import { ScrollbarSize } from "../../constants";
import { Scrollbar } from "./Scrollbar";

export class ScrollableArea extends Component {
  constructor(props) {
    super(props);
    this.ratios = {
      vertical: { display: 1, position: 0 },
      horizontal: { display: 1, position: 0 }
    };
    this.drag = { vertical: {}, horizontal: {} };
  }
  getRatios = () => this.ratios;
  getScrollbars = (height, width, ratios) => {
    const scrollbars = {};
    if (ratios.vertical.display < 1) {
      scrollbars.vertical = { width: ScrollbarSize };
    }
    if (ratios.horizontal.display < 1) {
      scrollbars.horizontal = { width: ScrollbarSize };
    }
    if (
      scrollbars.vertical &&
      !scrollbars.horizontal &&
      ratios.horizontal.display * (width - ScrollbarSize) / width < 1
    ) {
      scrollbars.horizontal = { width: ScrollbarSize };
      ratios.horizontal.display =
        ratios.horizontal.display * (width - ScrollbarSize) / width;
    }
    if (
      !scrollbars.vertical &&
      scrollbars.horizontal &&
      ratios.vertical.display * (height - ScrollbarSize) / height < 1
    ) {
      scrollbars.horizontal = { width: ScrollbarSize };
      ratios.vertical.display =
        ratios.vertical.display * (width - ScrollbarSize) / width;
    }
    if (scrollbars.vertical && scrollbars.horizontal) {
      scrollbars.horizontal.length = width - ScrollbarSize;
      scrollbars.vertical.length = height - ScrollbarSize;
    } else if (scrollbars.vertical) {
      scrollbars.vertical.length = Math.min(
        height,
        height / ratios.vertical.display
      );
      scrollbars.horizontal = {
        length: Math.min(
          width - ScrollbarSize,
          width / ratios.horizontal.display
        ),
        width: 0
      };
    } else if (scrollbars.horizontal) {
      scrollbars.horizontal.length = Math.min(
        width,
        width / ratios.horizontal.display
      );
      scrollbars.vertical = {
        length: Math.min(
          height - ScrollbarSize,
          height / ratios.vertical.display
        ),
        width: 0
      };
    } else {
      scrollbars.horizontal = {
        length: Math.min(width, width / ratios.horizontal.display),
        width: 0
      };
      scrollbars.vertical = {
        length: Math.min(height, height / ratios.vertical.display),
        width: 0
      };
    }
    scrollbars.horizontal.innerSize = Math.max(
      30,
      width * ratios.horizontal.display
    );
    scrollbars.vertical.innerSize = Math.max(
      30,
      height * ratios.vertical.display
    );
    this.scrollbars = scrollbars;
    return scrollbars;
  };
  getContent = () => [];
  onScroll = e => {
    if (e.type === "scrollbar") {
      if (e.initiator === "bar") {
        if (e.direction === "horizontal") {
          this.ratios.horizontal.position = e.positionRatio;
        } else {
          this.ratios.vertical.position = e.positionRatio;
        }
        if (this.props.onScroll) {
          this.props.onScroll(e);
        }
      }
      return true;
    }
    return false;
  };

  collect = e => {
    const { button, shiftKey, target, clientX, clientY } = e;
    const initiator = e.target.id.startsWith("thumb") ? "thumb" : "bar";
    const rect = target.getBoundingClientRect();
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
    return event;
  };
  handleDrag = (type, event) => {
    const { length, innerSize } = this.scrollbars[event.direction];
    event.thumbRatio = innerSize / length;
    const size = this.ratios[event.direction].display * length;
    if (type === "startDrag") {
      this.drag[event.direction] = {
        isDragging: true,
        previousPosition: event.position,
        previousPositionRatio: this.ratios[event.direction].position
      };
      // this.positionRatio = this.props.positionRatio;
    } else if (type === "endDrag") {
      this.drag[event.direction] = { isDragging: false };
    } else if (type === "drag") {
      const drag = this.drag[event.direction];
      const delta = event.position - drag.previousPosition;
      if (delta) {
        event.positionRatio = Math.min(
          Math.max(drag.previousPositionRatio + delta / length, 0),
          1 - size / length
        );
        // event.position = length * event.positionRatio;
        if (this.onScroll(event)) {
          drag.previousPosition = event.position;
          drag.previousPositionRatio = event.positionRatio;
        }
      }
    } else if (type === "click") {
      event.positionRatio =
        Math.max(
          0,
          Math.min(event.relativePosition - size / 2, length - size)
        ) / length;
      this.onScroll(event);
    }
  };
  handleMouseMove = e => {
    if (
      (this.drag.vertical.isDragging || this.drag.horizontal.isDragging) &&
      e.buttons
    ) {
      const direction = this.drag.vertical.isDragging
        ? "vertical"
        : "horizontal";
      this.handleDrag("drag", {
        type: "scrollbar",
        direction,
        position: direction === "horizontal" ? e.clientX : e.clientY
      });
    }
  };
  render() {
    const { height, width, gridId } = this.props;

    this.ratios = this.getRatios(height, width);
    const scrollbars = this.getScrollbars(height, width, this.ratios);
    const content = this.getContent(height, width);
    const style = {
      position: "relative",
      overflow: "hidden",
      height: scrollbars.vertical.length,
      width: scrollbars.horizontal.length
    };
    return (
      <div id={`grid ${gridId}`} onMouseMove={this.handleMouseMove}>
        <div style={{ display: "flex" }}>
          <div style={style}>{content}</div>
          <Scrollbar
            direction="vertical"
            width={scrollbars.vertical.width}
            length={scrollbars.vertical.length}
            positionRatio={this.ratios.vertical.position}
            displayRatio={this.ratios.vertical.display}
            id={`vertical-scrollbar ${gridId}`}
            onScroll={this.onScroll}
            handleDrag={this.handleDrag}
          />
        </div>
        <Scrollbar
          direction="horizontal"
          width={scrollbars.horizontal.width}
          length={scrollbars.horizontal.length}
          positionRatio={this.ratios.horizontal.position}
          displayRatio={this.ratios.horizontal.display}
          id={`horizontal-scrollbar ${gridId}`}
          onScroll={this.onScroll}
          handleDrag={this.handleDrag}
        />
      </div>
    );
  }
}
