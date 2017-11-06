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
      <div id={`grid ${gridId}`}>
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
        />
      </div>
    );
  }
}
