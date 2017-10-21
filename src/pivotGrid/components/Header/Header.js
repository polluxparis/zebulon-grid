import React, { Component } from "react";
import classNames from "classnames";

import { AxisType, toAxis, MEASURE_ID } from "../../constants";
import ResizeHandle from "../ResizeHandle/ResizeHandle";
import InnerHeader from "../InnerHeader/InnerHeader";
import { ContextMenuTrigger } from "react-contextmenu";
import { expandCollapseNode } from "../../selectors";
class Header extends Component {
  handleClickCollapse = () => {
    const { toggleCollapse, header, measuresCount } = this.props;
    toggleCollapse(
      header.key,
      expandCollapseNode(header, undefined, measuresCount)
    );
  };
  handleClick = () => {
    this.props.selectAxis(this.props.header);
  };

  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      if (data.action === "move") {
        this.props.toggleMeasuresAxis(
          this.props.axis === AxisType.ROWS ? "columns" : "rows"
        );
      } else if (data.action === "remove") {
        this.props.toggleMeasure(data.measureId);
      } else if (data.action === "add") {
        this.props.toggleMeasure(data.newMeasureId);
      }
    }
  };
  render() {
    const {
      axis,
      dimensionId,
      gridId,
      header,
      caption,
      positionStyle,
      previewSizes,
      isNotCollapsible,
      isCollapsed,
      collapseOffset,
      moveDimension,
      moveMeasure,
      toggleMeasuresAxis,
      collectMenu,
      isDropTarget,
      isDragSource
    } = this.props;
    let style = positionStyle;
    const rightKey = axis === AxisType.COLUMNS ? header.key : dimensionId;
    const bottomKey = axis === AxisType.ROWS ? header.key : dimensionId;
    const rightHeader = axis === AxisType.COLUMNS ? header : null;
    const bottomHeader = axis === AxisType.ROWS ? header : null;

    const head = (
      <div
        className="zebulon-grid-cell zebulon-grid-header zebulon-grid-column-header"
        style={{
          boxSizing: "border-box",
          overflow: "hidden",
          display: "flex",
          ...style
        }}
      >
        <InnerHeader
          axis={axis}
          id={dimensionId}
          measureId={dimensionId === MEASURE_ID ? header.id : null}
          index={header.index}
          caption={caption}
          isNotCollapsible={isNotCollapsible}
          isCollapsed={isCollapsed}
          collapseOffset={collapseOffset}
          handleClickCollapse={this.handleClickCollapse}
          handleClick={this.handleClick}
          handleClickMenu={this.handleClickMenu}
          moveDimension={moveDimension}
          moveMeasure={moveMeasure}
          toggleMeasuresAxis={toggleMeasuresAxis}
          isDropTarget={isDropTarget}
          isDragSource={isDragSource}
          gridId={gridId}
        />
        <ResizeHandle
          position="right"
          size={positionStyle.height}
          id={rightKey}
          axis={axis}
          header={rightHeader}
          previewSize={previewSizes.height}
          gridId={gridId}
        />
        <ResizeHandle
          position="bottom"
          size={positionStyle.width}
          id={bottomKey}
          gridId={gridId}
          axis={axis}
          header={bottomHeader}
          previewSize={previewSizes.width}
        />
      </div>
    );
    if (dimensionId === MEASURE_ID) {
      return (
        <ContextMenuTrigger
          id={`context-menu-${gridId}`}
          holdToDisplay={-1}
          collect={collectMenu}
          onItemClick={this.handleClickMenu}
          type={`header-${axis}`}
          axis={axis}
          dimensionId={dimensionId}
          measureId={header.id}
          caption={caption}
          style={{ width: "inherit" }}
        >
          {head}
        </ContextMenuTrigger>
      );
    } else {
      return head;
    }
  }
}

export default Header;
