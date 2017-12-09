import React, { Component } from "react";
import classNames from "classnames";
import { ContextualMenuClient } from "../Controls/ContextualMenu";
import { AxisType, MEASURE_ID } from "../../constants";
import ResizeHandle from "../ResizeHandle/ResizeHandle";
import InnerHeader from "../InnerHeader/InnerHeader";
// import { ContextMenuTrigger } from "react-contextmenu";
import { expandCollapseNode } from "../../selectors";
class Header extends Component {
  handleClickCollapse = () => {
    if (this.props.features.expandCollapse === "enabled") {
      const { toggleCollapse, header, measuresCount } = this.props;
      toggleCollapse(
        header.key,
        expandCollapseNode(header, undefined, measuresCount)
      );
    }
  };
  handleClick = () => {
    this.props.selectAxis(this.props.header);
  };

  // handleClickMenu = (e, data, target) => {
  //   if (e.button === 0) {
  //     if (data.action === "move") {
  //       this.props.toggleMeasuresAxis(
  //         this.props.axis === AxisType.ROWS ? "columns" : "rows"
  //       );
  //     } else if (data.action === "remove") {
  //       this.props.toggleMeasure(data.measureId);
  //     } else if (data.action === "add") {
  //       this.props.toggleMeasure(data.newMeasureId);
  //     }
  //   }
  // };
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
      toggleMeasure,
      collectMenu,
      isDropTarget,
      isDragSource,
      features
    } = this.props;
    let style = positionStyle;
    const rightKey = axis === AxisType.COLUMNS ? header.key : dimensionId;
    const bottomKey = axis === AxisType.ROWS ? header.key : dimensionId;
    const rightHeader = axis === AxisType.COLUMNS ? header : null;
    const bottomHeader = axis === AxisType.ROWS ? header : null;
    const className = classNames({
      "zebulon-grid-cell": true,
      "zebulon-grid-header": !header.isTotal,
      "zebulon-grid-column-header": axis === AxisType.COLUMNS,
      "zebulon-grid-row-header": axis === AxisType.ROWS,
      "zebulon-grid-header-total": header.isTotal === 1,
      "zebulon-grid-header-grandtotal": header.isTotal === 2
    });
    let resizeHandle = [];
    if (features.resize === "enabled") {
      resizeHandle = [
        <ResizeHandle
          key="right"
          position="right"
          size={positionStyle.height}
          id={rightKey}
          axis={axis}
          header={rightHeader}
          previewSize={previewSizes.height}
          gridId={gridId}
        />,
        <ResizeHandle
          key="bottom"
          position="bottom"
          size={positionStyle.width}
          id={bottomKey}
          gridId={gridId}
          axis={axis}
          header={bottomHeader}
          previewSize={previewSizes.width}
        />
      ];
    }
    let menuTrigger = null;
    if (true) {
      menuTrigger = `context-menu-${gridId}`;
    }
    const head = (
      <div
        className={className}
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
          isNotCollapsible={
            isNotCollapsible || this.props.features.expandCollapse !== "enabled"
          }
          isCollapsed={isCollapsed}
          collapseOffset={collapseOffset}
          handleClickCollapse={this.handleClickCollapse}
          handleClick={this.handleClick}
          handleClickMenu={this.handleClickMenu}
          moveDimension={moveDimension}
          moveMeasure={moveMeasure}
          toggleMeasuresAxis={toggleMeasuresAxis}
          isDropTarget={
            isDropTarget && this.props.features.dimensions === "enabled"
          }
          isDragSource={
            isDragSource && this.props.features.measures === "enabled"
          }
          gridId={gridId}
        />
        {resizeHandle}
      </div>
    );
    if (dimensionId === MEASURE_ID) {
      // return (
      //   <ContextMenuTrigger
      //     id={menuTrigger}
      //     holdToDisplay={-1}
      //     collect={collectMenu}
      //     onItemClick={this.handleClickMenu}
      //     type={`header-${axis}`}
      //     axis={axis}
      //     dimensionId={dimensionId}
      //     measureId={header.id}
      //     caption={caption}
      //     style={{ width: "inherit" }}
      //   >
      //     {head}
      //   </ContextMenuTrigger>
      // );
      return (
        <ContextualMenuClient
          menuId="measure-menu"
          id={`measure-menu-${header.id}`}
          collect={() =>
            collectMenu({
              axis,
              header,
              toggleMeasuresAxis,
              toggleMeasure
            })}
        >
          {head}
        </ContextualMenuClient>
      );
    } else {
      return head;
    }
  }
}

export default Header;
