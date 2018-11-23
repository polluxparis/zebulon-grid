import React, { Component } from "react";
import classNames from "classnames";
import { ContextualMenuClient, constants } from "zebulon-controls";
import ResizeHandle from "../ResizeHandle/ResizeHandle";
import InnerHeader from "../InnerHeader/InnerHeader";
import { expandCollapseNode } from "../../selectors";
const { AxisType, MEASURE_ID } = constants;
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
      features,
      zoom,
      componentId
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
    // let menuTrigger = null;
    // if (true) {
    //   menuTrigger = `context-menu-${gridId}`;
    // }
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
          zoom={zoom}
          gridId={gridId}
        />
        {resizeHandle}
      </div>
    );
    if (dimensionId === MEASURE_ID) {
      return (
        <ContextualMenuClient
          menu="measure-menu"
          id={`measure-menu-${header.id}`}
          component={`pivot-grid-${gridId}`}
          componentId={componentId}
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
