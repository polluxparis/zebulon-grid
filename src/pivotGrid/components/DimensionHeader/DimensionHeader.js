import React, { PureComponent } from "react";
import classNames from "classnames";
import { ContextualMenuClient, constants } from "zebulon-controls";
import InnerHeader from "../InnerHeader/InnerHeader";
import ResizeHandle from "../ResizeHandle/ResizeHandle";
const { MEASURE_ID, ROOT_ID, AxisType } = constants;
class DimensionHeader extends PureComponent {
  handleClickCollapse = e => {
    if (e.button === 0 && this.props.features.expandCollapse === "enabled") {
      const { toggleCollapseDimension, dimension } = this.props;
      toggleCollapseDimension(dimension.id);
    }
  };
  handleClickSort = e => {
    this.button = e.button;
    if (e.button === 0 && this.props.features.sorting === "enabled") {
      const { toggleSortOrder, dimension } = this.props;
      if (dimension.id !== MEASURE_ID && dimension.id !== ROOT_ID) {
        toggleSortOrder(dimension.id);
      }
    }
  };
  render() {
    const {
      positions,
      dimension,
      crossDimensionId,
      previewSizes,
      gridId,
      moveDimension,
      collectMenu,
      isFiltered,
      features,
      toggleSortOrder,
      expandCollapseAll,
      toggleSubTotal,
      getExpandCollapseKeys
    } = this.props;

    const ids = {};
    if (dimension.axis === AxisType.ROWS) {
      ids.right = dimension.id;
      ids.bottom = crossDimensionId;
    } else {
      ids.bottom = dimension.id;
      ids.right = crossDimensionId;
    }
    const { left, top, width, height } = positions;
    const className = classNames({
      "zebulon-grid-cell": true,
      "zebulon-grid-dimension-header": true,
      "zebulon-grid-dimension-header-column":
        dimension.axis === AxisType.COLUMNS,
      "zebulon-grid-dimension-header-row": dimension.axis === AxisType.ROWS,
      "zebulon-grid-dimension-attribute-header": dimension.isAttribute,
      "zebulon-grid-dimension-filtered-header": isFiltered
    });
    let resizeHandle = [];
    if (features.resize === "enabled") {
      resizeHandle = [
        <ResizeHandle
          key="right"
          position="right"
          size={height}
          id={ids.right}
          axis={AxisType.ROWS}
          gridId={gridId}
          previewSize={previewSizes.height}
        />,
        <ResizeHandle
          key="bottom"
          position="bottom"
          size={width}
          gridId={gridId}
          id={ids.bottom}
          axis={AxisType.COLUMNS}
          previewSize={previewSizes.width}
        />
      ];
    }
    // let menuTrigger = null;
    // if (true) {
    //   menuTrigger = `context-menu-${gridId}`;
    // }
    let header = (
      <div
        key={`fixed-dim-${dimension.id}`}
        className={className}
        style={{
          position: "absolute",
          left,
          top,
          width,
          height,
          boxSizing: "border-box",
          display: "flex"
        }}
        onClick={this.handleClickSort}
      >
        <InnerHeader
          axis={dimension.axis}
          id={dimension.id}
          index={dimension.depth}
          caption={dimension.caption}
          isNotCollapsible={
            !dimension.hasAttribute ||
            this.props.features.expandCollapse !== "enabled"
          }
          isCollapsed={dimension.isCollapsed}
          handleClickCollapse={this.handleClickCollapse}
          moveDimension={moveDimension}
          isDropTarget={this.props.features.dimensions === "enabled"}
          isDragSource={this.props.features.dimensions === "enabled"}
          gridId={gridId}
        />
        {resizeHandle}
      </div>
    );
    // return header;
    const collapseAll = collapse => {
      const keys = getExpandCollapseKeys(
        dimension.axis,
        dimension.depth,
        collapse
      );
      return expandCollapseAll(dimension.axis, keys.keys, keys.n);
    };
    return (
      <ContextualMenuClient
        menu="dimension-menu"
        id={`dimensions-menu-${dimension.id}`}
        component={`pivot-grid-${gridId}`}
        collect={() =>
          collectMenu({
            dimension,
            toggleSortOrder,
            moveDimension,
            collapseAll,
            toggleSubTotal,
            getExpandCollapseKeys
          })}
      >
        {header}
      </ContextualMenuClient>
    );
  }
}

export default DimensionHeader;
