import React, { PureComponent } from "react";
import classNames from "classnames";
import { ContextualMenuClient } from "../Controls/ContextualMenu";
import InnerHeader from "../InnerHeader/InnerHeader";
import ResizeHandle from "../ResizeHandle/ResizeHandle";

import {
  MEASURE_ID,
  ROOT_ID,
  TOTAL_ID,
  AxisType,
  toAxis
} from "../../constants";
// import { ContextMenuTrigger } from "react-contextmenu";

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

  // handleClickMenu = (e, data, target) => {
  //   if (e.button === 0 && data) {
  //     if (data.action === "remove") {
  //       this.props.moveDimension(
  //         data.dimensionId,
  //         toAxis(data.axis),
  //         toAxis(AxisType.DIMENSION)
  //       );
  //     } else if (data.action === "add") {
  //       this.props.moveDimension(
  //         data.newDimensionId,
  //         toAxis(AxisType.DIMENSION),
  //         toAxis(data.axis),
  //         // Add dimension after dimension which opened the context menu
  //         // except if it's the measures dimension. In this case, add before
  //         data.dimensionId === MEASURE_ID ? data.index : data.index + 1
  //       );
  //     } else if (data.action === "expand all") {
  //       const keys = this.props.getExpandCollapseKeys(
  //         data.axis,
  //         data.index,
  //         false
  //       );
  //       this.props.expandCollapseAll(data.axis, keys.keys, keys.n);
  //     } else if (data.action === "collapse all") {
  //       const keys = this.props.getExpandCollapseKeys(
  //         data.axis,
  //         data.index,
  //         true
  //       );
  //       this.props.expandCollapseAll(data.axis, keys.keys, keys.n);
  //     } else if (data.action === "toggle subtotal") {
  //       this.props.toggleSubTotal(data.dimensionId);
  //     } else if (data.action === "toggle grandtotal") {
  //       this.props.toggleSubTotal(`${toAxis(data.axis)}${TOTAL_ID}`);
  //     }
  //   }
  // };

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
    let menuTrigger = null;
    if (true) {
      menuTrigger = `context-menu-${gridId}`;
    }
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
        menuId="dimension-menu"
        id={`dimensions-menu-${dimension.id}`}
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
