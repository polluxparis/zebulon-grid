import React, { PureComponent } from "react";
// import { Grid as ReactVirtualizedGrid } from "react-virtualized/dist/commonjs/Grid";
import classnames from "classnames";
import { ROOT_ID, MEASURE_ID } from "../../constants";
import { AxisType } from "../../constants";
import Header from "../Header/Header";
// import { isNull } from "../../utils/generic";
// import { getLeaves } from "../../utils/headers";

class Headers extends PureComponent {
  componentWillReceiveProps = nextProps => {
    if (nextProps.headers !== this.props.headers) {
      this.cellCache = {};
    }
  };

  collectMenu = props => {
    return {
      ...props,
      availableMeasures: this.props.availableMeasures,
      measures: this.props.measures
    };
  };

  affixOffsets = (firstHeader, offset) => {
    const offsets = {};
    const position = firstHeader.sizes.main.position;
    let header = firstHeader;
    while (header.parent && header.parent.id !== ROOT_ID) {
      header = header.parent;
      offsets[header.depth] = position - header.sizes.main.position;
    }
    return offsets;
  };

  cellRenderer = (header, affix) => {
    const {
      gridId,
      selectAxis,
      moveDimension,
      toggleMeasure,
      moveMeasure,
      toggleCollapse,
      axisType,
      previewSizes,
      measures
    } = this.props;
    const positionStyle = {
      position: "absolute",
      left:
        axisType === AxisType.ROWS
          ? header.sizes.cross.position
          : header.sizes.main.position,
      top:
        axisType === AxisType.COLUMNS
          ? header.sizes.cross.position
          : header.sizes.main.position,
      height:
        axisType === AxisType.ROWS
          ? header.sizes.main.size
          : header.sizes.cross.size,
      width:
        axisType === AxisType.COLUMNS
          ? header.sizes.main.size
          : header.sizes.cross.size
    };
    // if (header.sizes.cross.collapsed) {
    //   axisType === AxisType.ROWS
    //     ? (positionStyle.paddingRight = header.sizes.cross.collapsed)
    //     : (positionStyle.paddingBottom = header.sizes.cross.collapsed);
    // }
    // affix management to keep labels on screen (except for leaves)
    if (header.isAffixManaged) {
      // let offset;
      if (axisType === AxisType.COLUMNS) {
        positionStyle.paddingLeft = affix[header.depth];
      } else {
        positionStyle.paddingTop = affix[header.depth];
      }
    }
    return (
      <Header
        key={`header-${header.key}`}
        header={header}
        axis={axisType}
        caption={header.caption}
        positionStyle={positionStyle}
        dimensionId={header.dimensionId}
        isNotCollapsible={header.options.isNotCollapsible}
        isCollapsed={header.options.isCollapsed}
        collapseOffset={
          header.sizes.cross.collapsed ? header.sizes.cross.collapsed : 0
        }
        isDropTarget={header.options.isDropTarget}
        isDragSource={
          header.dimensionId === MEASURE_ID &&
          Object.keys(this.props.measures || {}).length > 1
        }
        gridId={gridId}
        selectAxis={selectAxis}
        moveDimension={moveDimension}
        toggleMeasure={toggleMeasure}
        moveMeasure={moveMeasure}
        toggleCollapse={toggleCollapse}
        collectMenu={this.collectMenu}
        previewSizes={previewSizes}
        measuresCount={measures === null || Object.keys(measures).length}
      />
    );
  };
  cellsRenderer = () => {
    const headers =
      this.props.axisType === AxisType.ROWS
        ? this.props.rows
        : this.props.columns;
    if (headers === undefined || !headers.cells.length) {
      return [];
    }
    const affix = this.affixOffsets(headers.cells[0][0], headers.offset);
    const cells = [];
    headers.cells.map(header =>
      header.map(cell => cells.push(this.cellRenderer(cell, affix)))
    );
    return cells;
  };
  render() {
    const { height, width, axisType, gridId } = this.props;
    const cells = this.cellsRenderer();
    const style = {
      position: "relative",
      overflow: "hidden",
      height,
      width
    };
    return (
      <div
        className={classnames({
          "zebulon-grid-column-headers": axisType === AxisType.COLUMNS,
          "zebulon-grid-row-headers": axisType === AxisType.ROWS
        })}
        id={`headers - ${axisType} - ${gridId}`}
        style={style}
      >
        {cells}
      </div>
    );
  }
}

export default Headers;
