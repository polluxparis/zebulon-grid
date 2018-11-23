import React, { PureComponent } from "react";
// import { Grid as ReactVirtualizedGrid } from "react-virtualized/dist/commonjs/Grid";
import classnames from "classnames";
import { constants, utils } from "zebulon-controls";
import Header from "../Header/Header";
const { AxisType, ROOT_ID, MEASURE_ID } = constants;

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
      measures: this.props.measures,
      features: this.props.features
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

  cellRenderer = (header, offset, affix, lastSize) => {
    const {
      gridId,
      selectAxis,
      toggleMeasuresAxis,
      moveDimension,
      toggleMeasure,
      moveMeasure,
      toggleCollapse,
      axisType,
      previewSizes,
      measures,
      features,
      status,
      zoom,
      componentId
    } = this.props;
    const positionStyle = {
      position: "absolute",
      left:
        axisType === AxisType.ROWS
          ? header.sizes.cross.position
          : header.sizes.main.position - offset,
      top:
        axisType === AxisType.COLUMNS
          ? header.sizes.cross.position
          : header.sizes.main.position - offset,
      height:
        axisType === AxisType.ROWS
          ? header.sizes.main.size
          : header.sizes.cross.size,
      width:
        axisType === AxisType.COLUMNS
          ? header.sizes.main.size
          : header.sizes.cross.size
    };
    // affix management to keep labels on screen (except for leaves)
    if (header.sizes.affix && affix) {
      const affixOffset = Math.min(
        affix + offset,
        Math.max(header.sizes.main.size - lastSize, 0)
      );
      if (axisType === AxisType.COLUMNS) {
        positionStyle.paddingLeft = affixOffset;
      } else {
        positionStyle.paddingTop = affixOffset;
      }
    }
    return (
      <Header
        componentId={componentId}
        key={`header-${header.key}`}
        header={header}
        axis={axisType}
        caption={header.formatFunction({ value: header.caption })}
        positionStyle={positionStyle}
        dimensionId={header.dimensionId}
        isNotCollapsible={
          header.options.isNotCollapsible ||
          header.isAttribute ||
          header.isTotal
        }
        isCollapsed={header.isCollapsed}
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
        toggleMeasuresAxis={toggleMeasuresAxis}
        toggleMeasure={toggleMeasure}
        moveMeasure={moveMeasure}
        moveDimension={moveDimension}
        toggleCollapse={toggleCollapse}
        collectMenu={this.collectMenu}
        previewSizes={previewSizes}
        measuresCount={measures === null || Object.keys(measures).length}
        features={features}
        zoom={zoom}
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
    const cells = [];
    headers.cells.map(header =>
      header.map(cell =>
        cells.push(
          this.cellRenderer(
            cell,
            headers.offset,
            headers.affix,
            headers.lastSize
          )
        )
      )
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
