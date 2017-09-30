import React, { PureComponent } from "react";
import { Grid as ReactVirtualizedGrid } from "react-virtualized/dist/commonjs/Grid";
import classnames from "classnames";

import {
  // MEASURE_ID,
  // ROOT_ID,
  // TOTAL_ID,
  // HeaderType,
  AxisType
} from "../../constants";
import HeaderComponent from "../Header/Header";
// import { isNull } from "../../utils/generic";
// import { getLeaves } from "../../utils/headers";

class Headers extends PureComponent {
  componentWillReceiveProps = nextProps => {
    if (nextProps.headers !== this.props.headers) {
      this.cellCache = {};
    }
  };

  // componentDidUpdate = prevProps => {
  //   if (
  //     prevProps.sizes !== this.props.sizes ||
  //     prevProps.dimensions !== this.props.dimensions ||
  //     prevProps.zoom !== this.props.zoom
  //   ) {
  //   }
  //   this.grid.recomputeGridSize();
  // };
  // shouldComponentUpdate = () => false;

  collectMenu = props => {
    return {
      ...props,
      availableMeasures: this.props.availableMeasures,
      measures: this.props.measures
    };
  };

  headersRenderer = () => {
    const {
      scrollToRow,
      scrollToColumn,
      height,
      width,
      headers,
      gridId,
      selectAxis,
      moveDimension,
      toggleMeasure,
      moveMeasure,
      toggleCollapse,
      axisType,
      previewSizes
    } = this.props;
    const startIndex =
      axisType === AxisType.ROWS ? scrollToRow : scrollToColumn;
    const firstHeader = headers[startIndex][0].header;
    const maxSize = axisType === AxisType.ROWS ? height : width;
    const firstPosition = firstHeader.main.position;
    // const offsetAdjustment = scroll - firstLeaf.main.position

    const startRootIndex = firstHeader.rootIndex;
    // const firstSize = firstLeaf.main.size;
    let size = 0;
    let index = startRootIndex;

    // console.log("firstLeaf", firstLeaf, scrollTop, offsetAdjustment);
    // const correctStopIndex = Math.min(stopIndex, headers.length - 1);
    const cells = [];
    while (size < maxSize && index < headers.length) {
      headers[index].map((header, i) => {
        const h = header.header;
        if (h.lastIndex >= startIndex) {
          const positionStyle = {
            position: "absolute",
            left:
              axisType === AxisType.ROWS
                ? h.cross.position
                : h.main.position - firstPosition, // + offsetAdjustment,
            top:
              axisType === AxisType.COLUMNS
                ? h.cross.position
                : h.main.position - firstPosition, // + offsetAdjustment,
            height: axisType === AxisType.ROWS ? h.main.size : h.cross.size,
            width: axisType === AxisType.COLUMNS ? h.main.size : h.cross.size
          };
          if (h.cross.collapsed) {
            axisType === AxisType.ROWS
              ? (positionStyle.paddingRight = h.cross.collapsed)
              : (positionStyle.paddingBottom = h.cross.collapsed);
          }
          cells.push(
            <HeaderComponent
              key={`header-${h.key}`}
              header={h}
              axis={axisType}
              caption={header.caption}
              positionStyle={positionStyle}
              // span={header.span}
              // scrollLeft={scrollLeft}
              // scrollTop={scrollTop}
              // firstSize={firstSize}
              dimensionId={header.dimensionId}
              isNotCollapsible={header.isNotCollapsible}
              isCollapsed={header.header.isCollapsed}
              isAffixManaged={h.index < startIndex && header.isAffixManaged}
              isDropTarget={header.isDropTarget}
              gridId={gridId}
              selectAxis={selectAxis}
              moveDimension={moveDimension}
              toggleMeasure={toggleMeasure}
              moveMeasure={moveMeasure}
              toggleCollapse={toggleCollapse}
              collectMenu={this.collectMenu}
              previewSizes={previewSizes}
            />
          );
          if (i === 0) {
            size += h.main.size;
          }
        }
      });
      index++;
    }
    return cells;
  };
  render() {
    const {
      // headers,
      scrollToRow,
      scrollToColumn,
      height,
      width,
      axisType,
      // rowCount,
      // columnCount,
      // columnsSize,
      // rowsSize,
      gridId
    } = this.props;
    const cells = this.headersRenderer();
    const style = {
      position: "relative",
      overflow: "hidden",
      height,
      width
      // borderRight: "solid 0.03em grey",
      // borderBottom: "solid 0.03em grey",
      // borderRadius: " 0.25rem"
    };
    // console.log("header render", axisType, scrollTop, scrollLeft);
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
