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
    const position = firstHeader.main.position;
    let header = firstHeader;
    while (header.parent && header.parent.id !== ROOT_ID) {
      header = header.parent;
      offsets[header.depth] = position - header.main.position;
    }
    return offsets;
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
    const scroll = axisType === AxisType.ROWS ? scrollToRow : scrollToColumn;
    let { startIndex, offset } = scroll;
    if (startIndex > headers.length) {
      startIndex = 0;
      offset = 0;
    }
    const firstHeader = headers[startIndex][0].header;
    const maxSize = axisType === AxisType.ROWS ? height : width;
    const firstPosition = firstHeader.main.position;
    //  start looping from the root of the first leaf header
    const startRootIndex = firstHeader.rootIndex || 0;
    // const firstSize = firstLeaf.main.size;
    let size = 0;
    let index = startRootIndex;
    // calculate affix offset
    const affix = this.affixOffsets(firstHeader, offset);
    const cells = [];
    while (size < maxSize - offset && index < headers.length) {
      headers[index].map((header, i) => {
        const h = header.header;
        // don't add headers not parent of the first leaf
        if (h.lastIndex >= startIndex) {
          const positionStyle = {
            position: "absolute",
            left:
              axisType === AxisType.ROWS
                ? h.cross.position
                : h.main.position - firstPosition + offset,
            top:
              axisType === AxisType.COLUMNS
                ? h.cross.position
                : h.main.position - firstPosition + offset,
            height: axisType === AxisType.ROWS ? h.main.size : h.cross.size,
            width: axisType === AxisType.COLUMNS ? h.main.size : h.cross.size
          };
          if (h.cross.collapsed) {
            axisType === AxisType.ROWS
              ? (positionStyle.paddingRight = h.cross.collapsed)
              : (positionStyle.paddingBottom = h.cross.collapsed);
          }
          // affix management to keep labels on screen (except for leaves)
          if (h.index < startIndex && header.isAffixManaged) {
            // let offset;
            if (axisType === AxisType.COLUMNS) {
              positionStyle.paddingLeft = affix[h.depth];
            } else {
              positionStyle.paddingTop = affix[h.depth];
            }
          }
          cells.push(
            // index,
            // measureId,
            <Header
              key={`header-${h.key}`}
              header={h}
              axis={axisType}
              caption={header.caption}
              positionStyle={positionStyle}
              dimensionId={header.dimensionId}
              isNotCollapsible={header.isNotCollapsible}
              isCollapsed={h.isCollapsed}
              isDropTarget={header.isDropTarget}
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
    const { height, width, axisType, gridId } = this.props;
    const cells = this.headersRenderer();
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
