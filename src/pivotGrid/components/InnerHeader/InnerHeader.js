import React from "react";
import { DragSource, DropTarget } from "react-dnd";
// import { isNullOrUndefined } from "../../utils/generic";
import { MEASURE_ID, TOTAL_ID, toAxis, AxisType } from "../../constants";
import { rightArrow, downArrow } from "../../icons";

// -------------------------------
const headerSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    if (
      props.id === MEASURE_ID &&
      handle.id === MEASURE_ID &&
      handle.measureId !== props.measureId
    ) {
      props.moveMeasure(handle.measureId, props.measureId);
    } else if (handle.id !== props.id) {
      let newAxis, index;
      if (props.id === MEASURE_ID && handle.id === MEASURE_ID) {
        if (handle && handle.measureId) {
          props.moveMeasure(handle.measureId, props.measureId);
        }
      } else if (
        (handle.id === MEASURE_ID || handle.id === TOTAL_ID) &&
        props.id !== MEASURE_ID &&
        handle.axis !== props.axis
      ) {
        (handle.toggleMeasuresAxis || props.toggleMeasuresAxis)(
          handle.axis === AxisType.ROWS ? "columns" : "rows"
        );
      } else {
        if (props.id === MEASURE_ID || props.id === TOTAL_ID) {
          newAxis = toAxis(
            handle.axis === AxisType.ROWS ? AxisType.COLUMNS : AxisType.ROWS
          );
          index = 0;
        } else {
          newAxis = toAxis(props.axis);
          index = props.index + 1;
        }
        props.moveDimension(handle.id, toAxis(handle.axis), newAxis, index);
      }
    }
  }
};
const InnerHeaderSpec = {
  beginDrag(props) {
    return props;
  }
};

const collectDragSource = (connect, monitor) => ({
  connectDragSource: connect.dragSource(),
  connectDragPreview: connect.dragPreview(),
  isDragging: monitor.isDragging()
});
const collectDropTarget = connect => ({
  connectDropTarget: connect.dropTarget()
});

// --------------------------------
const innerHeader = ({
  axis,
  id,
  measureId,
  key,
  index,
  caption,
  isNotCollapsible,
  isCollapsed,
  collapseOffset,
  handleClick,
  handleClickCollapse,
  toggleMeasuresAxis,
  moveDimension,
  moveMeasure,
  connectDragSource,
  connectDropTarget,
  isDropTarget,
  isDragSource,
  gridId
}) => {
  const computedStyle = {
    whiteSpace: "nowrap",
    overflow: "hidden",
    display: "flex",
    width: "inherit"
  };
  let collapsedIcon;
  if (!isCollapsed && !isNotCollapsible) {
    collapsedIcon = (
      <div
        style={{
          background: downArrow,
          backgroundSize: "cover",
          height: "1em",
          width: "1em",
          marginTop: "0.1em",
          marginRight: "0.1em"
        }}
        onClick={handleClickCollapse}
      />
    );
  } else if (isCollapsed && !isNotCollapsible) {
    collapsedIcon = (
      <div
        style={{
          background: rightArrow,
          backgroundSize: "cover",
          height: "1em",
          width: "1em",
          marginTop: "0.1em",
          marginRight: "0.1em"
        }}
        onClick={handleClickCollapse}
      />
    );
  } else {
    collapsedIcon = (
      <div
        style={{
          height: "1em",
          width: "1em",
          marginTop: "0.1em",
          marginRight: "0.1em"
        }}
      />
    );
  }
  const style = { width: "-webkit-fill-available" };
  if (collapseOffset) {
    if (axis === AxisType.ROWS) {
      style.paddingRight = collapseOffset;
    } else {
      style.paddingBottom = collapseOffset;
    }
  }

  let header = (
    <div style={style} onClick={handleClick}>
      {caption}
    </div>
  );
  header = (
    <div className="zebulon-grid-header-inner" style={computedStyle}>
      {collapsedIcon}
      {header}
    </div>
  );
  // id={id}
  //       index={index}
  //       moveDimension={moveDimension}
  // drag and drop of dimension headers to move dimensions
  // dimension header -> drag source
  //             index={HeaderType.MEASURE && measuresCount > 1 ? 0 : null}

  if (isDragSource) {
    header = connectDragSource(header);
  }
  // dimension header -> drop target
  if (isDropTarget) {
    header = connectDropTarget(header);
  }

  return header;
};

export default DropTarget(
  props => `cell-inner-header--${props.gridId}`,
  headerSpec,
  collectDropTarget
)(
  DragSource(
    props => `cell-inner-header--${props.gridId}`,
    InnerHeaderSpec,
    collectDragSource
  )(innerHeader)
);
