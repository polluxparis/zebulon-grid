import React from "react";
import { DragSource, DropTarget } from "react-dnd";
// import { isNullOrUndefined } from "../../utils/generic";
import { MEASURE_ID, TOTAL_ID, toAxis, AxisType } from "../../constants";
import { icons } from "zebulon-controls";
import classNames from "classnames";
console.log("icons", icons);

// -------------------------------
const headerSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    if (
      props.id === MEASURE_ID &&
      handle.id === MEASURE_ID &&
      props.measureId &&
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
        if (toAxis(handle.axis) !== newAxis || handle.index !== index) {
          props.moveDimension(handle.id, toAxis(handle.axis), newAxis, index);
        }
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
  gridId,
  zoom
}) => {
  const computedStyle = {
    whiteSpace: "nowrap",
    overflow: "hidden",
    display: "-webkit-box",
    width: "inherit"
  };
  let collapsedIcon;
  if (!isCollapsed && !isNotCollapsible) {
    collapsedIcon = (
      <div
        style={{
          background: icons.downArrow,
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
          background: icons.rightArrow,
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
    collapsedIcon = null;
  }
  const style = { width: "-webkit-fill-available" };
  if (collapseOffset || collapsedIcon) {
    if (axis === AxisType.ROWS) {
      style.paddingRight =
        (collapseOffset || 0) + (collapsedIcon ? 18 * (zoom || 1) : 0);
    } else {
      style.paddingBottom = collapseOffset;
      style.paddingRight = collapsedIcon ? 18 * (zoom || 1) : 0;
    }
  }

  let header = (
    <div style={style} onClick={handleClick}>
      {caption}
    </div>
  );
  const className = classNames({
    "zebulon-grid-header-inner": !(measureId || !handleClick),
    "zebulon-grid-header-inner-center": measureId || !handleClick
  });
  header = (
    <div className={className} style={computedStyle}>
      {collapsedIcon}
      {header}
    </div>
  );

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
