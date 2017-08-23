import React from 'react';
import { DragSource, DropTarget } from 'react-dnd';
import { isNullOrUndefined } from '../../utils/generic';
import { MEASURE_ID, ROOT_ID, toAxis, AxisType } from '../../constants';
import { rightArrow, downArrow } from '../../icons';

// -------------------------------
const headerSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    let newAxis, index;
    if (props.id === MEASURE_ID || props.id === ROOT_ID) {
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
};
const InnerHeaderSpec = {
  beginDrag(props) {
    return {
      axis: props.axis,
      id: props.id,
      gridId: props.gridId,
      index: props.index,
      previewSize: props.previewSize
    };
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
  index,
  caption,
  isNotCollapsible,
  isCollapsed,
  handleClick,
  handleClickCollapse,
  moveDimension,
  connectDragSource,
  connectDropTarget,
  isDropTarget,
  gridId
}) => {
  const computedStyle = {
    whiteSpace: 'nowrap',
    overflow: 'hidden',
    display: 'flex',
    width: 'inherit'
  };
  let collapsedIcon;
  if (!isCollapsed && !isNotCollapsible) {
    collapsedIcon = (
      <div
        style={{
          background: downArrow,
          backgroundSize: 'cover',
          height: '1em',
          width: '1em',
          marginTop: '0.1em',
          marginRight: '0.1em'
        }}
        onClick={handleClickCollapse}
      />
    );
  } else if (isCollapsed && !isNotCollapsible) {
    collapsedIcon = (
      <div
        style={{
          background: rightArrow,
          backgroundSize: 'cover',
          height: '1em',
          width: '1em',
          marginTop: '0.1em',
          marginRight: '0.1em'
        }}
        onClick={handleClickCollapse}
      />
    );
  } else {
    collapsedIcon = (
      <div
        style={{
          height: '1em',
          width: '1em',
          marginTop: '0.1em',
          marginRight: '0.1em'
        }}
      />
    );
  }
  let header = (
    <div style={{ width: 'inherit' }} onClick={handleClick}>
      {caption}
    </div>
  );
  header = (
    <div className="pivotgrid-header-inner" style={computedStyle}>
      {collapsedIcon}
      {header}
    </div>
  );
  // id={id}
  //       index={index}
  //       moveDimension={moveDimension}
  // drag and drop of dimension headers to move dimensions
  // dimension header -> drag source
  if (!isNullOrUndefined(index)) {
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
