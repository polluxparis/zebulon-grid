import React from 'react';
import { DragSource, DropTarget } from 'react-dnd';
import { toAxis, AxisType } from '../../Axis';
import { isNullOrUndefined } from '../../utils/generic';
import { MEASURE_ID, ROOT_ID } from '../../constants';
import { rightArrow, downArrow } from '../../icons';
// -------------------------------
const headerSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();

    // const initialOffset = monitor.getInitialClientOffset();
    // const offset = monitor.getClientOffset();
    // component.props.updateCellSize({ handle, offset, initialOffset });
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

const sourceCollect = (connect, monitor) => ({
  connectDragSource: connect.dragSource(),
  connectDragPreview: connect.dragPreview(),
  isDragging: monitor.isDragging()
});
// --------------------------------
const innerHeader = ({
  axis,
  id,
  index,
  caption,
  isNotCollapsible,
  isCollapsed,
  handleClickSort,
  handleClickCollapse,
  moveDimension,
  connectDragSource,
  connectDropTarget
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
    collapsedIcon = <div> </div>;
  }
  let header = (
    <div
      className="pivotgrid-header-inner"
      style={computedStyle}
      onClick={handleClickSort}
      axis={axis}
      id={id}
      index={index}
      moveDimension={moveDimension}
    >
      {collapsedIcon}<div style={{ width: 'inherit' }}>{caption}</div>
    </div>
  );
  if (!isNullOrUndefined(index)) {
    header = connectDragSource(header);
  }
  if (!isNullOrUndefined(moveDimension)) {
    header = connectDropTarget(header);
  }
  return header;
};
const collect = connect => ({
  connectDropTarget: connect.dropTarget()
});
export default DropTarget(
  props => `cell-inner-header--${props.gridId}`,
  headerSpec,
  collect
)(
  DragSource(
    props => `cell-inner-header--${props.gridId}`,
    InnerHeaderSpec,
    sourceCollect
  )(innerHeader)
);
