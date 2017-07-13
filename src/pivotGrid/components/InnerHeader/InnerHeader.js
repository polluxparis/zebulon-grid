import React from 'react';
import { DragSource, DropTarget } from 'react-dnd';
import { toAxis, AxisType } from '../../Axis';
import { isNullOrUndefined } from '../../utils/generic';
import { MEASURE_ID, ROOT_ID } from '../../constants';
import { rightArrow, downArrow } from '../../icons';
import {
  ContextMenu,
  MenuItem,
  ContextMenuTrigger,
  connectMenu
} from 'react-contextmenu';

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
  handleClickMenu,
  moveDimension,
  connectDragSource,
  connectDropTarget,
  collectMenu,
  gridId,
  isRightClick
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
    <div style={{ width: 'inherit' }} onClick={handleClick}>
      {caption}
    </div>
  );
  // contextual menu
  // if ((false && !isNullOrUndefined(index)) || id === MEASURE_ID) {
  if (!isNullOrUndefined(index) || id === MEASURE_ID) {
    header = (
      <ContextMenuTrigger
        id={`context-menu-${gridId}`}
        holdToDisplay={-1}
        collect={collectMenu}
        onItemClick={handleClickMenu}
        type={'dimension-header'}
        axis={axis}
        dimensionId={id}
        caption={caption}
        index={index}
        style={{ width: 'inherit' }}
      >
        {header}
      </ContextMenuTrigger>
    );
  }
  header = (
    <div
      className="pivotgrid-header-inner"
      style={computedStyle}
      axis={axis}
      id={id}
      index={index}
      moveDimension={moveDimension}
    >
      {collapsedIcon}
      {header}
    </div>
  );
  // contextual menu
  // collectMenu = props => {
  //   const a = 3;
  //   return props;
  // };

  // drag and drop of dimension headers to move dimensions
  // dimension header -> drag source
  if (!isNullOrUndefined(index)) {
    header = connectDragSource(header);
    // header = connectDragSource(
    //   <div style={{ width: 'inherit' }}>{header}</div>
    // );
  }
  // dimension header -> drop target
  if (!isNullOrUndefined(moveDimension)) {
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
