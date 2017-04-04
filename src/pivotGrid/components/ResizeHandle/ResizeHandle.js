import React from 'react';
import { DragSource } from 'react-dnd';

const resizeHandleSpec = {
  beginDrag(props) {
    return {
      id: props.id,
      axis: props.axis,
      gridId: props.gridId,
      position: props.position,
      leafSubheaders: props.leafSubheaders,
      previewSize: props.previewSize
    };
  }
};

const sourceCollect = (connect, monitor) => ({
  connectDragSource: connect.dragSource(),
  connectDragPreview: connect.dragPreview(),
  isDragging: monitor.isDragging()
});

const ResizeHandle = ({ position, connectDragSource }) => {
  let handle;
  if (position === 'right') {
    handle = (
      <div
        style={{
          position: 'absolute',
          right: 0,
          width: 4,
          height: 'inherit',
          cursor: 'col-resize',
          opacity: 0
        }}
      />
    );
  } else if (position === 'bottom') {
    handle = (
      <div
        style={{
          position: 'absolute',
          bottom: 0,
          height: 4,
          width: 'inherit',
          cursor: 'row-resize',
          opacity: 0
        }}
      />
    );
  } else {
    handle = null;
  }
  return connectDragSource(handle);
};

export default DragSource(
  props => `cell-resize-handle--${props.gridId}`,
  resizeHandleSpec,
  sourceCollect
)(ResizeHandle);
