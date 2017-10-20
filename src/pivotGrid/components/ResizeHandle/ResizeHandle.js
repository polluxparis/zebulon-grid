import React from "react";
import { DragSource } from "react-dnd";

const resizeHandleSpec = {
  beginDrag(props) {
    return props;
  }
};

const sourceCollect = (connect, monitor) => {
  return {
    connectDragSource: connect.dragSource(),
    connectDragPreview: connect.dragPreview(),
    isDragging: monitor.isDragging()
  };
};

const ResizeHandle = ({ position, size, connectDragSource }) => {
  let handle;
  if (position === "right") {
    handle = (
      <div
        style={{
          position: "absolute",
          right: 0,
          width: 4,
          height: size,
          cursor: "col-resize",
          opacity: 0
        }}
      />
    );
  } else if (position === "bottom") {
    handle = (
      <div
        style={{
          position: "absolute",
          bottom: 0,
          height: 4,
          width: size,
          cursor: "row-resize",
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
