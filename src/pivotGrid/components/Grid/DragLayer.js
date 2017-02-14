import React, { Component } from 'react';
import { DragLayer } from 'react-dnd';

const collectDragLayer = monitor => ({
  item: monitor.getItem(),
  itemType: monitor.getItemType(),
  initialOffset: monitor.getInitialSourceClientOffset(),
  currentOffset: monitor.getSourceClientOffset(),
  isDragging: monitor.isDragging(),
});

const getItemPosition = ({ initialOffset, currentOffset, item }) => {
  if (!initialOffset || !currentOffset) {
    return {
      display: 'none',
    };
  }

  let { x, y } = currentOffset;
  if (item.position === 'right') {
    y = initialOffset.y - item.previewOffset;
  } else {
    x = initialOffset.x - item.previewOffset;
  }

  const transform = `translate(${x}px, ${y}px)`;
  return {
    transform,
    WebkitTransform: transform,
  };
};

class CustomDragLayer extends Component {
  render() {
    let height;
    let width;
    if (!this.props.item || this.props.itemType !== 'cell-resize-handle') {
      return null;
    }
    const { position, previewSize } = this.props.item;
    if (position === 'right') {
      width = 2;
      height = previewSize;
    } else {
      width = previewSize;
      height = 2;
    }
    return (<div
      style={{
          position: 'fixed',
          pointerEvents: 'none',
          zIndex: 100,
          left: 0,
          top: 0,
          width: '100%',
          height: '100%',
        }}
    ><div
      style={{
          height,
          width,
          backgroundColor: 'grey',
          ...getItemPosition(this.props),
        }}
    /></div>);
  }
}

export default DragLayer(collectDragLayer)(CustomDragLayer);
