import React, { Component } from 'react';
import { DragLayer } from 'react-dnd';

const collectDragLayer = monitor => ({
  item: monitor.getItem(),
  itemType: monitor.getItemType(),
  clientOffset: monitor.getClientOffset(),
  initialClientOffset: monitor.getInitialClientOffset()
});

const getItemPosition = (
  {
    clientOffset,
    item,
    gridRect
  }
) => {
  if (!clientOffset) {
    return {
      display: 'none'
    };
  }
  let x;
  let y;
  if (item.position === 'right') {
    y = 0;
    // x = Math.min(Math.max(clientOffset.x - gridRect.left, 0), gridRect.width);
    x = clientOffset.x - gridRect.left;
  } else {
    x = 0;
    // y = Math.min(Math.max(clientOffset.y - gridRect.top, 0), gridRect.height);
    y = clientOffset.y - gridRect.top;
  }

  const transform = `translate(${x}px, ${y}px)`;
  return {
    transform,
    WebkitTransform: transform
  };
};

class CustomDragLayer extends Component {
  render() {
    let height;
    let width;
    if (
      !this.props.item ||
      this.props.itemType.substring(0, 18) !== 'cell-resize-handle' ||
      this.props.item.gridId !== this.props.gridId
    ) {
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
    return (
      <div
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          zIndex: 100,
          left: 0,
          top: 0,
          width: '100%',
          height: '100%'
        }}
      >
        <div
          style={{
            position: 'absolute',
            height,
            width,
            backgroundColor: 'grey',
            ...getItemPosition(this.props)
          }}
        />
      </div>
    );
  }
}

export default DragLayer(collectDragLayer)(CustomDragLayer);
