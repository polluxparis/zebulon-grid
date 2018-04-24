import React, { Component } from "react";
import { DragLayer } from "react-dnd";

const collectDragLayer = monitor => {
  return {
    item: monitor.getItem(),
    itemType: monitor.getItemType(),
    clientOffset: monitor.getClientOffset(),
    initialClientOffset: monitor.getInitialClientOffset(),
    isDragging: monitor.isDragging()
  };
};

class CustomDragLayer extends Component {
  constructor() {
    super();

    this.state = {};
    this.getItemPosition = this.getItemPosition.bind(this);
  }

  getItemPosition() {
    const { clientOffset, item } = this.props;
    if (!clientOffset) {
      return {
        display: "none"
      };
    }
    let x;
    let y;
    const gridRect = this.element.getBoundingClientRect();
    if (item.position === "right") {
      y = 0;
      // Keep hint bar inside the grid
      x = Math.min(Math.max(clientOffset.x - gridRect.left, 0), gridRect.width);
    } else {
      x = 0;
      // Keep hint bar inside the grid
      y = Math.min(Math.max(clientOffset.y - gridRect.top, 0), gridRect.height);
    }

    // Translate the hint bar in the correct position starting from the top left corner of the grid
    // console.log("transform", x, y);
    const transform = `translate(${x}px, ${y}px)`;
    return {
      transform
    };
  }
  render() {
    let height;
    let width;
    let resizeBar;

    if (
      !this.props.item ||
      this.props.itemType.substring(0, 18) !== "cell-resize-handle" ||
      this.props.item.gridId !== this.props.gridId
    ) {
      resizeBar = null;
    } else {
      const { position, previewSize } = this.props.item;
      if (position === "right") {
        width = 2;
        height = previewSize;
      } else {
        width = previewSize;
        height = 2;
      }
      resizeBar = (
        <div
          style={{
            position: "absolute",
            height,
            width,
            backgroundColor: "grey",
            ...this.getItemPosition()
          }}
        />
      );
    }
    return (
      <div
        ref={element => {
          this.element = element;
        }}
        style={
          this.props.isDragging ? (
            {
              position: "absolute",
              pointerEvents: "none",
              zIndex: 100,
              left: 0,
              top: 0,
              width: "100%",
              height: "100%"
            }
          ) : (
            { display: "none" }
          )
        }
      >
        {resizeBar}
      </div>
    );
  }
}

export default DragLayer(collectDragLayer)(CustomDragLayer);
