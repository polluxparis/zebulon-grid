import React, {Component} from 'react'
import {DragSource} from 'react-dnd'

const resizeHandleSpec = {
  beginDrag (props) {
    return {
      id: props.id,
      axis: props.axis,
      position: props.position,
      dimensionIsMeasure: props.dimensionIsMeasure,
      isOnDimensionHeader: props.isOnDimensionHeader,
      leafSubheaders: props.leafSubheaders,
      size: props.size
    }
  }
}

const sourceCollect = (connect, monitor) => ({
  connectDragSource: connect.dragSource(),
  connectDragPreview: connect.dragPreview(),
  isDragging: monitor.isDragging()
})

class ResizeHandle extends Component {
  render  () {
    const {position, size, connectDragSource} = this.props
    let handle
    if (position === 'right') {
      handle = <div style={{position: 'absolute', right: 0, width: 2, height: size, cursor: 'col-resize', opacity: 0}} />
        } else if (position === 'bottom') {
      handle = <div style={{position: 'absolute', bottom: 0, height: 2, width: size, cursor: 'row-resize', opacity: 0}} />
    } else {
      handle=null
    }
    return connectDragSource(handle)
  }
}
export default DragSource('cell-resize-handle', resizeHandleSpec, sourceCollect)(ResizeHandle)
