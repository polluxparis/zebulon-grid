import React, {Component} from 'react'
import {DragLayer} from 'react-dnd'

const collectDragLayer = monitor => ({
  item: monitor.getItem(),
  itemType: monitor.getItemType(),
  initialOffset: monitor.getInitialSourceClientOffset(),
  currentOffset: monitor.getSourceClientOffset(),
  isDragging: monitor.isDragging()
})

const getItemPosition = ({initialOffset, currentOffset, item}) => {
    if (!initialOffset || !currentOffset) {
      return {
        display: 'none'
      }
    }

    let { x, y } = currentOffset
    if (item.position)
    if (item.position === 'right') {
      y = initialOffset.y
    } else {
      x = initialOffset.x
    }

    const transform = `translate(${x}px, ${y}px)`
    return {
      transform: transform,
      WebkitTransform: transform
    }
  }

class CustomDragLayer extends Component {
  render () {
    let height, width
    if (!this.props.item || this.props.itemType !== 'cell-resize-handle' ) { return null }
    const {position, size} = this.props.item
    if (position === 'right') {
      width = 2
      height = size
    } else {
      width = size
      height = 2
    }
    return <div style={{
      position: 'fixed',
      pointerEvents: 'none',
      zIndex: 100,
      left: 0,
      top: 0,
      width: '100%',
      height: '100%'
    }}><div style={{height, width, backgroundColor: 'grey', ...getItemPosition(this.props)}} /></div>
  }
}

export default DragLayer(collectDragLayer)(CustomDragLayer)
