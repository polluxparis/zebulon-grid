import React from 'react'
import {DropTarget} from 'react-dnd'

import DropIndicator from './DropIndicator'

const FieldList = ({buttons, axetype, connectDropTarget, isOverCurrent, isOver, moveButton}) => {
  const buttonComponents = buttons.map((button, index) => {
    if (index < buttons.length - 1) {
      return [
        <div>
          <DropIndicator isFirst={index === 0} position={index} axetype={axetype} moveButton={moveButton} />
        </div>,
        <div>
          {button}
        </div>
      ]
    } else {
      return [
        <div>
          <DropIndicator isFirst={index === 0} position={index} axetype={axetype} moveButton={moveButton} />
        </div>,
        <div>
          {button}
        </div>,
        <div>
          <DropIndicator isLast position={null} axetype={axetype} moveButton={moveButton} />
        </div>
      ]
    }
  })

  const highlight = buttons.length === 0 ? isOver : isOver && !isOverCurrent

  const style = {
    border: highlight ? 'dotted rgba(255, 192, 222, 0.7)' : 'dotted rgba(91, 192, 222, 0.7)',
    minHeight: '24px',
    minWidth: '67px',
    borderRadius: 10,
    display: 'flex'
  }

  return connectDropTarget(
    <div style={style}>
      {buttonComponents}
    </div>
 )
}

const dropTarget = {
  drop (props, monitor, component) {
    const {id, axetype} = monitor.getItem()
    props.moveButton(id, axetype, props.axetype, props.position)
  },
  canDrop (props, monitor) {
    return props.buttons.length === 0
  }
}

const collect = (connect, monitor) => ({
  connectDropTarget: connect.dropTarget(),
  isOver: monitor.isOver(),
  isOverCurrent: monitor.isOver({shallow: true})
})

export default DropTarget('button', dropTarget, collect)(FieldList)
