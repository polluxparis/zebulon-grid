import React from 'react'
import {DropTarget} from 'react-dnd'

const dropTarget = {
  drop (props, monitor, component) {
    const {id, axetype} = monitor.getItem()
    props.moveButton(id, axetype, props.axetype, props.position)
  }
}

const collect = (connect, monitor) => ({
  connectDropTarget: connect.dropTarget(),
  isOver: monitor.isOver(),
  isOverCurrent: monitor.isOver({shallow: true})
})

const DropIndicator = ({isFirst, isLast, isVertical, isOver, connectDropTarget}) => {
  let classname = `drp-indic${isVertical ? '-vertical' : ''}`

  if (isFirst) {
    classname += ' drp-indic-first'
  }

  if (isLast) {
    classname += ' drp-indic-last'
  }

  const style = {
    width: isOver ? '3em' : '0.5em',
    height: '100%'
  }

  return connectDropTarget(<div style={style} className={classname}></div>)
}

export default DropTarget('button', dropTarget, collect)(DropIndicator)
