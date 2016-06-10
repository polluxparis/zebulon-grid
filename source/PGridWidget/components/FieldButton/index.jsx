'use strict'

import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import DragManager from '../../DragManager'
import FilterPanel from '../FilterPanel'
import * as utils from '../../Utils'
import * as domUtils from '../../Utils.dom'
let pbid = 0

export default class FieldButton extends Component {
  constructor (props) {
    super(props)
    this.state = {
      pbid: pbid + 1,
      // initial state, all zero.
      pos: { x: 0, y: 0 },
      startpos: { x: 0, y: 0 },
      mousedown: false,
      dragging: false
    }
    this.onFilterMouseDown = this.onFilterMouseDown.bind(this)
    this.onMouseDown = this.onMouseDown.bind(this)
    this.onMouseUp = this.onMouseUp.bind(this)
    this.onMouseMove = this.onMouseMove.bind(this)
  }

  onFilterMouseDown (e) {
    const {field, store, axetype} = this.props
    // left mouse button only
    if (e.button !== 0) return

    const filterButton = this.refs['filterButton']
    const filterButtonPos = domUtils.getOffset(filterButton)
    const filterContainer = document.createElement('div')

    filterContainer.style.position = 'fixed'
    filterContainer.style.top = `${filterButtonPos.y}px`
    filterContainer.style.left = `${filterButtonPos.x}px`

    document.body.appendChild(filterContainer)
    ReactDOM.render(
      <FilterPanel
        field={field}
        store={store}
        axetype={axetype}
      />, filterContainer)

    // prevent event bubbling (to prevent text selection while dragging for example)
    utils.stopPropagation(e)
    utils.preventDefault(e)
  }

  componentDidUpdate () {
    if (this.props.store.config.canMoveFields) {
      if (!this.state.mousedown) {
        // mouse not down, don't care about mouse up/move events.
        DragManager.setDragElement(null)
        utils.removeEventListener(document, 'mousemove', this.onMouseMove)
      } else if (this.state.mousedown) {
        // mouse down, interested by mouse up/move events.
        DragManager.setDragElement(this)
        utils.addEventListener(document, 'mousemove', this.onMouseMove)
      }
    }
  }
  componentDidMount () {
    // this.props.pivotTableComp.registerThemeChanged(this.updateClasses)
  }
  componentWillUnmount () {
    // this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses)
    utils.removeEventListener(document, 'mousemove', this.onMouseMove)
  }
  onMouseDown (e) {
    // drag/sort with left mouse button
    if (e.button !== 0) return

    if (e.ctrlKey) {
      this.props.store.toggleFieldExpansion(this.props.axetype, this.props.field)
    } else {
      const thispos = domUtils.getOffset(ReactDOM.findDOMNode(this))
      const mousePageXY = utils.getMousePageXY(e)

      // inform mousedown, save start pos
      this.setState({
        mousedown: true,
        mouseoffset: {
          x: thispos.x - mousePageXY.pageX,
          y: thispos.y - mousePageXY.pageY
        },
        startpos: {
          x: mousePageXY.pageX,
          y: mousePageXY.pageY
        }
      })
    }

    // prevent event bubbling (to prevent text selection while dragging for example)
    utils.stopPropagation(e)
    utils.preventDefault(e)
  }
  onMouseUp (e) {
    const isdragged = this.state.dragging

    this.setState({
      mousedown: false,
      dragging: false,
      size: null,
      pos: {
        x: 0,
        y: 0
      }
    })

    if (!e.ctrlKey && !isdragged) {
      // if button was not dragged, proceed as a click
      this.props.pivotTableComp.sort(this.props.axetype, this.props.field)
    }
  }
  onMouseMove (e) {
    // if the mouse is not down while moving, return (no drag)
    if (!this.props.store.config.canMoveFields || !this.state.mousedown) return

    let size = null
    const mousePageXY = utils.getMousePageXY(e)

    if (!this.state.dragging) {
      size = domUtils.getSize(ReactDOM.findDOMNode(this))
    } else {
      size = this.state.size
    }

    const newpos = {
      x: mousePageXY.pageX + this.state.mouseoffset.x,
      y: mousePageXY.pageY + this.state.mouseoffset.y
    }

    if (!this.state.dragging || newpos.x !== this.state.pos.x || newpos.y !== this.state.pos.y) {
      this.setState({
        dragging: true,
        size,
        pos: newpos
      })

      DragManager.elementMoved()
    }

    utils.stopPropagation(e)
    utils.preventDefault(e)
  }
  updateClasses () {
    ReactDOM.findDOMNode(this).className = this.props.store.config.theme.getButtonClasses().pivotButton
  }
  render () {
    const {field, store} = this.props
    const divstyle = {
      left: `${this.state.pos.x}px`,
      top: `${this.state.pos.y}px`,
      position: this.state.dragging ? 'fixed' : '',
      zIndex: 101,
      width: '',
      backgroundColor: '#5bc0de',
      borderRadius: 4,
      cursor: 'default'
    }

    if (this.state.size) {
      divstyle.width = `${this.state.size.width}px`
    }

    const sortDirectionClass = this.props.field.sort.order === 'asc'
      ? 'sort-asc'
      : (this.props.field.sort.order === 'desc'
        ? 'sort-desc'
        : '')
    const filterClass = (this.state.dragging ? '' : 'fltr-btn') + (store.isFieldFiltered(field.name) ? ' fltr-btn-active' : '')
    const filterImage = 'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAMUlEQVQYlWP4//9/I7GYgSzFDHgAVsX/sQCsirFpQFaI1c0wDegKB0AxeihQFs7EYAAT8WYwzt7jxgAAAABJRU5ErkJggg==) no-repeat 0px 0px'

    return (
      <div
        key={field.name}
        onMouseDown={this.onMouseDown}
        onMouseUp={this.onMouseUp}
        style={divstyle}>
        <table>
          <tbody>
            <tr>
              <td className='caption'>
                {field.caption}
              </td>
              <td>
                <div className={'sort-indicator ' + sortDirectionClass}></div>
              </td>
              <td className='filter'>
                <div
                  ref='filterButton'
                  className={filterClass}
                  onMouseDown={this.state.dragging ? null : this.onFilterMouseDown}
                  style={{ width: 11, height: 11, background: filterImage }}>
                </div>
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    )
  }
}
