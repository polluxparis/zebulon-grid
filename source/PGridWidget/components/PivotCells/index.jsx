'use strict'

import React, {Component} from 'react'
import {HeaderType} from '../../Cells'
import ReactDOM from 'react-dom'
import * as domUtils from '../../Utils.dom'
let _paddingLeft = null
let _borderLeft = null

export default class HeaderCellComp extends Component {

  constructor (props) {
    super(props)
    this._latestVisibleState = false
    this.expand = this.expand.bind(this)
    this.collapse = this.collapse.bind(this)
  }
  expand () {
    this.props.onToggle(this.props.cell, false)
  }
  collapse () {
    this.props.onToggle(this.props.cell, false)
  }
  updateCellInfos () {
    const node = ReactDOM.findDOMNode(this)
    const cell = this.props.cell
    node['__orb'] = node['__orb'] || {}

    if (!cell.visible()) {
      node['__orb']._visible = false
    } else {
      const cellContentNode = this.refs['cellContent']

      const propList = []
      const retPaddingLeft = _paddingLeft == null
      const retBorderLeft = !this.props.leftmost && _borderLeft == null
      const text = node.textContent || node['innerText']

      if (retPaddingLeft) {
        propList.push('padding-left')
      }

      if (retBorderLeft) {
        propList.push('border-left-width')
      }

      if (propList.length > 0) {
        const nodeStyle = domUtils.getStyle(node, propList, true)

        if (retPaddingLeft) {
          _paddingLeft = parseFloat(nodeStyle[0])
        }

        if (retBorderLeft) {
          _borderLeft = parseFloat(nodeStyle[retPaddingLeft ? 1 : 0])
        }
      }

      domUtils.removeClass(node, 'cell-hidden')

      node['__orb']._visible = true
      if (text !== node['__orb']._lastText || !node['__orb']._textWidth) {
        node['__orb']._lastText = text
        node['__orb']._textWidth = domUtils.getSize(cellContentNode).width
      }
      node['__orb']._colSpan = this.props.cell.hspan(true) || 1
      node['__orb']._rowSpan = this.props.cell.vspan(true) || 1
      node['__orb']._paddingLeft = _paddingLeft
      node['__orb']._paddingRight = _paddingLeft
      node['__orb']._borderLeftWidth = this.props.leftmost ? 0 : _borderLeft
      node['__orb']._borderRightWidth = 0
    }
  }
  componentDidMount () {
    // this.updateCellInfos()
  }
  componentDidUpdate () {
    // this.updateCellInfos()
  }
  shouldComponentUpdate (nextProps, nextState) {
    if (nextProps.cell && nextProps.cell === this.props.cell && !this._latestVisibleState && !nextProps.cell.visible()) {
      return false
    }
    return true
  }
  render () {
    const {cell} = this.props
    const divcontent = []
    let value
    let cellClick
    let headerPushed = false

    this._latestVisibleState = cell.visible()

    switch (cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        const isWrapper = cell.type === HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible
        const isSubtotal = cell.type === HeaderType.SUB_TOTAL && !cell.expanded
        if (isWrapper || isSubtotal) {
          headerPushed = true

          divcontent.push(
            <div key='header-value' ref='cellContent'>
              <div className='orb-tgl-btn'><div className={'orb-tgl-btn-' + (isWrapper ? 'down' : 'right')} onClick={(isWrapper ? this.collapse : this.expand)}></div></div>
              <div className='hdr-val'>{cell.value}</div>
            </div>
          )
        } else {
          value = (cell.value || '&#160') + (cell.type === HeaderType.SUB_TOTAL ? ' Total' : '')
        }
        break
      case 'cell-template-dataheader':
        value = cell.value.caption
        break
      default:
        break
    }

    if (!headerPushed) {
      let headerClassName
      if (cell.template !== 'cell-template-dataheader' && cell.type !== HeaderType.GRAND_TOTAL) {
        headerClassName = 'hdr-val'
      }
      // var style = {}
      // if (cell.subheaders && cell.subheaders.length>0){
      //   const transfo = `translate(0px,${this.props.scrollTop}px`
      //   style={transform: transfo}
      // }
      divcontent.push(<div key='cell-value' ref='cellContent' className={headerClassName}>{value}</div>)
    }

    return (
      <div
        style={{width: '100%', height: '100%'}}
        onDoubleClick={cellClick}
      >
          {divcontent}
      </div>
    )
  }
}
