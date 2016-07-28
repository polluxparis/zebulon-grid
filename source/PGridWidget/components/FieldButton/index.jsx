import React, { Component } from 'react'
import {DragSource} from 'react-dnd'

import * as utils from '../../Utils'
import FilterPanel from '../FilterPanel'

const filterImage = 'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAMUlEQVQYlWP4//9/I7GYgSzFDHgAVsX/sQCsirFpQFaI1c0wDegKB0AxeihQFs7EYAAT8WYwzt7jxgAAAABJRU5ErkJggg==) no-repeat 0px 0px'

class FieldButton extends Component {
  constructor (props) {
    super(props)
    this.state = {filtering: false}

    this.addFilterPanel = this.addFilterPanel.bind(this)
    this.removeFilterPanel = this.removeFilterPanel.bind(this)
    this.onMouseDown = this.onMouseDown.bind(this)
    this.onFilter = this.onFilter.bind(this)
  }

  addFilterPanel () {
    const {filtering} = this.state
    if (!filtering) {
      utils.addEventListener(document, 'mousedown', this.onMouseDown)
      this.setState({filtering: true})
    }
  }

  removeFilterPanel () {
    utils.removeEventListener(document, 'mousedown', this.onMouseDown)
    this.setState({filtering: false})
  }

  onMouseDown (e) {
    const filterPanelNode = document.getElementById('filter-panel')
    let target = e.target || e.srcElement
    while (target !== null) {
      if (target === filterPanelNode) {
        return true
      }
      target = target.parentNode
    }
    this.removeFilterPanel()
  }

  onFilter (all, operator, term, staticValue, excludeStatic) {
    const {store, field, axetype} = this.props
    store.applyFilter(field.name, axetype, all, operator, term, staticValue, excludeStatic)
    this.removeFilterPanel()
  }

  render () {
    const {field, store, axetype, connectDragSource, isDragging} = this.props
    const {filtering} = this.state
    const styles = {
      div: {
        width: isDragging ? 0 : '',
        backgroundColor: '#5bc0de',
        borderRadius: 4,
        cursor: 'default',
        opacity: isDragging ? 0 : 1,
        padding: '0.2em',
        marginTop: '0.2em',
        marginBottom: '0.2em',
        display: 'flex'
      },
      filterPlaceholder: {
        width: 11,
        height: 11,
        margin: '0.2em',
        marginLeft: '0.5em'
      },
      filterButton: {
        width: '100%',
        height: '100%',
        background: filterImage
      }
    }
    return connectDragSource(
      <div key={field.name} style={styles.div}>
        <div>
          {field.caption}
        </div>
        <div style={styles.filterPlaceholder}>
            {filtering
              ? <FilterPanel
                field={field}
                axetype={axetype}
                store={store}
                onFilter={this.onFilter}
                onCancel={() => this.removeFilterPanel()}
              />
              : <div
                onClick={this.addFilterPanel}
                style={styles.filterButton}
                >
              </div>
            }
        </div>
      </div>
    )
  }
}

const fieldSource = {
  beginDrag (props) {
    return {
      id: props.field.name,
      axetype: props.axetype
    }
  }
}

function collect (connect, monitor) {
  return {
    connectDragSource: connect.dragSource(),
    isDragging: monitor.isDragging()
  }
}

export default DragSource('button', fieldSource, collect)(FieldButton)
