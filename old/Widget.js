'use strict'

import React from 'react'
import ReactDOM from 'react-dom'

import PivotTableComponent from './components/Main'

/**
Wrapper for PivotTableComponent
so that programs using this package do not need to know anything about React
**/
class PGridWidget {

  constructor (config, datasource) {
    this.config = config
    this.datasource = datasource
  }

  render (elem) {
    this.DOMNode = elem
    ReactDOM.render(React.createElement(PivotTableComponent, {config: this.config, datasource: this.datasource}), elem)
  }

  unmount () {
    ReactDOM.unmountComponentAtNode(this.DOMNode)
  }
}

export default PGridWidget
