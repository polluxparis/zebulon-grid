import React, { Component } from 'react'
import {ResizableBox} from 'react-resizable'
import { DragDropContext } from 'react-dnd'
import HTML5Backend from 'react-dnd-html5-backend'

import {ChartConfiguration, Chart, GridConfiguration, Grid, Store} from './orb'

import './App.css'
import logo from './logo.svg'
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'


class App extends Component {
  constructor (props) {
    super(props)
    const store = new Store(props.config, this.forceUpdate.bind(this))
    this.state = {store}

    this.onDrilldown = this.onDrilldown.bind(this)
  }

  componentDidMount () {
    // Store is subscribed here because it triggers a forceUpdate
    // forceUpdate can only be called on a mounted Component
    this.state.store.subscribe(this.props.datasource)
  }

  componentWillReceiveProps (newProps) {
    const store = new Store(newProps.config, this.forceUpdate.bind(this))
    store.subscribe(newProps.datasource)
    this.setState({store})
  }

  sort (axetype, field) {
    this.state.store.sort(axetype, field)
  }

  toggleSubtotals (axetype) {
    this.state.store.toggleSubtotals(axetype)
  }

  toggleGrandtotal (axetype) {
    this.state.store.toggleGrandtotal(axetype)
  }

  onDrilldown (cell) {
    console.log('drilldown (prop)', cell)
  }

  render () {
    const {store} = this.state
    console.log(store)
    return (
      <div className='App'>
        <div className='App-header'>
          <img src={logo} className='App-logo' alt='logo' />
          <h2>Zebulon visualization components</h2>
        </div>
        <div className='App-body'>
          {/* <div>
            <ChartConfiguration store={store} />
            <ResizableBox height={600} width={800}>
              <Chart type='bar' store={store} />
            </ResizableBox>
          </div>
          <div>
            <ChartConfiguration store={store} />
            <ResizableBox height={600} width={800}>
              <Chart type='line' store={store} />
            </ResizableBox>
          </div>
          <div>
            <ChartConfiguration store={store} />
            <ResizableBox height={600} width={800}>
              <Chart type='area' store={store} />
            </ResizableBox>
          </div>
          <div>
          <ChartConfiguration store={store} />
          <ResizableBox height={600} width={800}>
          <Chart type='pie' store={store} />
          </ResizableBox>
          </div> */}
          <div>
            <GridConfiguration store={store} />
            <div className='Zoom-bar'>
              <span className='Zoom-icon Zoom-icon-in' onClick={() => store.handleZoom(true)}>+</span>
              <span className='Zoom-icon Zoom-icon-out' onClick={() => store.handleZoom()}>-</span>
            </div>
            <ResizableBox height={600} width={800}>
              <Grid store={store} />
            </ResizableBox>
          </div>
        </div>
      </div>
    )
  }

}

export default DragDropContext(HTML5Backend)(App)
