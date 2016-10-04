import React, { Component } from 'react'
import {ResizableBox} from 'react-resizable'

import {ChartConfiguration, BarChart, LineChart, GridConfiguration, Grid, Store} from '../index'

let pivotId = 1

class Main extends Component {

  constructor (props) {
    super(props)
    this.id = pivotId++

    const store = new Store(props.config, this.forceUpdate.bind(this))
    store.subscribe(props.datasource)
    this.state = {store}

    this.onDrilldown = this.onDrilldown.bind(this)
  }

  componentWillReceiveProps (newProps) {
    console.log('main received props', newProps)
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
      <div>
        <div>
          <ChartConfiguration store={store} />
          <ResizableBox height={600} width={800}>
            <BarChart store={store} />
          </ResizableBox>
        </div>
        <div>
          <ChartConfiguration store={store} />
          <ResizableBox height={600} width={800}>
            <LineChart store={store} />
          </ResizableBox>
        </div>
        <div>
          <GridConfiguration store={store} />
          <ResizableBox height={600} width={800}>
            <Grid store={store} />
          </ResizableBox>
        </div>
      </div>
    )
    // return (
    //   <MuiThemeProvider>
    //     <div>
    //       <Card expanded>
    //         <CardHeader
    //           title='Grid'
    //           expanded
    //           actAsExpander
    //           showExpandableButton
    //         />
    //         <Card expanded>
    //           <CardHeader
    //             title='Configuration'
    //             expanded
    //             actAsExpander
    //             showExpandableButton
    //           />
    //           <CardText expandable>
    //             <GridConfiguration store={store} />
    //           </CardText>
    //         </Card>
    //         <CardText expandable style={{height: 1000}}>
    //           <Grid store={store} drilldown={this.onDrilldown} />
    //         </CardText>
    //       </Card>
    //       <Card expanded>
    //         <CardHeader
    //           title='Chart'
    //           expanded
    //           actAsExpander
    //           showExpandableButton
    //         />
    //         <Card expanded>
    //           <CardHeader
    //             title='Configuration'
    //             expanded
    //             actAsExpander
    //             showExpandableButton
    //           />
    //           <CardText expandable>
    //             <ChartConfiguration store={store} />
    //           </CardText>
    //         </Card>
    //         <CardText expandable style={{height: 1000}}>
    //           <BarChart store={store} />
    //         </CardText>
    //       </Card>
    //       <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
    //     </div>
    //   </MuiThemeProvider>
    // )
  }
}

export default Main
