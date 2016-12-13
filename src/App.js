import React, { Component } from 'react';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import 'react-virtualized/styles.css';
import 'react-resizable/css/styles.css';
// import { ResizableBox } from 'react-resizable';
// import { AutoSizer } from 'react-virtualized';

// import GridConfiguration from './orb/containers/GridConfiguration';
// import PivotGrid from './orb/containers/PivotGrid';
import { WrappedGrid } from './pivotGrid';
import './App.css';
import logo from './logo.svg';

import {
  getMockDatasource,
  // getObservableMockDatasource,
  basicConfig,
 } from './utils/mock';

let i = 0;

class App extends Component {
  constructor(props) {
    super(props);
    this.state = { data: props.data };

    this.addData = this.addData.bind(this);
    this.moveField = this.moveField.bind(this);
    this.toggleDatafield = this.toggleDatafield.bind(this);
  }

  addData() {
    this.grid.pushData([
      { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '0' },
    ]);
    // this.setState({ pushData: [
    //   // ...this.state.data,
    //   { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '0' },
    // ],
    // });
  }

  moveField() {
    if (i % 2) {
      this.grid.moveField('tutu', 'columns', 'rows', 1);
    } else {
      this.grid.moveField('tutu', 'rows', 'columns', 1);
    }
    i += 1;
  }

  toggleDatafield() {
    this.grid.toggleDatafield('amt');
  }

  render() {
    return (
      <div className="App">
        <div className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h2>Zebulon visualization components</h2>
        </div>
        <div className="App-body">
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
            {/* <GridConfiguration /> */}
            {/* <div className="Zoom-bar">
              <button
                className="Zoom-icon Zoom-icon-in"
                onClick={() => this.store.handleZoom(true)}
              >+</button>
              <button
                className="Zoom-icon Zoom-icon-out"
                onClick={() => this.store.handleZoom()}
              >-</button>
            </div> */}
            {/* <ResizableBox height={600} width={800}>
              <AutoSizer>
                {({ width, height }) =>
                  <Grid height={height} width={width} store={this.store} />
                }
              </AutoSizer>
            </ResizableBox> */}
            {/* <PivotGrid customFunctions={this.props.customFunctions} /> */}
            <div>
              <button onClick={this.addData}>Add data</button>
              <button onClick={this.moveField}>Move Field</button>
              <button onClick={this.toggleDatafield}>Toggle datafield</button>
            </div>
            <WrappedGrid
              ref={(ref) => { this.grid = ref.getDecoratedComponentInstance(); }}
              config={basicConfig}
              data={getMockDatasource(1, 100, 100)}
            />
          </div>
        </div>
      </div>
    );
  }

}

export default DragDropContext(HTML5Backend)(App);
