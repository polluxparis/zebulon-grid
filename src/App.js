import React, { Component } from 'react';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import 'react-virtualized/styles.css';
import 'react-resizable/css/styles.css';
import { ResizableBox } from 'react-resizable';
import { AutoSizer } from 'react-virtualized';

import GridConfiguration from './orb/containers/GridConfiguration';
import PivotGrid from './orb/containers/PivotGrid';

import './App.css';
import logo from './logo.svg';


class App extends Component {
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
            <GridConfiguration />
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
            <PivotGrid customFunctions={this.props.customFunctions} />
          </div>
        </div>
      </div>
    );
  }

}

export default DragDropContext(HTML5Backend)(App);
