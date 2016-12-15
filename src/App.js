import React, { Component } from 'react';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import 'react-virtualized/styles.css';
import 'react-resizable/css/styles.css';
import { ResizableBox } from 'react-resizable';
import { AutoSizer } from 'react-virtualized';

import { PivotGrid } from './pivotGrid';
import './App.css';

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
      <div>
        {/* <PivotGrid customFunctions={this.props.customFunctions} /> */}
        <ResizableBox height={this.props.config.height} width={this.props.config.width}>
          <AutoSizer>
            {({ height, width }) => (
              <PivotGrid
                customFunctions={this.props.customFunctions}
                height={height}
                width={width}
              />)
            }
          </AutoSizer>
        </ResizableBox>
        {/* <div>
          <button onClick={this.addData}>Add data</button>
          <button onClick={this.moveField}>Move Field</button>
          <button onClick={this.toggleDatafield}>Toggle datafield</button>
        </div>
        <WrappedGrid
          ref={(ref) => { this.grid = ref.getDecoratedComponentInstance(); }}
          config={basicConfig}
          data={getMockDatasource(1, 100, 100)}
        /> */}
      </div>
    );
  }

}

export default DragDropContext(HTML5Backend)(App);
