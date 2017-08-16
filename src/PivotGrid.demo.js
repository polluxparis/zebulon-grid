import React, { Component } from 'react';
// import Checkbox from 'rc-checkbox';
import { Provider } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import { AutoSizer } from 'react-virtualized/dist/commonjs/AutoSizer';
// import Filter from './pivotGrid/containers/Filter';
import 'react-resizable/css/styles.css';
import { ResizableBox } from 'react-resizable';
import { createStore } from 'redux';
import PivotGrid, { reducer, setConfig } from './pivotGrid';
// import reducer from './reducers';
// import { MEASURE_ID } from './pivotGrid/constants';

// custom functions and mock dataset
import {
  getMockDatasource,
  getMockDatasource2,
  getPromiseMockDatasource,
  getObservableError,
  basicConfig,
  basicConfig2
} from './utils/mock';
import { configFunctions } from './utils/configFunctions';
import { externalFunctions } from './utils/externalFunctions';
import './App.css';
//
class PivotGridDemo extends Component {
  constructor(props) {
    super(props);

    const store = createStore(
      reducer,
      /* eslint-disable no-underscore-dangle */
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
      /* eslint-enable */
    );
    // _setData(store, data);
    const data = getMockDatasource(1, 100, 100);
    // const data = getObservableError();
    setConfig(store, basicConfig, configFunctions, data);
    this.setState({ store, configFunctions, externalFunctions });
    this.setState({ status: 'loading' });
    this.state = { store, focusCells: [] };
  }

  setData = () => this.grid.setData(getMockDatasource(1, 3, 3));
  pushData = () => this.grid.pushData(getMockDatasource(1, 10, 10));
  setConfig = () =>
    this.grid.setConfig(basicConfig2, getMockDatasource2(1, 10, 10));
  // pushData = () => this.grid.pushData(getMockDatasource(1, 10, 10))

  // focusCell = () => {
  //   const getCell = id => ({
  //     dimensions: [
  //       {
  //         cell: { caption: '0', id: '0' },
  //         dimension: { caption: 'Tutu', id: 'tutu' },
  //         axis: 'rows',
  //         index: 1
  //       },
  //       {
  //         cell: { caption: `toto ${id}`, id },
  //         dimension: { caption: 'Toto', id: 'toto' },
  //         axis: 'rows',
  //         index: 0
  //       },
  //       {
  //         cell: { caption: 'titi 0', id: 'titi 0' },
  //         dimension: { caption: 'Titi', id: 'titi' },
  //         axis: 'columns',
  //         index: 0
  //       }
  //     ],
  //     measure: {
  //       id: 'qty',
  //       axis: 'columns'
  //     }
  //   });
  //   k = (k + 1) % 3;
  //   if (k === 0) {
  //     this.setState({ focusCells: [] });
  //   } else {
  //     this.setState({
  //       focusCells: [...Array(k).keys()].map(id => getCell(id))
  //     });
  //   }
  // };

  // handleClickMenu = (e, data) => console.log(`Clicked on menu ${data.item}`);
  render() {
    return (
      <Provider store={this.state.store}>
        <div>
          <div>
            <button onClick={this.setData}>set data</button>
            <button onClick={this.pushData}>push data</button>
            <button onClick={this.setConfig}>set config</button>
          </div>
          <div>
            Ctrl A : select all, Ctrl + : zoom in, Ctrl - : zoom out, Shift Up,
            Down, Right, Left(, PageDown, PageUp, Home, End) to extend selection
          </div>
          <div> Dimension headers: </div>
          <div> Click: toggle sort direction</div>
          <div>
            Drag and drop on an other dimension header to reorder or change axis
            of a dimension.
          </div>
          <div>
            Collapse or expand button to hide or show attribute dimensions (in
            Italic).
          </div>
          <div>
            Right and bottom handle drag and drop to resize rows or columns
            headers.
          </div>
          <div>
            Collapse or expand button to hide or show attribute dimensions (in
            Italic).
          </div>
          <div>
            Right click to sort, filter, remove or add a dimension.
          </div>
          <div> Column or row headers: </div>
          <div> Click: select children headers columns or rows.</div>
          <div>
            Collapse or expand button to hide or show children headers and
            recompute measures.
          </div>
          <div>
            Right and bottom handle drag and drop to resize rows or columns.
          </div>
          <div>
            <ResizableBox height={basicConfig.height} width={basicConfig.width}>
              <AutoSizer>
                {({ height, width }) =>
                  <PivotGrid
                    id={0}
                    externalFunctions={externalFunctions}
                    configFunctions={configFunctions}
                    focusCells={this.state.focusCells}
                    height={height}
                    width={width}
                    drilldown={cell => {
                      console.log('drilldown', cell);
                    }}
                  />}
              </AutoSizer>
            </ResizableBox>
          </div>
        </div>
      </Provider>
    );
  }
}

export default DragDropContext(HTML5Backend)(PivotGridDemo);
