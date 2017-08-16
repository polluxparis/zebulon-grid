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
import PivotGrid, { reducer, hydrateStore, actions } from './pivotGrid';
import { MEASURE_ID } from './pivotGrid/constants';

// custom functions and mock dataset
import { getMockDatasource, basicConfig } from './utils/mock';
import { configFunctions } from './utils/configFunctions';
import { externalFunctions } from './utils/externalFunctions';
import './App.css';
//
let i = 0;
let j = 0;
let k = 0;

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

    const data = getMockDatasource(1, 100, 100);
    // this.customFunctions =
    hydrateStore(store, basicConfig, data, configFunctions);
    this.state = { store, focusCells: [] };
  }

  addData = () => {
    this.state.store.dispatch(
      actions.pushData([
        {
          toto: '666',
          toto_lb: 'toto 666',
          toto_0: 'aaaa',
          toto_1: 'bbbb',
          titi: 'titi 0',
          tutu: '1',
          qty: 100,
          amt: 100
        },
        {
          toto: '666',
          toto_lb: 'toto 666',
          toto_0: 'aaaa',
          toto_1: 'bbbb',
          titi: 'titi 4',
          qty: 201,
          amt: 302,
          tutu: '4'
        }
      ])
    );
  };

  moveDimension = () => {
    this.state.store.dispatch(actions.selectCell(null));
    if (i % 2) {
      this.state.store.dispatch(
        actions.moveDimension('tutu', 'columns', 'rows', 1)
      );
    } else {
      this.state.store.dispatch(
        actions.moveDimension('tutu', 'rows', 'columns', 1)
      );
    }
    i += 1;
  };
  toggleMeasureAxis = () => {
    this.state.store.dispatch(actions.selectCell(null));
    const store = this.state.store.getState();
    if (store.config.measureHeadersAxis === 'columns') {
      this.state.store.dispatch(
        actions.setConfigProperty(
          { measureHeadersAxis: 'rows' },
          'measureHeadersAxis',
          ''
        )
      );
      this.state.store.dispatch(
        actions.moveDimension(
          MEASURE_ID,
          'columns',
          'rows',
          store.axis.rows.length
        )
      );
    } else {
      this.state.store.dispatch(
        actions.setConfigProperty(
          { measureHeadersAxis: 'columns' },
          'measureHeadersAxis',
          ''
        )
      );
      this.state.store.dispatch(
        actions.moveDimension(
          MEASURE_ID,
          'rows',
          'columns',
          store.axis.columns.length
        )
      );
    }
  };

  sortDimension = () => {
    this.state.store.dispatch(actions.toggleSortOrder('titi'));
  };

  toggleMeasure = () => {
    this.state.store.dispatch(actions.selectCell({}));
    this.state.store.dispatch(actions.toggleMeasure('amt'));
  };

  zoomIn = () => {
    this.state.store.dispatch(actions.zoomIn());
  };

  zoomOut = () => {
    this.state.store.dispatch(actions.zoomOut());
  };

  toggleFilter = () => {
    if (j % 2) {
      this.state.store.dispatch(actions.deleteFilter('titi'));
    } else {
      this.state.store.dispatch(
        actions.addFilter('titi', 'in', null, ['titi 0', 'titi 2'], false)
      );
    }
    j += 1;
  };

  focusCell = () => {
    const getCell = id => ({
      dimensions: [
        {
          cell: { caption: '0', id: '0' },
          dimension: { caption: 'Tutu', id: 'tutu' },
          axis: 'rows',
          index: 1
        },
        {
          cell: { caption: `toto ${id}`, id },
          dimension: { caption: 'Toto', id: 'toto' },
          axis: 'rows',
          index: 0
        },
        {
          cell: { caption: 'titi 0', id: 'titi 0' },
          dimension: { caption: 'Titi', id: 'titi' },
          axis: 'columns',
          index: 0
        }
      ],
      measure: {
        id: 'qty',
        axis: 'columns'
      }
    });
    k = (k + 1) % 3;
    if (k === 0) {
      this.setState({ focusCells: [] });
    } else {
      this.setState({
        focusCells: [...Array(k).keys()].map(id => getCell(id))
      });
    }
  };

  handleClickMenu = (e, data) => console.log(`Clicked on menu ${data.item}`);
  render() {
    return (
      <Provider store={this.state.store}>
        <div>
          <div>
            <button onClick={this.addData}>Add data</button>
            <button onClick={this.toggleFilter}>Toggle filter</button>
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
//
// <button onClick={this.zoomIn}>Zoom in</button>
//        <button onClick={this.zoomOut}>Zoom out</button>
// <button onClick={this.zoomOut}>Zoom out</button>
// <button onClick={this.focusCell}>Focus cells</button>
//    <button onClick={this.toggleMeasureAxis}>Move measures</button>
// <button onClick={this.toggleMeasure}>Toggle measure</button>
export default DragDropContext(HTML5Backend)(PivotGridDemo);
