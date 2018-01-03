import React, { Component } from "react";
import { Provider } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";

// import { AutoSizer } from "react-virtualized/dist/commonjs/AutoSizer";
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
import { createStore } from "redux";
import {
  GridWithoutStore,
  reducer,
  applyConfigToStore,
  actions
} from "./pivotGrid";
// custom functions and mock dataset
import {
  getMockDatasource,
  getMockDatasource2,
  getPromiseMockDatasource,
  getObservableError,
  basicConfig,
  basicConfig2
} from "./utils/mock";
import { configurationFunctions } from "./utils/configurationFunctions";
import { menuFunctions } from "./utils/menuFunctions";
import "./App.css";

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
    applyConfigToStore(store, basicConfig, configurationFunctions, data);
    this.state = {
      store,
      height: basicConfig.height,
      width: basicConfig.width
    };
  }

  pushData = () =>
    this.state.store.dispatch(actions.pushData(getMockDatasource(1, 10, 10)));
  onResize = (e, data) => {
    this.setState({ height: data.size.height, width: data.size.width });
  };
  render() {
    return (
      <Provider store={this.state.store}>
        <div>
          <ResizableBox height={this.state.height} width={this.state.width}>
            <GridWithoutStore
              id={0}
              menuFunctions={menuFunctions}
              configurationFunctions={configurationFunctions}
              height={this.state.height}
              width={this.state.width}
            />
          </ResizableBox>
          <div>
            <button onClick={this.pushData}>Push data</button>
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
          <div>Right click to sort, filter, remove or add a dimension.</div>
          <div> Column or row headers: </div>
          <div> Click: select children headers columns or rows.</div>
          <div>
            Collapse or expand button to hide or show children headers and
            recompute measures.
          </div>
          <div>
            Right and bottom handle drag and drop to resize rows or columns.
          </div>
        </div>
      </Provider>
    );
  }
}

export default DragDropContext(HTML5Backend)(PivotGridDemo);
