import React, { Component } from "react";
import { Provider } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";
import "react-virtualized/styles.css";
import { AutoSizer } from "react-virtualized/dist/commonjs/AutoSizer";
/* eslint-disable import/no-extraneous-dependencies*/
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
/* eslint-enable */

import { createStore } from "redux";
import PivotGrid, { reducer, hydrateStore, actions } from "./pivotGrid";
import { getMockDatasource, basicConfig } from "./utils/mock";

import "./App.css";

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

    const data = getMockDatasource(1, 20, 10);
    this.customFunctions = hydrateStore(store, basicConfig, data);
    this.state = { store, focusCells: [] };

    this.addData = this.addData.bind(this);
    this.moveDimension = this.moveDimension.bind(this);
    this.sortDimension = this.sortDimension.bind(this);
    this.toggleMeasure = this.toggleMeasure.bind(this);
    this.zoomIn = this.zoomIn.bind(this);
    this.zoomOut = this.zoomOut.bind(this);
    this.toggleFilter = this.toggleFilter.bind(this);
    this.focusCell = this.focusCell.bind(this);
  }

  addData() {
    this.state.store.dispatch(
      actions.pushData([
        {
          toto: "666",
          toto_lb: "",
          qty: 100,
          amt: 100,
          titi: "titi 0",
          tutu: "0"
        }
      ])
    );
  }

  moveDimension() {
    if (i % 2) {
      this.state.store.dispatch(
        actions.moveDimension("tutu", "columns", "rows", 1)
      );
    } else {
      this.state.store.dispatch(
        actions.moveDimension("tutu", "rows", "columns", 1)
      );
    }
    i += 1;
  }

  sortDimension() {
    this.state.store.dispatch(actions.changeSortOrder("titi"));
  }

  toggleMeasure() {
    this.state.store.dispatch(actions.toggleMeasure("amt"));
  }

  zoomIn() {
    this.state.store.dispatch(actions.zoomIn());
  }

  zoomOut() {
    this.state.store.dispatch(actions.zoomOut());
  }

  toggleFilter() {
    if (j % 2) {
      this.state.store.dispatch(actions.deleteFilter("titi"));
    } else {
      this.state.store.dispatch(
        actions.addFilter("titi", "in", null, ["titi 0", "titi 2"], false)
      );
    }
    j += 1;
  }

  focusCell() {
    const getCell = id => ({
      dimensions: [
        {
          cell: { caption: "0", id: "0" },
          dimension: { caption: "Tutu", id: "tutu" },
          axis: "rows",
          index: 1
        },
        {
          cell: { caption: `toto ${id}`, id },
          dimension: { caption: "Toto", id: "toto" },
          axis: "rows",
          index: 0
        },
        {
          cell: { caption: "titi 0", id: "titi 0" },
          dimension: { caption: "Titi", id: "titi" },
          axis: "columns",
          index: 0
        }
      ],
      measure: {
        id: "qty",
        axis: "columns"
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
  }

  render() {
    return (
      <Provider store={this.state.store}>
        <div>
          <div>
            <button onClick={this.addData}>Add data</button>
            <button onClick={this.moveDimension}>Move dimension</button>
            <button onClick={this.sortDimension}>Sort titi dimension</button>
            <button onClick={this.toggleMeasure}>Toggle measure</button>
            <button onClick={this.zoomIn}>Zoom in</button>
            <button onClick={this.zoomOut}>Zoom out</button>
            <button onClick={this.toggleFilter}>Toggle filter</button>
            <button onClick={this.focusCell}>Focus cells</button>
          </div>
          <div>
            <ResizableBox height={basicConfig.height} width={basicConfig.width}>
              <AutoSizer>
                {({ height, width }) =>
                  <PivotGrid
                    id={0}
                    customFunctions={this.customFunctions}
                    focusCells={this.state.focusCells}
                    height={height}
                    width={width}
                    drilldown={cell => {
                      console.log("drilldown", cell);
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
