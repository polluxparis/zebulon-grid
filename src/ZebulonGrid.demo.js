import React, { Component } from "react";

import ZebulonGrid from "./pivotGrid";
import { AutoSizer } from "react-virtualized/dist/commonjs/AutoSizer";
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
import {
  getMockDatasource,
  getMockDatasource2,
  getPromiseMockDatasource,
  basicConfig,
  basicConfig2
} from "./utils/mock";
import { configurationFunctions } from "./utils/configurationFunctions";
import { menuFunctions } from "./utils/menuFunctions";
// <button onClick={this.setData}>Set new data</button>
// <button onClick={this.pushData}>Push data</button>
class ZebulonGridDemo extends Component {
  constructor(props) {
    super(props);
    this.state = {
      focusCell: [],
      data: getPromiseMockDatasource(1, 100, 100, 3)
    };
    this.bigDataSet = false;
  }
  handleDataSetOption = () => {
    this.bigDataSet = !this.bigDataSet;
    const options = this.bigDataSet ? [1, 500, 400, 5] : [1, 100, 100, 3];
    this.setState({ data: getPromiseMockDatasource(...options) });
  };

  // setData = () => this.grid.setData(getMockDatasource(1, 3, 3));
  // pushData = () => this.grid.pushData(getMockDatasource(1, 3, 3));
  render() {
    return (
      <div>
        <ResizableBox height={basicConfig.height} width={basicConfig.width}>
          <AutoSizer>
            {({ height, width }) => (
              <ZebulonGrid
                config={basicConfig}
                data={this.state.data}
                menuFunctions={menuFunctions}
                configurationFunctions={configurationFunctions}
                height={height}
                width={width}
                ref={ref => {
                  this.grid = ref;
                }}
                /* eslint-disable no-console */
                drilldown={cellInfos => console.log("drilldown", cellInfos)}
                /* eslint-enable */
              />
            )}
          </AutoSizer>
        </ResizableBox>
        <div
          style={{
            fontFamily: "sans-serif",
            fontSize: "smaller",
            marginTop: "2em",
            width: 500
          }}
        >
          <div>
            <input
              id="option"
              checked={this.bigDataSet}
              type="checkbox"
              onChange={this.handleDataSetOption}
            />
            <label for="option">Try a 1M rows dataset</label>

            <div style={{ fontWeight: "bold" }}> Key board and mouse: </div>
            <div style={{ paddingLeft: "2em" }}>
              <div>
                Ctrl+A : select all, Ctrl + : zoom in, Ctrl - : zoom out.
              </div>
              <div>
                Up, Down, Right, Left, PageDown, PageUp, Home, End to move
                selection.
              </div>
              <div>
                +Shift to extend selection, +Alt to move in column axis for
                PageDown, PageUp, Home, End.
              </div>
              <div>Mouse down and move over to select a range.</div>
              <div>Mouse wheel to scroll. +Alt to scroll in column</div>
            </div>
            <div style={{ fontWeight: "bold" }}> Dimension headers: </div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Click: toggle sort direction.</div>
              <div>
                Drag and drop on an other dimension header to reorder or change
                dimension axis.
              </div>
              <div>
                Collapse or expand button to hide or show attribute dimensions
                (in Italic).
              </div>
              <div>
                Right and bottom handle: drag and drop to resize rows or columns
                headers.
              </div>
              <div>
                Right click context menu to sort, filter, expand and collapse
                all, remove or add a dimension.
              </div>
            </div>
            <div style={{ fontWeight: "bold" }}>
              Measure, column and row headers:
            </div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Click: select children headers columns or rows.</div>
              <div>
                Collapse or expand button to hide or show children headers and
                recompute measures.
              </div>
              <div>
                Right and bottom handle: drag and drop to resize rows or
                columns.
              </div>
              <div>
                Drag and drop measure on an other measure to reorder them, on a
                dimension header to change measure axis.
              </div>
              <div>
                Right click context menu on measures to remove, add a measure or
                change the axis.
              </div>
            </div>
            <div style={{ fontWeight: "bold" }}>Cells and ranges:</div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Ctrl+C to copy selected range</div>
              <div> Double click for drilldown main custom function</div>
              <div>
                Right click context menu to call cell, range or general custom
                functions and to filter any dimension.
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default ZebulonGridDemo;
