import React, { Component } from "react";

import ZebulonGrid from "../pivotGrid";
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
import {
  getPromiseMockDatasource,
  basicConfig,
  getRandomMockDatasource
} from "./mock";
import { configurationFunctions } from "./configurationFunctions";
import { menuFunctions } from "./menuFunctions";

class ZebulonGridDemo extends Component {
  constructor(props) {
    super(props);
    this.options = [100, 100, 5];
    this.state = {
      focusCell: [],
      data: getPromiseMockDatasource(1, ...this.options),
      pushedData: [],
      sizes: {
        height: 600,
        width: 1000
      }
    };
    this.bigDataSet = false;
  }
  handleDataSetOption = () => {
    this.bigDataSet = !this.bigDataSet;
    this.options = this.bigDataSet ? [500, 400, 5] : [200, 40, 3];
    this.setState({ data: getPromiseMockDatasource(1, ...this.options) });
  };
  pushData = () => {
    this.setState({ pushedData: getRandomMockDatasource(30, ...this.options) });
  };
  customMeasure = () => {
    this.setState({ customMeasure: !this.state.customMeasure });
  };
  customSort = () => {
    this.setState({ customSort: !this.state.customSort });
  };
  customCellFunction = () => {
    this.setState({ customCellFunction: !this.state.customCellFunction });
  };
  onResize = (e, data) => {
    this.setState({
      sizes: {
        ...this.state.sizes,
        height: data.size.height,
        width: data.size.width
      }
    });
  };
  render() {
    return (
      <div style={{ fontFamily: "sans-serif" }}>
        <ResizableBox
          height={this.state.sizes.height}
          width={this.state.sizes.width}
          onResize={this.onResize}
        >
          <ZebulonGrid
            configuration={basicConfig}
            data={this.state.data}
            pushedData={this.state.pushedData}
            menuFunctions={menuFunctions}
            configurationFunctions={configurationFunctions}
            sizes={this.state.sizes}
            ref={ref => {
              this.grid = ref;
            }}
          />
        </ResizableBox>
        <div style={{ display: "flex", marginTop: ".5em" }}>
          <button style={{ marginRight: ".5em" }} onClick={this.pushData}>
            Push data
          </button>
          <button style={{ marginRight: ".5em" }} onClick={this.customMeasure}>
            {`${this.state.customMeasure ? "Remove" : "Add"} a custom measure`}
          </button>
          <button style={{ marginRight: ".5em" }} onClick={this.customSort}>
            {`${this.state.customSort
              ? "Remove"
              : "Add"} a custom sorting function`}
          </button>
          <button
            style={{ marginRight: ".5em" }}
            onClick={this.customCellFunction}
          >
            {`${this.state.customCellFunction
              ? "Remove"
              : "Add"} a custom cell function`}
          </button>
          <div>
            <input
              id="option"
              checked={this.bigDataSet}
              type="checkbox"
              onChange={this.handleDataSetOption}
            />
            <label htmlFor="option">Try a 1M rows dataset</label>
          </div>
        </div>
        <div
          style={{
            fontFamily: "sans-serif",
            fontSize: "smaller",
            marginTop: "2em"
          }}
        >
          <div>
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
