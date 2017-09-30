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

class ZebulonGridDemo extends Component {
  state = { focusCell: [] };
  data = getPromiseMockDatasource(1, 100, 100);

  setData = () => this.grid.setData(getMockDatasource(1, 3, 3));
  pushData = () => this.grid.pushData(getMockDatasource(1, 3, 3));
  render() {
    return (
      <div>
        <button onClick={this.setData}>Set new data</button>
        <button onClick={this.pushData}>Push data</button>
        <ResizableBox height={basicConfig.height} width={basicConfig.width}>
          <AutoSizer>
            {({ height, width }) => (
              <ZebulonGrid
                config={basicConfig}
                data={this.data}
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
        <div>
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
      </div>
    );
  }
}

export default ZebulonGridDemo;
