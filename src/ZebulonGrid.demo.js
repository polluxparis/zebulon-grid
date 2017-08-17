import React, { Component } from 'react';

import { ZebulonGrid } from './pivotGrid';

import {
  getMockDatasource,
  getMockDatasource2,
  getPromiseMockDatasource,
  basicConfig,
  basicConfig2
} from './utils/mock';
import { configFunctions } from './utils/configFunctions';
import { externalFunctions } from './utils/externalFunctions';

class ZebulonGridDemo extends Component {
  state = { focusCell: [] };
  data = getPromiseMockDatasource(1, 100, 100);

  setData = () => this.grid.setData(getMockDatasource(1, 3, 3));
  pushData = () => this.grid.pushData(getMockDatasource(1, 3, 3));
  setConfig = () =>
    this.grid.setConfig(basicConfig2, getMockDatasource2(1, 10, 10));
  render() {
    return (
      <div>
        <button onClick={this.setData}>set data</button>
        <button onClick={this.pushData}>push data</button>
        <button onClick={this.setConfig}>set config</button>
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
        <ZebulonGrid
          config={basicConfig}
          data={this.data}
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          ref={ref => {
            this.grid = ref;
          }}
          /* eslint-disable no-console */
          drilldown={cellInfos => console.log('drilldown', cellInfos)}
          /* eslint-enable */
        />
      </div>
    );
  }
}

export default ZebulonGridDemo;
