import React, { Component } from 'react';

import WrappedGrid from './pivotGrid/WrappedGrid';

import { getMockDatasource, basicConfig } from './utils/mock';

let i = 0;
let k = 0;
class WrappedGridDemo extends Component {
  constructor() {
    super();
    this.addData = this.addData.bind(this);
    this.moveField = this.moveField.bind(this);
    this.sortField = this.sortField.bind(this);
    this.toggleDatafield = this.toggleDatafield.bind(this);
    this.zoomIn = this.zoomIn.bind(this);
    this.zoomOut = this.zoomOut.bind(this);
    this.toggleDatafieldAxis = this.toggleDatafieldAxis.bind(this);
    this.focusCell = this.focusCell.bind(this);

    this.state = { focusCell: [] };
    this.data = getMockDatasource(1, 100, 100);
  }
  addData() {
    this.grid.pushData([
      {
        toto: '0',
        toto_lb: 'toto 0',
        qty: 100,
        amt: 100,
        titi: 'titi 0',
        tutu: '0'
      }
    ]);
  }

  moveField() {
    if (i % 2) {
      this.grid.moveField('tutu', 'columns', 'rows', 1);
    } else {
      this.grid.moveField('tutu', 'rows', 'columns', 1);
    }
    i += 1;
  }
  sortField() {
    this.grid.changeSortOrder('toto');
  }

  toggleDatafield() {
    this.grid.toggleDatafield('amt');
  }
  toggleDatafieldAxis() {
    if (k % 2) {
      this.grid.setConfigProperty(
        { dataHeadersLocation: 'columns' },
        'dataHeadersLocation'
      );
    } else {
      this.grid.setConfigProperty(
        { dataHeadersLocation: 'rows' },
        'dataHeadersLocation'
      );
    }
    k += 1;
  }
  zoomIn() {
    this.grid.zoomIn();
  }
  zoomOut() {
    this.grid.zoomOut();
  }
  focusCell() {
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
  }

  render() {
    return (
      <div>
        <button onClick={this.addData}>Add data</button>
        <button onClick={this.moveField}>Move field</button>
        <button onClick={this.sortField}>Sort toto field</button>
        <button onClick={this.toggleDatafield}>Toggle datafield</button>
        <button onClick={this.zoomIn}>Zoom in</button>
        <button onClick={this.zoomOut}>Zoom out</button>
        <button onClick={this.focusCell}>Focus cells</button>
        <button onClick={this.toggleDatafieldAxis}>
          Toggle datafields axis
        </button>

        <WrappedGrid
          config={basicConfig}
          data={this.data}
          focusCells={this.state.focusCells}
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

export default WrappedGridDemo;
