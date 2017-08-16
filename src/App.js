import React, { Component } from 'react';
import PivotGridDemo from './PivotGrid.demo';
import WrappedGridDemo from './WrappedGrid.demo';

class App extends Component {
  constructor() {
    super();
    this.state = { demo: 'zebulonGrid' };
    this.handleChange = this.handleChange.bind(this);
  }
  handleChange(e) {
    this.setState({ demo: e.target.value });
  }
  render() {
    let grid = null;
    if (this.state.demo === 'pivotGrid') {
      grid = <PivotGridDemo />;
    } else if (this.state.demo === 'zebulonGrid') {
      grid = <WrappedGridDemo />;
    }
    return (
      <div>
        <select onChange={this.handleChange} defaultValue={this.state.demo}>

          <option value="zebulonGrid">
            Zebulon Grid
          </option>
          <option value="pivotGrid">
            Pivot Grid
          </option>
        </select>
        {grid}
      </div>
    );
  }
}

export default App;
