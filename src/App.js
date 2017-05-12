import React, { Component } from 'react';
import PivotGridDemo from './PivotGrid.demo';
import WrappedGridDemo from './WrappedGrid.demo';

class App extends Component {
  constructor() {
    super();
    this.state = { demo: 'wrappedGrid' };
    this.handleChange = this.handleChange.bind(this);
  }
  handleChange(e) {
    this.setState({ demo: e.target.value });
  }
  render() {
    let grid = null;
    if (this.state.demo === 'pivotGrid') {
      grid = <PivotGridDemo />;
    } else if (this.state.demo === 'wrappedGrid') {
      grid = <WrappedGridDemo />;
    }
    return (
      <div>
        <select onChange={this.handleChange} defaultValue={this.state.demo}>
          <option value="pivotGrid">
            Pivot Grid
          </option>
          <option value="wrappedGrid">
            Wrapped Grid
          </option>
        </select>
        {grid}
      </div>
    );
  }
}

export default App;
