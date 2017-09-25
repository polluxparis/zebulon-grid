import React, { Component } from "react";
import PivotGridDemo from "./PivotGrid.demo";
import ZebulonGridDemo from "./ZebulonGrid.demo";

class App extends Component {
  constructor() {
    super();
    this.state = { demo: "pivotGrid" };
    this.handleChange = this.handleChange.bind(this);
  }
  handleChange(e) {
    this.setState({ demo: e.target.value });
  }
  render() {
    let grid = null;
    if (this.state.demo === "pivotGrid") {
      grid = <PivotGridDemo />;
    } else if (this.state.demo === "zebulonGrid") {
      grid = <ZebulonGridDemo />;
    }
    return (
      <div>
        <select onChange={this.handleChange} defaultValue={this.state.demo}>
          <option value="zebulonGrid">Zebulon Grid</option>
          <option value="pivotGrid">Grid without store</option>
        </select>
        {grid}
      </div>
    );
  }
}

export default App;
