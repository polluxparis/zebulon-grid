import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";
import PivotGrid from "./containers/PivotGrid";
import Chart from "./containers/Chart";
import Configuration from "./containers/Configuration";
import reducer from "./reducers";
import "./index.css";
import {
  defaultMenuFunctions,
  defaultSizes,
  applySizesToStore,
  applyConfigurationToStore,
  setData,
  pushData
} from "./utils/configuration";
import * as actions from "./actions";

class ZebulonGrid extends Component {
  componentWillMount() {
    const { data, configuration, configurationFunctions, sizes } = this.props;
    this.store = createStore(
      reducer,
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
    );
    applySizesToStore(this.store, { ...defaultSizes, ...sizes });
    applyConfigurationToStore(
      this.store,
      configuration,
      configurationFunctions,
      data
    );
  }
  componentDidMount() {
    document.addEventListener("copy", this.handleCopy);
    document.addEventListener("paste", this.handlePaste);
    document.addEventListener("keydown", this.handleKeyDown);
  }
  componentDidUnMount() {
    document.removeEventListener("copy", this.handleCopy);
    document.removeEventListener("paste", this.handlePaste);
    document.removeEventListener("keydown", this.handleKeyDown);
  }
  handleKeyDown = e => {
    if (
      !e.defaultPrevented &&
      this.display.handleKeyDown &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handleKeyDown(e);
    }
  };
  handleCopy = e => {
    if (
      !e.defaultPrevented &&
      this.display.handleCopy &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handleCopy(e);
    }
  };
  handlePaste = e => {
    if (
      !e.defaultPrevented &&
      this.display.handlePaste &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handlePaste(e);
    }
  };
  componentWillReceiveProps(nextProps) {
    const {
      data,
      configuration,
      configurationFunctions,
      pushedData,
      sizes
    } = nextProps;
    // this.sizes = { ...defaultSizes, ...sizes };

    if (sizes !== this.props.sizes) {
      applySizesToStore(this.store, sizes);
    }
    if (this.props.configuration !== configuration) {
      applyConfigurationToStore(
        this.store,
        configuration,
        configurationFunctions,
        data
      );
    } else if (this.props.data !== data) {
      setData(this.store, data);
    } else if (this.props.pushedData !== pushedData && pushedData.length) {
      pushData(this.store, pushedData);
    }
  }

  render() {
    this.displayId = `pivotgrid-${this.props.id || 0}`;
    let div = (
      <div>
        <Provider store={this.store}>
          <PivotGrid
            id={this.displayId}
            menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
            key={this.displayId}
            gridId={this.displayId}
            isActive={this.props.isActive}
            getRef={ref => (this.display = ref)}
          />
        </Provider>
      </div>
    );

    if (this.props.display === "configuration") {
      this.displayId = `configuration-${this.props.id || 0}`;
      div = (
        <div>
          <Provider store={this.store}>
            <Configuration
              id={this.displayId}
              menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
              configurationFunctions={this.props.configurationFunctions}
              configuration={this.props.configuration}
              key={this.displayId}
              configurationId={this.displayId}
              isActive={this.props.isActive}
              getRef={ref => (this.display = ref)}
            />
          </Provider>
        </div>
      );
    } else if (this.props.display === "chart") {
      this.displayId = `chart-${this.props.id || 0}`;
      div = (
        <div>
          <Provider store={this.store}>
            <Chart
              id={this.displayId}
              menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
              key={this.displayId}
              configurationId={this.displayId}
              isActive={this.props.isActive}
              getRef={ref => (this.display = ref)}
            />
          </Provider>
        </div>
      );
    }
    return div;
  }
}
// expose all actions
Object.keys(actions).forEach(action => {
  /* eslint-disable func-names */
  ZebulonGrid.prototype[action] = function(...args) {
    this.store.dispatch(actions[action](...args));
  };
  /* eslint-enable */
});
ZebulonGrid.prototype["setData"] = function(data) {
  setData(this.store, data);
};
ZebulonGrid.prototype["getStore"] = function() {
  return this.store.getState();
};
ZebulonGrid.prototype["setConfiguration"] = function(configuration, data) {
  applyConfigurationToStore(
    this.store,
    configuration,
    this.props.configurationFunctions,
    data
  );
};
ZebulonGrid.prototype["setSizes"] = function(sizes) {
  applySizesToStore(this.store, { ...defaultSizes, ...sizes });
};
export default ZebulonGrid;
