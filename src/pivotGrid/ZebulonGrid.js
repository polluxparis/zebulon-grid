import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";
import PivotGrid from "./containers/PivotGrid";
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
    // this.setState({
    //   store,
    //   configurationFunctions,
    //   menuFunctions: menuFunctions || defaultMenuFunctions,
    // });
  }
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
  // componentDidMount() {
  //   this.element = document.getElementById(this.props.id || 0);
  //   // this.element.addEventListener("copy", this.handleCopy);
  //   // this.element.addEventListener("keydown", this.handleKeyDown);
  // }
  render() {
    return (
      <div id={`pivotgrid-${this.props.id || 0}`}>
        <Provider store={this.store}>
          <PivotGrid
            menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
            key={`pivotgrid-${this.props.id || 0}`}
            gridId={`pivotgrid-${this.props.id || 0}`}
            isActive={this.props.isActive}
          />
        </Provider>
      </div>
    );
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
