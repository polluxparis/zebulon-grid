import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";

import PivotGrid from "../pivotGrid/containers/PivotGrid";
import reducer from "../pivotGrid/reducers";
import {
  applyConfigurationToStore,
  setData,
  pushData
} from "./utils/configuration";
import * as actions from "./actions";

class ZebulonGrid extends Component {
  componentWillMount() {
    const {
      data,
      configuration,
      configurationFunctions,
      menuFunctions
    } = this.props;
    const store = createStore(
      reducer,
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
    );
    applyConfigurationToStore(
      store,
      configuration,
      configurationFunctions,
      data
    );
    this.setState({ store, configurationFunctions, menuFunctions });
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.configuration !== nextProps.configuration) {
      applyConfigurationToStore(
        this.state.store,
        nextProps.configuration,
        this.state.configurationFunctions,
        nextProps.data
      );
    } else if (this.props.data !== nextProps.data) {
      setData(this.state.store, nextProps.data);
    } else if (
      this.props.pushedData !== nextProps.pushedData &&
      nextProps.pushedData.length
    ) {
      pushData(this.state.store, nextProps.pushedData);
    }
  }
  render() {
    const { store, menuFunctions } = this.state;
    return (
      <Provider store={store}>
        <PivotGrid
          menuFunctions={menuFunctions}
          id={this.props.id}
          drilldown={this.props.drilldown}
          focusCells={this.props.focusCells}
          height={this.props.height}
          width={this.props.width}
          ref={ref => (this.grid = ref)}
        />
      </Provider>
    );
  }
}
// expose all actions
Object.keys(actions).forEach(action => {
  /* eslint-disable func-names */
  ZebulonGrid.prototype[action] = function(...args) {
    this.state.store.dispatch(actions[action](...args));
  };
  /* eslint-enable */
});
ZebulonGrid.prototype["setData"] = function(data) {
  setData(this.state.store, data);
};
ZebulonGrid.prototype["getStore"] = function() {
  return this.state.store.getState();
};
ZebulonGrid.prototype["setConfiguration"] = function(configuration, data) {
  applyConfigurationToStore(
    this.state.store,
    configuration,
    this.props.configurationFunctions,
    data
  );
};
export default ZebulonGrid;
