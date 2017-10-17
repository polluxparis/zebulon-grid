import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";

import PivotGrid from "./containers/PivotGrid";
import reducer from "./reducers";
import { applyConfigToStore, setData, pushData } from "./utils/configuration";
import * as actions from "./actions";

class ZebulonGrid extends Component {
  componentWillMount() {
    const { data, config, configurationFunctions, menuFunctions } = this.props;
    const store = createStore(
      reducer,
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
    );
    applyConfigToStore(store, config, configurationFunctions, data);
    this.setState({ store, configurationFunctions, menuFunctions });
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.config !== nextProps.config) {
      applyConfigToStore(
        this.state.store,
        nextProps.config,
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
ZebulonGrid.prototype["setConfig"] = function(config, data) {
  applyConfigToStore(
    this.state.store,
    config,
    this.props.configurationFunctions,
    data
  );
};
export default ZebulonGrid;
