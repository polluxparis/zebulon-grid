import React, { Component } from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import PivotGrid from './containers/PivotGrid';
import reducer from './reducers';
import { setConfig, setData } from './utils/setConfig';
import * as actions from './actions';

class ZebulonGrid extends Component {
  componentWillMount() {
    const { data, config, configFunctions, externalFunctions } = this.props;
    const store = createStore(reducer);
    // _setData(store, data);
    setConfig(store, config, configFunctions, data);
    this.setState({ store, configFunctions, externalFunctions });
    this.setState({ status: 'loading' });
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.config !== nextProps.config) {
      setConfig(
        this.state.store,
        nextProps.config,
        [],
        this.state.configFunctions
      );
      // this.setState({ customFunctions });
    }
    // if (this.props.data !== nextProps.data) {
    //   this.state.store.dispatch(actions.setData(nextProps.data));
    // }
  }
  // setData = data => _setData(this.state.store, data);
  // setConfig = (config, data) =>
  //   setConfig(this.state.store, config, this.props.configFunctions, data);

  render() {
    const { store, externalFunctions } = this.state;

    return (
      <Provider store={store}>
        <PivotGrid
          externalFunctions={externalFunctions}
          id={this.props.id}
          drilldown={this.props.drilldown}
          focusCells={this.props.focusCells}
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
ZebulonGrid.prototype['setData'] = function(data) {
  setData(this.state.store, data);
};
ZebulonGrid.prototype['setConfig'] = function(config, data) {
  setConfig(this.state.store, config, this.props.configFunctions, data);
};
export default ZebulonGrid;
