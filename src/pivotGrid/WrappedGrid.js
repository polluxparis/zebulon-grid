import React, { Component } from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import reducer from './reducers';
import PivotGrid from './containers/PivotGrid';
import { _setConfig, _setData } from './setConfig';
import * as actions from './actions';

class WrappedGrid extends Component {
  componentWillMount() {
    const { data, config, configFunctions, externalFunctions } = this.props;
    const store = createStore(reducer);
    // _setData(store, data);
    _setConfig(store, config, configFunctions, data);
    this.setState({ store, configFunctions, externalFunctions });
    this.setState({ status: 'loading' });
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.config !== nextProps.config) {
      _setConfig(
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
  //   _setConfig(this.state.store, config, this.props.configFunctions, data);

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
  WrappedGrid.prototype[action] = function(...args) {
    this.state.store.dispatch(actions[action](...args));
  };
  /* eslint-enable */
});
WrappedGrid.prototype['setData'] = function(data) {
  _setData(this.state.store, data);
};
WrappedGrid.prototype['setConfig'] = function(config, data) {
  _setConfig(this.state.store, config, this.props.configFunctions, data);
};
export default WrappedGrid;
