import React, { Component } from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import reducer from './reducers';
import PivotGrid from './containers/PivotGrid';
import hydrateStore from './hydrateStore';
import * as actions from './actions';

class WrappedGrid extends Component {
  componentWillMount() {
    const { data, config } = this.props;
    const store = createStore(reducer);
    const customFunctions = hydrateStore(store, config, data);
    this.setState({ store, customFunctions });
  }

  componentWillReceiveProps(nextProps) {
    if (this.props.config !== nextProps.config) {
      const customFunctions = hydrateStore(
        this.state.store,
        nextProps.config,
        []
      );
      this.setState({ customFunctions });
    }
    if (this.props.data !== nextProps.data) {
      this.state.store.dispatch(actions.setData(nextProps.data));
    }
  }

  render() {
    const { store, customFunctions } = this.state;
    return (
      <Provider store={store}>
        <PivotGrid
          customFunctions={customFunctions}
          id={this.props.id}
          drilldown={this.props.drilldown}
          focusCells={this.props.focusCells}
        />
      </Provider>
    );
  }
}

Object.keys(actions).forEach(action => {
  /* eslint-disable func-names */
  WrappedGrid.prototype[action] = function(...args) {
    this.state.store.dispatch(actions[action](...args));
  };
  /* eslint-enable */
});

export default WrappedGrid;
