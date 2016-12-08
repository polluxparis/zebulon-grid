import React, { Component } from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import reducer from './reducers';
import PivotGrid from './containers/PivotGrid';
import hydrateStore from './hydrateStore';

class WrappedGrid extends Component {
  constructor(props) {
    super(props);
    const { datasource, config } = this.props;
    this.store = createStore(reducer);
    this.customFunctions = hydrateStore(this.store, config, datasource);
  }

  render() {
    return (
      <Provider store={this.store}>
        <PivotGrid {...this.props} customFunctions={this.customFunctions} />
      </Provider>
    );
  }
}

export default DragDropContext(HTML5Backend)(WrappedGrid);
