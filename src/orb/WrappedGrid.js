import React, { Component } from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import reducer from './reducers';
import PivotGrid from './containers/PivotGrid';
import { pushData, setConfig, setConfigProperty, moveField, toggleDatafield } from './actions';
import { AxisType } from './Axis';

function initializeStore(store, config, datasource) {
  store.dispatch(pushData(datasource));

  store.dispatch(setConfig(config));

  store.dispatch(setConfigProperty(config, 'dataHeadersLocation', 'columns'));
  store.dispatch(setConfigProperty(config, 'height', 600));
  store.dispatch(setConfigProperty(config, 'width', 800));
  store.dispatch(setConfigProperty(config, 'cellHeight', 30));
  store.dispatch(setConfigProperty(config, 'cellWidth', 100));
  store.dispatch(setConfigProperty(config, 'zoom', 1));

  config.rows.forEach((fieldCaption, index) => {
    const fieldId = config.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(moveField(fieldId, -1, AxisType.ROWS, index));
  });
  config.columns.forEach((fieldCaption, index) => {
    const fieldId = config.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(moveField(fieldId, -1, AxisType.COLUMNS, index));
  });
  Object.values(config.fields)
  .filter((field) => {
    const state = store.getState();
    const rows = state.axis.rows;
    const columns = state.axis.columns;
    return !(rows.includes(field.id) || columns.includes(field.id));
  })
  .forEach((field, index) => {
    store.dispatch(moveField(field.id, -1, AxisType.FIELDS, index));
  });

  config.data.forEach((fieldCaption) => {
    const fieldId = config.datafields.find(field => field.caption === fieldCaption).id;
    store.dispatch(toggleDatafield(fieldId));
  });
}

class WrappedGrid extends Component {
  constructor(props) {
    super(props);
    const { datasource, config } = this.props;
    this.store = createStore(reducer);
    initializeStore(this.store, config, datasource);
  }

  render() {
    return (
      <Provider store={this.store}>
        <PivotGrid {...this.props} />
      </Provider>
    );
  }
}

export default DragDropContext(HTML5Backend)(WrappedGrid);
