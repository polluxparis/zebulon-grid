import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';

import Perf from 'react-addons-perf'; // ES6
import { TestRunner } from 'fps-measurer';

import './orb/index.css';
import reducer from './orb/reducers';
import { pushData, setConfig, setConfigProperty, moveField, toggleDatafield } from './orb/actions';

import createScrollingTestCase from './fpsTests/tests';

import App from './App';

import {
  getMockDatasource,
  // getObservableMockDatasource,
  basicConfig,
 } from './utils/mock';

import { AxisType } from './orb/Axis';

if (process.env.NODE_ENV !== 'production') {
  window.Perf = Perf;
}

function initializeStore(store) {
  const datasource = getMockDatasource(1, 20, 20);
  store.dispatch(pushData(datasource));

  store.dispatch(setConfig(basicConfig));

  store.dispatch(setConfigProperty(basicConfig, 'dataHeadersLocation', 'columns'));
  store.dispatch(setConfigProperty(basicConfig, 'height', 600));
  store.dispatch(setConfigProperty(basicConfig, 'width', 800));
  store.dispatch(setConfigProperty(basicConfig, 'cellHeight', 30));
  store.dispatch(setConfigProperty(basicConfig, 'cellWidth', 100));
  store.dispatch(setConfigProperty(basicConfig, 'zoom', 1));

  basicConfig.rows.forEach((fieldCaption, index) => {
    const fieldId = basicConfig.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(moveField(fieldId, AxisType.FIELD, AxisType.ROWS, index));
  });
  basicConfig.columns.forEach((fieldCaption, index) => {
    const fieldId = basicConfig.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(moveField(fieldId, AxisType.FIELD, AxisType.COLUMNS, index));
  });
  Object.values(basicConfig.fields)
  .filter((field) => {
    const state = store.getState();
    const rows = state.axis.rows;
    const columns = state.axis.columns;
    return !(rows.includes(field.id) || columns.includes(field.id));
  })
  .forEach((field, index) => {
    store.dispatch(moveField(field.id, AxisType.FIELDS, AxisType.FIELDS, index));
  });

  basicConfig.data.forEach((fieldCaption) => {
    const fieldId = basicConfig.datafields.find(field => field.caption === fieldCaption).id;
    store.dispatch(toggleDatafield(fieldId));
  });
}

const store = createStore(reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__());

initializeStore(store);

ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root'));

if (process.env.REACT_APP_ORB_ENV === 'fps-test') {
  const testCase = createScrollingTestCase(document.getElementsByClassName('OrbGrid-data-cells')[0]);
  const testRunner = new TestRunner(testCase, 5);

  const btn = document.createElement('button');
  btn.innerText = 'Run FPS test';
  document.body.prepend(btn);
  btn.addEventListener('click', () => {
    if (testRunner.isRunning()) {
      testRunner.stop();
    } else {
      testRunner.start();
    }
  });
}
