import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { createStore } from 'redux';
import reducer from './orb/reducers';
import { pushData, setConfig, setConfigProperty, addField, toggleDatafield } from './orb/actions';

import { TestRunner } from 'fps-measurer';
import createScrollingTestCase from './fpsTests/tests';

import { Config } from './orb/Config';

import App from './App';
import Store from './orb/stores/Store';

import {
  getMockDatasource,
  getObservableMockDatasource,
  basicConfig,
 } from './utils/mock';

 import { AxisType } from './orb/Axis';
 import Perf from 'react-addons-perf'; // ES6

if (process.env.NODE_ENV !== 'production') {
  window.Perf = Perf;
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

function initializeStore(store) {
  const datasource = getMockDatasource(1, 10, 10);
  store.dispatch(pushData(datasource));

  // const datasource = getObservableMockDatasource(1100);
  // datasource.subscribe(data => store.dispatch(pushData(data)));
  // console.log(new Config(basicConfig));
  // console.log(new Store(basicConfig));
  store.dispatch(setConfig(basicConfig));

  store.dispatch(setConfigProperty(basicConfig, 'dataHeadersLocation', 'columns'));
  store.dispatch(setConfigProperty(basicConfig, 'height', 600));
  store.dispatch(setConfigProperty(basicConfig, 'width', 800));
  store.dispatch(setConfigProperty(basicConfig, 'cellHeight', 30));
  store.dispatch(setConfigProperty(basicConfig, 'cellWidth', 100));
  store.dispatch(setConfigProperty(basicConfig, 'zoom', 1));

  basicConfig.rows.forEach((fieldCaption, index) => {
    const fieldId = basicConfig.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(addField(fieldId, AxisType.ROWS, index));
  });
  basicConfig.columns.forEach((fieldCaption, index) => {
    const fieldId = basicConfig.fields.find(field => field.caption === fieldCaption).id;
    store.dispatch(addField(fieldId, AxisType.COLUMNS, index));
  });
  Object.values(basicConfig.fields)
  .filter((field) => {
    const state = store.getState();
    const rows = state.axis.rows;
    const columns = state.axis.columns;
    return !(rows.includes(field.id) || columns.includes(field.id));
  })
  .forEach((field, index) => {
    store.dispatch(addField(field.id, AxisType.FIELDS, index));
  });

  basicConfig.data.forEach((fieldCaption) => {
    const fieldId = basicConfig.datafields.find(field => field.caption === fieldCaption).id;
    store.dispatch(toggleDatafield(fieldId));
  });
}
