import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { createStore } from 'redux';
import reducer from './orb/reducers';
import { pushData, setConfig, addField, toggleDatafield } from './orb/actions';

import { TestRunner } from 'fps-measurer';
import createScrollingTestCase from './fpsTests/tests';

import App from './App';
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

const datasource = getMockDatasource(1, 10, 10);
store.dispatch(pushData(datasource));

// const datasource = getObservableMockDatasource(1100);
// datasource.subscribe(data => store.dispatch(pushData(data)));

store.dispatch(setConfig(basicConfig));
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

ReactDOM.render(
  <Provider store={store}>
    <App config={basicConfig} datasource={datasource} />
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
