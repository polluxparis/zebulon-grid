import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';

import Perf from 'react-addons-perf'; // ES6
import { TestRunner } from 'fps-measurer';

import './orb/index.css';

import reducer from './orb/reducers';
import hydrateStore from './orb/hydrateStore';

import createScrollingTestCase from './fpsTests/tests';
import App from './App';

import {
  getMockDatasource,
  // getObservableMockDatasource,
  basicConfig,
 } from './utils/mock';

if (process.env.NODE_ENV !== 'production') {
  window.Perf = Perf;
}

const store = createStore(reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__());

const customFunctions = hydrateStore(store, basicConfig, getMockDatasource(1, 100, 100));

ReactDOM.render(
  <Provider store={store}>
    <App customFunctions={customFunctions} />
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
