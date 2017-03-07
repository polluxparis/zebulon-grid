import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';

import Perf from 'react-addons-perf'; // ES6
import { TestRunner } from 'fps-measurer';

import './pivotGrid/index.css';

import reducer from './pivotGrid/reducers';
import hydrateStore from './pivotGrid/hydrateStore';

import createScrollingTestCase from './fpsTests/tests';
import App from './App';
import { WrappedGrid } from './pivotGrid';

import { getMockDatasource, basicConfig } from './utils/mock';

if (process.env.NODE_ENV !== 'production') {
  window.Perf = Perf;
}

const store = createStore(
  reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__()
);

const data = getMockDatasource(1, 100, 100);
const customFunctions = hydrateStore(store, basicConfig, data);

ReactDOM.render(
  <div>
    {/*<Provider store={store}>
      <App customFunctions={customFunctions} config={store.getState().config} />
    </Provider>*/}
    <WrappedGrid data={data} config={basicConfig} drilldown={() => 33} id={0} />
  </div>,
  document.getElementById('root')
);

if (process.env.REACT_APP_ORB_ENV === 'fps-test') {
  const testCase = createScrollingTestCase(
    document.getElementsByClassName('OrbGrid-data-cells')[0]
  );
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
