import React from 'react';
import ReactDOM from 'react-dom';

import { TestRunner } from 'fps-measurer';
import createScrollingTestCase from './fpsTests/tests';

import App from './App';
import {
  getMockDatasource,
  // getObservableMockDatasource,
  basicConfig,
 } from './utils/mock';


const datasource = getMockDatasource(1, 10);
// const datasource = getObservableMockDatasource();


ReactDOM.render(<App config={basicConfig} datasource={datasource} />, document.getElementById('root'));

if (process.env.REACT_APP_ORB_ENV === 'fps-test') {
  const testCase = createScrollingTestCase(document.getElementsByClassName('.OrbGrid-data-cells')[0]);
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
