import React from 'react';
import ReactDOM from 'react-dom';

import Perf from 'react-addons-perf'; // ES6
import { TestRunner } from 'fps-measurer';

import './pivotGrid/index.css';

import createScrollingTestCase from './fpsTests/tests';
import App from './App';

if (process.env.NODE_ENV !== 'production') {
  window.Perf = Perf;
  // whyDidYouUpdate(React, { exclude: '/^DataCell' });
}

ReactDOM.render(<App />, document.getElementById('root'));

if (process.env.REACT_APP_PIVOTGRID_ENV === 'fps-test') {
  const testCase = createScrollingTestCase(
    document.getElementsByClassName('pivotgrid-data-cells')[0]
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
