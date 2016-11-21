import React from 'react';
import ReactDOM from 'react-dom';
// import { Observable } from 'rx-lite';

import { TestRunner } from 'fps-measurer';
import createScrollingTestCase from './fpsTests/tests';

import App from './App';
import { getMockDataSource, basicConfig } from './utils/mock';

const dataArray = getMockDataSource(1, 10);
const datasourceArray = [
  dataArray,
  // dataArray.slice(0, 5000),
  [
    { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '1' },
    { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '0' },
  ],
  { toto: '0', toto_lb: 'toto 0', qty: 1, amt: 2, titi: 'titi 0', tutu: '1' },
  // [
  // {toto: '1', toto_lb: 'toto 1', qty: 100, amt: 1000, titi: 'titi 0', tutu: '0'},
  // {toto: '12', toto_lb: 'toto 12', qty: 44, amt: 777, titi: 'titi 0', tutu: '0'},
// ],
];

const datasource = datasourceArray[0];

// const datasource = Observable.interval(2000).take(2)
//  .map(i => datasourceArray[i])
//  .do(data => console.log('data received', data))

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
