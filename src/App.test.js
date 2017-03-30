import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { createStore } from 'redux';

import App from './App';
import { reducer, hydrateStore } from './pivotGrid';
import { getMockDatasource, basicConfig } from './utils/mock';

describe('App', () => {
  test('renders without crashing', () => {
    const store = createStore(reducer);

    const data = getMockDatasource(1, 2, 5);
    const customFunctions = hydrateStore(store, basicConfig, data);

    const div = document.createElement('div');
    ReactDOM.render(
      <Provider store={store}>
        <App
          customFunctions={customFunctions}
          config={store.getState().config}
        />
      </Provider>,
      div
    );
  });
});
