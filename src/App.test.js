import React from 'react';
import ReactDOM from 'react-dom';
// import renderer from 'react-test-renderer';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import { GridConfiguration, Grid, Store } from './orb';
// import { AxisType } from './orb/Axis';
import { getMockDatasource, basicConfig } from './utils/mock';

const datasource = getMockDatasource();

class RawApp extends React.Component {
  componentDidMount() {
    this.props.store.forceUpdateCallback = this.forceUpdate.bind(this);
  }
  render() {
    return (
      <div>
        <GridConfiguration store={this.props.store} />
        <Grid store={this.props.store} height={600} width={800} />
      </div>
    );
  }
}

const App = DragDropContext(HTML5Backend)(RawApp);

it('renders without crashing', () => {
  const store = new Store(basicConfig, null, datasource);
  const div = document.createElement('div');
  ReactDOM.render(<App store={store} />, div);
});
