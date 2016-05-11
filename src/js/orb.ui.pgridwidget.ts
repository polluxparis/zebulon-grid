import * as React from 'react';
import * as ReactDOM from 'react-dom';

import {PGridWidgetStore} from './orb.ui.pgridwidgetstore';
import {PivotTableComponent} from './react/orb.react.PivotTable';

export class PGridWidget{

  private store: PGridWidgetStore;

  constructor(config){
    this.store= new PGridWidgetStore(config);
  }

  render(elem){
    ReactDOM.render(React.createElement(PivotTableComponent,{pgridwidgetstore:this.store}), elem);
  }
}
