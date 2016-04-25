import * as React from 'react';
import PivotButton from './orb.react.PivotButton';
import DropTarget from './orb.react.DropTarget';
import DropTargetVertical from './orb.react.DropTargetVertical';
import {AxeType} from '../orb.axe';

export default class RowButtonComponent extends React.Component<any,any>{
  render() {
    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    const rowButtons = config.rowFields.map((field, index) => {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={AxeType.ROWS}
                          position={index}
                          pivotTableComp={this.props.pivotTableComp}>
             </PivotButton>;
    });

    if(config.chartMode.enabled) {
      return  <DropTargetVertical buttons={rowButtons} axetype={AxeType.ROWS}>
              </DropTargetVertical>;
    } else {
      return  <DropTarget buttons={rowButtons} axetype={AxeType.ROWS}>
              </DropTarget>;
    }
  }
};
