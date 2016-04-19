import * as React from 'react';
import PivotButton from './orb.react.PivotButton';
import DropTarget from './orb.react.DropTarget';
import DropTargetVertical from './orb.react.DropTargetVertical';
import axe from '../orb.axe';

export default React.createClass({
  render() {
    const self = this;
    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    const rowButtons = config.rowFields.map((field, index) => {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.ROWS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    if(config.chartMode.enabled) {
      return  <DropTargetVertical buttons={rowButtons} axetype={axe.Type.ROWS}>
              </DropTargetVertical>;
    } else {
      return  <DropTarget buttons={rowButtons} axetype={axe.Type.ROWS}>
              </DropTarget>;
    }
  }
});