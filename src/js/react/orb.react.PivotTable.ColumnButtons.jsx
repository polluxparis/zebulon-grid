import React from 'react';
import axe from '../orb.axe';
import PivotButton from './orb.react.PivotButton.jsx';
import DropTarget from './orb.react.DropTarget.jsx';

export default React.createClass({
  render() {
    const self = this;
    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    const columnButtons = config.columnFields.map((field, index) => {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.COLUMNS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    return  <DropTarget buttons={columnButtons} axetype={axe.Type.COLUMNS}>
            </DropTarget>;
  }
});