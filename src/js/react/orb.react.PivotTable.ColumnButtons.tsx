import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotButton from './orb.react.PivotButton';
import DropTarget from './orb.react.DropTarget';

export default class ColumnButtonsComponent extends React.Component<any,any>{
  render() {
    const config = this.props.pivotTableComp.pgridwidgetstore.pgrid.config;

    const columnButtons = config.columnFields.map((field, index) => {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={AxeType.COLUMNS}
                          position={index}
                          pivotTableComp={this.props.pivotTableComp}>
             </PivotButton>;
    });

    return( <table><tbody><tr><td><DropTarget buttons={columnButtons} axetype={AxeType.COLUMNS}/></td></tr></tbody></table>);
  }
};
