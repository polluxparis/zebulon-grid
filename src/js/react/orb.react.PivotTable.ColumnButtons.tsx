import * as React from 'react';
import {AxeType} from '../orb.axe';
import PivotButton from './orb.react.PivotButton';
import DropTarget from './orb.react.DropTarget';

export default class ColumnButtonComponent extends React.Component<any,any>{
  constructor(props){
    super(props)
  }
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

    return  <DropTarget buttons={columnButtons} axetype={AxeType.COLUMNS}>
            </DropTarget>;
  }
};
