import * as React from 'react';
import PivotButton from './orb.react.PivotButton';
import DropTarget from './orb.react.DropTarget';
import {AxeType} from '../orb.axe';

export default React.createClass({
  render() {
    const self = this;
    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    let fieldsDropTarget;
    if(config.canMoveFields) {
      const fieldsButtons = config.availablefields().map((field, index) => {
        return <PivotButton key={field.name}
                            field={field}
                            axetype={null}
                            position={index}
                            pivotTableComp={self.props.pivotTableComp}>
               </PivotButton>;
      });
      fieldsDropTarget = <tr>
        <td className="flds-grp-cap av-flds text-muted">
          <div>Fields</div>
        </td>
        <td className="av-flds">
          <DropTarget buttons={fieldsButtons} axetype={null}>
          </DropTarget>
        </td>
      </tr>;
    } else {
      fieldsDropTarget = null;
    }

    const dataButtons = config.dataFields.map((field, index) => {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={AxeType.DATA}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    var dataDropTarget = <tr>
      <td className="flds-grp-cap text-muted">
        <div>Data</div>
      </td>
      <td className="empty">
        <DropTarget buttons={dataButtons} axetype={AxeType.DATA}>
        </DropTarget>
      </td>
    </tr>;

    return <table className="inner-table upper-buttons">
        <tbody>
        {fieldsDropTarget}
        {dataDropTarget}
        </tbody>
    </table>;
  }
});