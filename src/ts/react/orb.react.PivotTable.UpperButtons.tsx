import * as React from 'react';
import {PivotButton, DataButton} from './orb.react.Buttons';
import DropTarget from './orb.react.DropTarget';
import {AxeType} from '../orb.axe';

export default class UpperButtonsComponent extends React.Component<any,any>{
  render() {
    const config = this.props.pivotTableComp.pgridwidgetstore.pgrid.config;

    let fieldsDropTarget;
    const tdStyle = {
      padding: '7px 4px'
    };
    const fldsGrpCapStyle = {
      width: 45,
      whiteSpace: 'nowrap'
    };
    const dropTargetContainerStyle = {display: 'flex', alignItems: 'center'};

    if(config.canMoveFields) {
      const fieldsButtons = config.availablefields().map((field, index) =>
       <PivotButton key={field.name}
                    field={field}
                    axetype={null}
                    position={index}
                    pivotTableComp={this.props.pivotTableComp}>
       </PivotButton>
      );
      fieldsDropTarget =
      <div style={dropTargetContainerStyle}>
        <div style={{padding:'7px 4px'}} className="flds-grp-cap av-flds text-muted">
          Fields
        </div>
        <div style={{padding:'7px 4px'}} className="av-flds">
          <DropTarget buttons={fieldsButtons} axetype={null}>
          </DropTarget>
        </div>
      </div>;
    } else {
      fieldsDropTarget = null;
    }

    const dataButtons = config.allFields
    // This is a hacky way to detect which fields are measures in order to avoid mapping them
    // This will have to be solved later as part of a bigger overhaul where dimension and measures will be clearly separated
      .filter(field => field.aggregateFuncName !== null)
      .map((field, index) =>
      <div style={{padding:'0px 4px'}}>
      <DataButton key={field.name}
                   field={field}
                   axetype={AxeType.DATA}
                   position={index}
                   active={config.dataFields.filter(fld => fld.name===field.name).length ? true : false}
                   pgrid={this.props.pivotTableComp.pgridwidgetstore.pgrid}>
      </DataButton>
      </div>
    );

    var dataDropTarget = <div style={dropTargetContainerStyle}>
      <div style={{padding:'7px 4px'}} className="flds-grp-cap text-muted">
        <div>Data</div>
      </div>
      <div style={{padding:'7px 4px'}} className="empty">
        <div style={{display: 'flex'}}>
        {dataButtons}
        </div>
      </div>
    </div>;

    const columnButtons = config.columnFields.map((field, index) =>
       <PivotButton key={field.name}
                    field={field}
                    axetype={AxeType.COLUMNS}
                    position={index}
                    pivotTableComp={this.props.pivotTableComp}>
       </PivotButton>
    );

    const columnDropTarget =
    <div style={dropTargetContainerStyle}>
      <div style={{padding:'7px 4px'}} className="flds-grp-cap text-muted">
        Columns
      </div>
      <div style={{padding:'7px 4px'}}>
        <DropTarget buttons={columnButtons} axetype={AxeType.COLUMNS}/>
      </div>
    </div>;

    const rowButtons = config.rowFields.map((field, index) =>
    <PivotButton key={field.name}
                field={field}
                axetype={AxeType.ROWS}
                position={index}
                pivotTableComp={this.props.pivotTableComp}>
    </PivotButton>
    );

    const rowDropTarget =
    <div style={dropTargetContainerStyle}>
      <div style={{padding:'7px 4px'}} className="flds-grp-cap text-muted">
        Rows
      </div>
      <div style={{padding:'7px 4px'}}>
        <DropTarget buttons={rowButtons} axetype={AxeType.ROWS}/>
      </div>
    </div>;

    const style ={
      borderSpacing: 0,
      borderCollapse: 'separate',
    };
    return <div className="inner-table upper-buttons" style={style}>
        <div>
          {fieldsDropTarget}
          {columnDropTarget}
          {rowDropTarget}
        </div>
        {dataDropTarget}
    </div>;
  }
}
