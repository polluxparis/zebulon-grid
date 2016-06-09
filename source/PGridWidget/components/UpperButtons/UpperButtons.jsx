import { PivotButton, DataButton } from '../Buttons'
import DropTarget from '../DropTarget'
import { AxeType } from '../../Axe'

export const UpperButtonsComponent = ({store}) => {
  const {config, onClick} = store

  let fieldsDropTarget
  const dropTargetContainerStyle = {display: 'flex', alignItems: 'center'}

  if (config.canMoveFields) {
    const fieldsButtons = config.availablefields().map((field, index) =>
      <PivotButton
        key={field.name}
        field={field}
        axetype={null}
        position={index}
        store={store} />

    )
    fieldsDropTarget =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap av-flds text-muted'>
          Fields
        </div>
        <div style={{padding: '7px 4px'}} className='av-flds'>
          <DropTarget buttons={fieldsButtons} axetype={null} />
        </div>
      </div>
  } else {
    fieldsDropTarget = null
  }

  const dataButtons = config.allFields
    // This is a hacky way to detect which fields are measures in order to avoid mapping them
    // This will have to be solved later as part of a bigger overhaul where dimension and measures will be clearly separated
    .filter(field => field.aggregateFuncName !== null)
    .map((field, index) =>
      <div style={{padding: '0px 4px'}} key={'div-' + field.name}>
        <DataButton
          key={field.name}
          field={field}
          position={index}
          active={config.dataFields.filter(fld => fld.name === field.name).length}
          onClick={onClick}
          />
      </div>
  )

  var dataDropTarget =
    <div style={dropTargetContainerStyle}>
      <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
        <div>
          Data
        </div>
      </div>
      <div style={{padding: '7px 4px'}} className='empty'>
        <div style={{display: 'flex'}}>
          {dataButtons}
        </div>
      </div>
    </div>

  const columnButtons = config.columnFields.map((field, index) =>
    <PivotButton
      key={field.name}
      field={field}
      axetype={AxeType.COLUMNS}
      position={index}
      pivotTableComp={this.props.pivotTableComp} />
  )

  const columnDropTarget =
    <div style={dropTargetContainerStyle}>
      <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
        Columns
      </div>
      <div style={{padding: '7px 4px'}}>
        <DropTarget buttons={columnButtons} axetype={AxeType.COLUMNS} />
      </div>
    </div>

  const rowButtons = config.rowFields.map((field, index) =>
    <PivotButton
      key={field.name}
      field={field}
      axetype={AxeType.ROWS}
      position={index}
      pivotTableComp={this.props.pivotTableComp} />
  )

  const rowDropTarget =
    <div style={dropTargetContainerStyle}>
      <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
        Rows
      </div>
      <div style={{padding: '7px 4px'}}>
        <DropTarget buttons={rowButtons} axetype={AxeType.ROWS} />
      </div>
    </div>

  const style = {
    borderSpacing: 0,
    borderCollapse: 'separate'
  }
  return (
    <div className='inner-table upper-buttons' style={style}>
      <div>
        {fieldsDropTarget}
        {columnDropTarget}
        {rowDropTarget}
      </div>
      {dataDropTarget}
    </div>
  )
}
