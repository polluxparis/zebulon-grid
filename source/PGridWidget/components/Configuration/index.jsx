import React from 'react'

import FieldButton from '../FieldButton'
import DataButton from '../DataButton'
import DropTarget from '../DropTarget'
import { AxeType } from '../../Axe'

const Configuration = ({store}) => {
  const {config} = store
  let fieldsDropTarget
  const dropTargetContainerStyle = {display: 'flex', alignItems: 'center'}

  if (config.canMoveFields) {
    const fieldsButtons = config.availableFields.map((field, index) =>
      <FieldButton
        key={field.name}
        field={field}
        axetype={AxeType.FIELDS}
        position={index}
        store={store} />

    )
    fieldsDropTarget =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap av-flds text-muted'>
          Fields
        </div>
        <div style={{padding: '7px 4px'}} className='av-flds'>
          <DropTarget buttons={fieldsButtons} axetype={AxeType.FIELDS} />
        </div>
      </div>
  } else {
    fieldsDropTarget = null
  }

  const dataButtons = config.dataFields
    .map((field, index) =>
      <div style={{padding: '0px 4px'}} key={'div-' + field.name}>
        <DataButton
          key={field.name}
          field={field}
          position={index}
          active={config.activatedDataFields.filter(fld => fld.name === field.name).length}
          store={store}
          />
      </div>
  )

  var dataButtonsContainer =
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
    <FieldButton
      key={field.name}
      field={field}
      axetype={AxeType.COLUMNS}
      position={index}
      store={store} />
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
    <FieldButton
      key={field.name}
      field={field}
      axetype={AxeType.ROWS}
      position={index}
      store={store} />
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
      {dataButtonsContainer}
    </div>
  )
}

export default Configuration
