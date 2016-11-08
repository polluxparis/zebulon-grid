import React, {Component} from 'react'

import DragManager from '../../DragManager'
import FieldButton from '../FieldButton'
import DataButton from '../DataButton'
import FieldList from '../FieldList'
import { AxisType } from '../../Axis'

class GridConfiguration extends Component {
  constructor (props) {
    super(props)
    this.moveButton = this.moveButton.bind(this)
    DragManager.init(this.moveButton.bind(this))
  }

  moveButton (buttonId, oldAxisType, newAxisType, position) {
    const {store} = this.props
    store.moveField(buttonId, oldAxisType, newAxisType, position)
  }

  render () {
    const {store} = this.props
    const {config} = store
    let unusedFieldList
    const dropTargetContainerStyle = {display: 'flex', alignItems: 'center'}

    if (config.canMoveFields) {
      const fieldsButtons = config.availableFields.map((field, index) =>
        <FieldButton
          key={field.name}
          field={field}
          axetype={AxisType.FIELDS}
          position={index}
          store={store} />

      )
      unusedFieldList =
        <div style={dropTargetContainerStyle}>
          <div style={{padding: '7px 4px'}} className='flds-grp-cap av-flds text-muted'>
            Fields
          </div>
          <div style={{padding: '7px 4px'}} className='av-flds'>
            <FieldList buttons={fieldsButtons} axetype={AxisType.FIELDS} moveButton={this.moveButton} />
          </div>
        </div>
    } else {
      unusedFieldList = null
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

    const dataButtonsContainer =
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
        axetype={AxisType.COLUMNS}
        position={index}
        store={store} />
    )

    const columnFieldList =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
          Columns
        </div>
        <div style={{padding: '7px 4px'}}>
          <FieldList buttons={columnButtons} axetype={AxisType.COLUMNS} moveButton={this.moveButton} />
        </div>
      </div>

    const rowButtons = config.rowFields.map((field, index) =>
      <FieldButton
        key={field.name}
        field={field}
        axetype={AxisType.ROWS}
        position={index}
        store={store} />
    )

    const rowFieldList =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
          Rows
        </div>
        <div style={{padding: '7px 4px'}}>
          <FieldList buttons={rowButtons} axetype={AxisType.ROWS} moveButton={this.moveButton} />
        </div>
      </div>

    const style = {
      borderSpacing: 0,
      borderCollapse: 'separate'
    }
    return (
      <div className='inner-table upper-buttons' style={style}>
        <div>
          {unusedFieldList}
          {columnFieldList}
          {rowFieldList}
        </div>
        {dataButtonsContainer}
      </div>
    )
  }
}

export default GridConfiguration
