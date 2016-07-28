import React, {Component} from 'react'
import { DragDropContext } from 'react-dnd'
import HTML5Backend from 'react-dnd-html5-backend'

import DragManager from '../../DragManager'
import FieldButton from '../FieldButton'
import DataButton from '../DataButton'
import FieldList from '../FieldList'
import { AxeType } from '../../Axe'

class Configuration extends Component {
  constructor (props) {
    super(props)
    this.moveButton = this.moveButton.bind(this)
    DragManager.init(this.moveButton.bind(this))
  }

  moveButton (buttonId, oldAxeType, newAxeType, position) {
    const {store} = this.props
    store.moveField(buttonId, oldAxeType, newAxeType, position)
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
          axetype={AxeType.FIELDS}
          position={index}
          store={store} />

      )
      unusedFieldList =
        <div style={dropTargetContainerStyle}>
          <div style={{padding: '7px 4px'}} className='flds-grp-cap av-flds text-muted'>
            Fields
          </div>
          <div style={{padding: '7px 4px'}} className='av-flds'>
            <FieldList buttons={fieldsButtons} axetype={AxeType.FIELDS} moveButton={this.moveButton} />
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

    const columnFieldList =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
          Columns
        </div>
        <div style={{padding: '7px 4px'}}>
          <FieldList buttons={columnButtons} axetype={AxeType.COLUMNS} moveButton={this.moveButton} />
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

    const rowFieldList =
      <div style={dropTargetContainerStyle}>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
          Rows
        </div>
        <div style={{padding: '7px 4px'}}>
          <FieldList buttons={rowButtons} axetype={AxeType.ROWS} moveButton={this.moveButton} />
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

export default DragDropContext(HTML5Backend)(Configuration)
