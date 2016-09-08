import React, {Component} from 'react'

import FixedFieldButton from '../FixedFieldButton'
import DataButton from '../DataButton'

class ChartConfiguration extends Component {
  constructor (props) {
    super(props)
    this.clickButton = this.clickButton.bind(this)
  }

  clickButton (buttonId) {
    const {store} = this.props
    if (store.config.selectedField.name !== buttonId) {
      store.selectField(buttonId)
    }
  }

  render () {
    const {store} = this.props
    const {config} = store
    const rowButtons = config.allFields.map((field, index) =>
      <div style={{padding: '0px 4px'}} onClick={() => this.clickButton(field.name)}>
        <FixedFieldButton
          key={field.name}
          field={field}
          position={index}
          isSelected={field.name === config.selectedField.name}
          store={store} />
      </div>
    )

    const fieldList =
      <div>
        <div style={{padding: '7px 4px'}} className='flds-grp-cap text-muted'>
        </div>
        <div style={{padding: '7px 4px', display: 'flex'}}>
          {rowButtons}
        </div>
      </div>
    const dropTargetContainerStyle = {display: 'flex', alignItems: 'center'}

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

    const style = {
      borderSpacing: 0,
      borderCollapse: 'separate'
    }
    return (
      <div className='inner-table upper-buttons' style={style}>
        {fieldList}
        {dataButtonsContainer}
      </div>
    )
  }
}

export default ChartConfiguration
