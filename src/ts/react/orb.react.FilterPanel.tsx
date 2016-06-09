import * as React from 'react'
import * as ReactDOM from 'react-dom'
import {VirtualScroll, AutoSizer} from 'react-virtualized'
import {ResizableBox} from 'react-resizable'
import VirtualizedCheckbox from 'react-virtualized-checkbox'

import * as utils from '../orb.utils'
import * as filtering from '../orb.filtering'
import {FilterManager} from './FilterManager'

import {PivotTableComponent} from './orb.react.PivotTable'

export interface IFilterPanelProps {
  field: string,
  pivotTableComp: PivotTableComponent
}

export default class FilterPanelComponent extends React.Component<IFilterPanelProps, any> {

  public _pgridwidgetstore
  public checkboxes
  public filterManager: FilterManager
	private startingHeight = 223
	private startingWidth = 301

  constructor(props) {
    super(props)
    this._pgridwidgetstore = this.props.pivotTableComp.pgridwidgetstore
    this.filterManager = new FilterManager(this, null)

    this.onFilter = this.onFilter.bind(this)
    this.onMouseDown = this.onMouseDown.bind(this)
    // this.onMouseWheel = this.onMouseWheel.bind(this)
    this.toggleCheckbox = this.toggleCheckbox.bind(this)
    this.filterManager.onOperatorChanged = this.filterManager.onOperatorChanged.bind(this.filterManager)
  }

  destroy() {
    const container = ReactDOM.findDOMNode(this).parentNode
    ReactDOM.unmountComponentAtNode(container as Element)
    container.parentNode.removeChild(container)
  }

  onFilter(operator, term, staticValue, excludeStatic) {
    this._pgridwidgetstore.applyFilter(this.props.field, operator, term, staticValue, excludeStatic)
    this.destroy()
  }

  onMouseDown(e) {
    const container = ReactDOM.findDOMNode(this).parentNode
    let target = e.target || e.srcElement
    while (target !== null) {
      if (target === container) {
        return true
      }
      target = target.parentNode
    }

    this.destroy()
  }

  // onMouseWheel(e) {
  //   const valuesTable = this.refs.valuesTable
  //   let target = e.target || e.srcElement
  //   while(target !== null) {
  //     if(target == valuesTable) {
  //       if(valuesTable['scrollHeight'] <= valuesTable['clientHeight']) {
  //         utils.stopPropagation(e)
  //         utils.preventDefault(e)
  //       }
  //       return
  //     }
  //     target = target.parentNode
  //   }
  //   this.destroy()
  // }

  componentWillMount() {
    utils.addEventListener(document, 'mousedown', this.onMouseDown)
    // utils.addEventListener(document, 'wheel', this.onMouseWheel)
    utils.addEventListener(window, 'resize', this.destroy)
  }

  componentDidMount() {
      // this.filterManager.init(ReactDOM.findDOMNode(this))
  }

  componentWillUnmount() {
    utils.removeEventListener(document, 'mousedown', this.onMouseDown)
    // utils.removeEventListener(document, 'wheel', this.onMouseWheel)
    utils.removeEventListener(window, 'resize', this.destroy)
  }

  toggleCheckbox(value) {
    console.log(`toggling ${value.value}`)
  }

  getVisibleCheckboxes() {
    // this.checkboxes.filter(checkbox => )
  }

  render() {
    this.filterManager.reactComp = this
    this.filterManager.initialFilterObject = this._pgridwidgetstore.pgrid.getFieldFilter(this.props.field)
    this.checkboxes = this._pgridwidgetstore.pgrid.getFieldValues(this.props.field).map(val => ({checked: true, label: val}))

		const checkboxes =
			<VirtualizedCheckbox
				options={this.checkboxes}
				onOk={(result) => this.onFilter('','',result,false)}
				onCancel={() => this.destroy()}
				maxHeight={this.startingHeight}
			/>

    // const buttonClass = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().orbButton

    // const currentFilter = this._pgridwidgetstore.pgrid.getFieldFilter(this.props.field)

    const divStyle = {
      backgroundColor: 'white',
      border: 'solid 1px',
      boxShadow: '0 5px 15px #9d9d9d',
      display: 'flex',
      flexDirection: 'column',
      fontSize: '90%',
      height: '100%',
      justifyContent: 'space-between',
      padding: '3px',
      width: '100%'
    }

    return(
      <ResizableBox width={this.startingWidth} height={this.startingHeight} minConstraints={[this.startingWidth, this.startingHeight]}>
        <div style={divStyle}>
          {checkboxes}
        </div>
      </ResizableBox>
    )

  }
}
