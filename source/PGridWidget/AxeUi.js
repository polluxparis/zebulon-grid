'use strict'

import { AxeType } from './Axe'
import { Header, DataHeader, HeaderType } from './Cells'

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
export default class AxeUi {

  constructor (axeModel) {
    /**
     * Dimensions axe
     * @type {orb.axe}
     */
    this.axe = axeModel

    /**
     * Headers render properties
     * @type {Array}
     */
    this.headers = []

    this._x = 0

    this.build()
  }

  build () {
    var headers = []
    var grandtotalHeader

    if (this.axe != null) {
      if (this.axe.root.values.length > 0 || this.axe.store.config.grandTotal.rowsvisible) {
        headers.push([])

        // Fill Rows layout infos
        this.getUiInfo(headers, this.axe.root, this.axe.type)

        if (this.axe.store.config.grandTotal.rowsvisible) {
          var lastrow = headers[headers.length - 1]
          grandtotalHeader = new Header(this.axe.type, HeaderType.GRAND_TOTAL, this.axe.root, null, this.dataFieldsCount())
          if (lastrow.length === 0) {
            lastrow.push(grandtotalHeader)
          } else {
            headers.push([grandtotalHeader])
          }
        }
      }

      if (headers.length === 0) {
        headers.push([grandtotalHeader = new Header(this.axe.type, HeaderType.INNER, this.axe.root, null, this.dataFieldsCount())])
      }

      if (grandtotalHeader) {
        // add grand-total data headers if more than 1 data field and they will be the leaf headers
        this.addDataHeaders(headers, grandtotalHeader)
      }
    }
    this.headers = headers
  }

  addDataHeaders (infos, parent, y) {
    if (this.isMultiDataFields()) {
      var lastInfosArray = infos[infos.length - 1]
      for (let datafieldindex = 0; datafieldindex < this.dataFieldsCount(); datafieldindex++) {
        lastInfosArray.push(new DataHeader(this.axe.store.config.activatedDataFields[datafieldindex], parent, this._x++, y))
        if (datafieldindex < this.dataFieldsCount() - 1) {
          infos.push((lastInfosArray = []))
        }
      }
    } else {
      this._x++
    }
  }

  /**
   * Fills the infos array given in argument with the dimension layout infos as row.
   * @param  {orb.dimension}  dimension - the dimension to get ui info for
   * @param  {object}  infos - array to fill with ui dimension info
   * @param  {number}  axetype - type of the axe (rows or columns)
   */
  getUiInfo (infos, dimension, axetype, y = 0) {
    if (dimension.values.length > 0) {
      var infosMaxIndex = infos.length - 1
      var lastInfosArray = infos[infosMaxIndex]
      var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null

      for (let valIndex = 0; valIndex < dimension.values.length; valIndex++) {
        var subvalue = dimension.values[valIndex]
        var subdim = dimension.subdimvals[subvalue]

        var subTotalHeader
        if (!subdim.isLeaf && subdim.field.subTotal.visible) {
          // x here will probably create bugs. To change when necessary
          subTotalHeader = new Header(axetype, HeaderType.SUB_TOTAL, subdim, parent, this.dataFieldsCount(), this._x, y)
        } else {
          subTotalHeader = null
        }

        var newHeader = new Header(axetype, null, subdim, parent, this.dataFieldsCount(), this._x, y, subTotalHeader)

        if (valIndex > 0) {
          infos.push((lastInfosArray = []))
        }

        lastInfosArray.push(newHeader)

        if (!subdim.isLeaf) {
          this.getUiInfo(infos, subdim, axetype, y + 1)
          if (subdim.field.subTotal.visible) {
            infos.push([subTotalHeader])

            // add sub-total data headers if more than 1 data field and they will be the leaf headers
            this.addDataHeaders(infos, subTotalHeader, y + 1)
          }
        } else {
          // add data headers if more than 1 data field and they will be the leaf headers
          this.addDataHeaders(infos, newHeader, y + 1)
        }
      }
    }
  }

  dataFieldsCount () {
    return (this.axe.store.config.dataHeadersLocation === 'columns' && this.axe.type === AxeType.COLUMNS) ||
    (this.axe.store.config.dataHeadersLocation === 'rows' && this.axe.type === AxeType.ROWS)
      ? this.axe.store.config.activatedDataFieldsCount : 1
  }

  isMultiDataFields () {
    return this.dataFieldsCount() > 1
  }

  toggleFieldExpansion (field, newState) {
    var toToggle = []
    var allExpanded = true
    var hIndex

    for (var i = 0; i < this.headers.length; i++) {
      for (hIndex = 0; hIndex < this.headers[i].length; hIndex++) {
        var header = this.headers[i][hIndex]
        if (header.type === HeaderType.SUB_TOTAL && (field == null || header.dim.field.name === field.name)) {
          toToggle.push(header)
          allExpanded = allExpanded && header.expanded
        }
      }
    }

    if (newState !== undefined) {
      allExpanded = !newState
    }

    if (toToggle.length > 0) {
      for (hIndex = 0; hIndex < toToggle.length; hIndex++) {
        if (allExpanded) {
          toToggle[hIndex].collapse()
        } else {
          toToggle[hIndex].expand()
        }
      }
      return true
    }

    return false
  }
}
