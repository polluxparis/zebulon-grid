import { AxisType } from './Axis';
import { getKey } from './utils/keys';
import { isNullOrUndefined } from './utils/generic';

export const HeaderType = {
  EMPTY: 1,
  DATA_HEADER: 2,
  DATA_VALUE: 3,
  FIELD_BUTTON: 4,
  INNER: 5,
  WRAPPER: 6,
  SUB_TOTAL: 7,
  GRAND_TOTAL: 8,
  DIMENSION_HEADER: 9,
  getHeaderClass(headerType, axisType) {
    let cssclass;
    if (axisType === AxisType.ROWS) {
      cssclass = 'header-row';
    } else if (axisType === AxisType.COLUMNS) {
      cssclass = 'header-col';
    } else {
      cssclass = '';
    }
    switch (headerType) {
      case HeaderType.EMPTY:
      case HeaderType.FIELD_BUTTON:
      default:
        cssclass = 'empty';
        break;
      case HeaderType.INNER:
        cssclass = `header ${cssclass}`;
        break;
      case HeaderType.WRAPPER:
        cssclass = `header ${cssclass}`;
        break;
      case HeaderType.SUB_TOTAL:
        cssclass = `header header-st ${cssclass}`;
        break;
      case HeaderType.GRAND_TOTAL:
        cssclass = `header header-gt ${cssclass}`;
        break;
    }

    return cssclass;
  },
  getCellClass(rowHeaderType, colHeaderType) {
    let cssclass = '';
    switch (rowHeaderType) {
      case HeaderType.GRAND_TOTAL:
        cssclass = 'cell-gt';
        break;
      case HeaderType.SUB_TOTAL:
        if (colHeaderType === HeaderType.GRAND_TOTAL) {
          cssclass = 'cell-gt';
        } else {
          cssclass = 'cell-st';
        }
        break;
      default:
        if (colHeaderType === HeaderType.GRAND_TOTAL) {
          cssclass = 'cell-gt';
        } else if (colHeaderType === HeaderType.SUB_TOTAL) {
          cssclass = 'cell-st';
        } else {
          cssclass = '';
        }
    }
    return cssclass;
  }
};

class CellBase {
  constructor(options) {
    // // CellBase is an abstract class
    // Symbol new.target does not pass in Uglify.js
    // if (new.target === CellBase) {
    //   throw new Error('CellBase is an abstract class and cannot be instantiated directly.')
    // }

    /**
     * axe type (COLUMNS, ROWS, DATA, ...)
     * @type {pivotgrid.AxisType}
     */
    this.axisType = options.axisType;
    /**
     * cell type (EMPTY, DATA_VALUE, FIELD_BUTTON, INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL, ...)
     * @type {HeaderType}
     */
    this.type = options.type;
    /**
     * header cell template
     * @type {String}
     */
    this.template = options.template;
    /**
     * header cell value
     * @type {Object}
     */
    this.value = options.value;
    /**
     * is header cell expanded
     * @type {Boolean}
     */
    this.expanded = true;
    /**
     * header cell css class(es)
     * @type {String}
     */
    this.cssclass = options.cssclass;
    /**
     * header cell width
     * @type {Number}
     */
    this.hspan = options.hspan || (() => 1);
    /**
     * gets header cell's height
     * @return {Number}
     */
    this.vspan = options.vspan || (() => 1);
    /**
     * gets wether header cell is visible
     * @return {Boolean}
     */
    this.visible = options.isvisible || (() => true);

    this.key = this.axisType + this.type + this.value;
  }
}

/**
 * Creates a new instance of a row header.
 * @class
 * @memberOf pivotgrid.ui
 * @param  {pivotgrid.ui.rowHeader} parent - parent header.
 * @param  {pivotgrid.dimension} dim - related dimension values container.
 * @param  {HeaderType} type - header type (INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL).
 * @param  {pivotgrid.ui.rowHeader} totalHeader - sub total or grand total related header.
 */
export class Header extends CellBase {
  constructor(
    axisType,
    headerTypeP,
    dimension,
    parent,
    datafieldscount,
    x,
    y,
    subtotalHeader,
    crossAxisFieldsCode = []
  ) {
    const isOnRowAxis = axisType === AxisType.ROWS;
    const headerType =
      headerTypeP ||
      (dimension.depth === 1 ? HeaderType.INNER : HeaderType.WRAPPER);
    let value;
    let hspan;
    let vspan;

    switch (headerType) {
      case HeaderType.GRAND_TOTAL:
        value = 'Total';
        hspan = isOnRowAxis ? dimension.depth - 1 || 1 : datafieldscount || 1;
        vspan = isOnRowAxis ? datafieldscount || 1 : dimension.depth - 1 || 1;
        // key = `${TOTAL_ID}${AXIS_SEPARATOR}${crossAxisFieldsCode.join(KEY_SEPARATOR)}`;
        break;
      case HeaderType.SUB_TOTAL:
        value = String(dimension.id);
        hspan = isOnRowAxis ? dimension.depth : datafieldscount || 1;
        vspan = isOnRowAxis ? datafieldscount || 1 : dimension.depth;
        // key = parent ? `${parent.key}${KEY_SEPARATOR}${value}` : value;
        break;
      default:
        value = String(dimension.id);
        hspan = isOnRowAxis ? 1 : null;
        vspan = isOnRowAxis ? null : 1;
        // key = parent
        //   ? `${parent.key}${KEY_SEPARATOR}${dimension.id}`
        //   : String(dimension.id);
        break;
    }

    const options = {
      axisType,
      type: headerType,
      template: isOnRowAxis
        ? 'cell-template-row-header'
        : 'cell-template-column-header',
      value,
      cssclass: HeaderType.getHeaderClass(headerType, axisType)
    };

    super(options);

    this.isOnRowAxis = isOnRowAxis;
    this.hspan = hspan != null ? () => hspan : this.calcSpan;
    this.vspan = vspan != null ? () => vspan : this.calcSpan;
    this.isvisible = this.isParentExpanded;

    this.subtotalHeader = subtotalHeader;
    this.parent = parent;
    this.dim = dimension;
    // this.expanded = this.getState()
    // ? this.getState().expanded
    // : (headerType !== HeaderType.SUB_TOTAL || !dimension.field.subTotal.collapsed);
    this.subheaders = [];

    if (parent != null) {
      this.parent.subheaders.push(this);
    }

    this.datafieldscount = datafieldscount;

    this.key = getKey({
      headerType,
      parent,
      crossAxisFieldsCode,
      value,
      dimension
    });

    // Total headers have not dimension caption
    if (
      headerType === HeaderType.GRAND_TOTAL ||
      headerType === HeaderType.SUB_TOTAL
    ) {
      this.caption = value;
    } else {
      this.caption = dimension.caption;
    }

    this.x = x;
    this.y = y;
  }

  expand() {
    this.expanded = true;
    // this.setState({
    //   expanded: this.expanded,
    // });
  }

  collapse() {
    this.expanded = false;
    // this.setState({
    //   expanded: this.expanded,
    // });
  }

  isParentExpanded() {
    if (this.type === HeaderType.SUB_TOTAL) {
      let hparent = this.parent;
      while (hparent != null) {
        if (hparent.subtotalHeader && !hparent.subtotalHeader.expanded) {
          return false;
        }
        hparent = hparent.parent;
      }
      return true;
    }
    const isexpanded =
      this.dim.isRoot ||
      this.dim.isLeaf ||
      !this.dim.field.subTotal.visible ||
      this.subtotalHeader.expanded;
    if (!isexpanded) {
      return false;
    }

    let par = this.parent;
    while (
      par != null &&
      (!par.dim.field.subTotal.visible ||
        (par.subtotalHeader != null && par.subtotalHeader.expanded))
    ) {
      par = par.parent;
    }
    return par == null || par.subtotalHeader == null
      ? isexpanded
      : par.subtotalHeader.expanded;
  }

  calcSpan(ignoreVisibility) {
    let span = 0;
    let subSpan;
    let addone = false;

    if (this.isOnRowAxis || ignoreVisibility || this.visible()) {
      if (!this.dim.isLeaf) {
        // subdimvals 'own' properties are the set of values for this dimension
        if (this.subheaders.length > 0) {
          for (let i = 0; i < this.subheaders.length; i += 1) {
            const subheader = this.subheaders[i];
            // if it's not an array
            if (!subheader.dim.isLeaf) {
              subSpan = this.isOnRowAxis
                ? subheader.vspan()
                : subheader.hspan();
              span += subSpan;
              if (i === 0 && subSpan === 0) {
                addone = true;
              }
            } else {
              span += this.datafieldscount || 1;
            }
          }
        } else {
          span += this.datafieldscount || 1;
        }
      } else {
        return this.datafieldscount || 1;
      }
      return span + (addone ? 1 : 0);
    }
    return span;
  }
}

export class DataHeader extends CellBase {
  constructor(axisType, datafield, parent, x, y) {
    super({
      axisType,
      type: HeaderType.DATA_HEADER,
      template: 'cell-template-dataheader',
      value: datafield,
      cssclass: HeaderType.getHeaderClass(parent.type, parent.axisType),
      isvisible: parent.visible
    });

    this.parent = parent;

    if (parent != null) {
      this.parent.subheaders.push(this);
    }

    this.key = getKey({
      headerType: HeaderType.DATA_HEADER,
      parent,
      datafieldId: datafield.id
    });

    this.caption = this.value.caption;

    this.x = x;
    this.y = y;
  }
}

export class DimensionHeader extends CellBase {
  constructor(axisType, field) {
    super({
      axisType,
      type: HeaderType.DIMENSION_HEADER,
      template: 'cell-template-dimensionheader',
      value: field,
      cssclass: HeaderType.getHeaderClass(
        HeaderType.DIMENSION_HEADER,
        axisType
      ),
      isvisible: () => true
    });

    this.key = String(field.id);

    this.hspan = () => 1;
    this.vspan = () => 1;
  }
}

export class DataCell extends CellBase {
  constructor(
    getCellValue,
    dataHeadersLocation,
    rowinfo,
    colinfo,
    customFunctions
  ) {
    let rowHeader;
    let columnHeader;
    let dataHeader;
    if (rowinfo.type === HeaderType.DATA_HEADER) {
      dataHeader = rowinfo;
      rowHeader = rowinfo.parent;
      columnHeader = colinfo;
    } else if (colinfo.type === HeaderType.DATA_HEADER) {
      dataHeader = colinfo;
      rowHeader = rowinfo;
      columnHeader = colinfo.parent;
    } else {
      // no data header
      rowHeader = rowinfo;
      columnHeader = colinfo;
    }
    const rowDimension = rowHeader.dim;
    const columnDimension = columnHeader.dim;
    const rowType = rowHeader.type;
    const colType = columnHeader.type;

    let value;
    let datafield;
    let caption;
    if (!isNullOrUndefined(dataHeader)) {
      datafield = dataHeader.value;
      value = getCellValue(
        customFunctions.access[datafield.id] || (() => null),
        rowDimension,
        columnDimension,
        customFunctions.aggregation[datafield.id]
      );
      if (!isNullOrUndefined(customFunctions.format[datafield.id])) {
        caption = customFunctions.format[datafield.id](value);
      } else {
        caption = value;
      }
    } else {
      value = null;
      caption = value;
    }

    super({
      axisType: null,
      type: HeaderType.DATA_VALUE,
      template: 'cell-template-datavalue',
      value,
      cssclass: `cell ${HeaderType.getCellClass(rowType, colType)}`,
      isvisible: true
    });

    this.rowDimension = rowDimension;
    this.columnDimension = columnDimension;
    this.rowType = rowType;
    this.colType = colType;
    this.datafield = datafield;
    this.hspan = 1;
    this.vspan = 1;
    this.caption = caption;
    this.value = value;
  }
}

export class ButtonCell extends CellBase {
  constructor(field) {
    super({
      axisType: null,
      type: HeaderType.FIELD_BUTTON,
      template: 'cell-template-fieldbutton',
      value: field,
      cssclass: HeaderType.getHeaderClass(HeaderType.FIELD_BUTTON)
    });
  }
}

export class EmptyCell extends CellBase {
  constructor(hspan, vspan) {
    super({
      axisType: null,
      type: HeaderType.EMPTY,
      template: 'cell-template-empty',
      value: null,
      cssclass: HeaderType.getHeaderClass(HeaderType.EMPTY),
      hspan() {
        return hspan;
      },
      vspan() {
        return vspan;
      }
    });
  }
}
