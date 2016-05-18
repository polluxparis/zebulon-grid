import * as React from 'react';
import * as ReactDOM from 'react-dom';
import {HeaderType} from '../orb.ui.header';
import * as domUtils from '../orb.utils.dom';
let _paddingLeft = null, _borderLeft = null;

import {PGridWidgetStore} from '../orb.ui.pgridwidgetstore';
import {Header, DataHeader, DataCell} from '../orb.ui.header';

export interface PivotHeaderCellProps{
  key:number,
  cell:Header|DataHeader,
  leftmost:boolean,
  topmost:boolean,
  pgridwidgetstore: PGridWidgetStore
}

export class PivotHeaderCell extends React.Component<PivotHeaderCellProps,{}>{
  private _latestVisibleState: boolean;

  constructor(props){
    super(props);
    this._latestVisibleState = false;
    this.expand = this.expand.bind(this);
    this.collapse = this.collapse.bind(this);
  }
  expand() {
      this.props.pgridwidgetstore.expandRow(this.props.cell);
  }
  collapse() {
      this.props.pgridwidgetstore.collapseRow(this.props.cell);
  }
  updateCellInfos() {
    const node = ReactDOM.findDOMNode(this);
    const cell = this.props.cell;
    node['__orb'] = node['__orb'] || {};

    if(!cell.visible()) {

      node['__orb']._visible = false;

    } else {
      const cellContentNode = this.refs['cellContent'];

      const propList = [];
      const retPaddingLeft = _paddingLeft == null;
      const retBorderLeft = !this.props.leftmost && _borderLeft == null;
      const text = node.textContent || node['innerText'];

      if(retPaddingLeft) {
        propList.push('padding-left');
      }

      if(retBorderLeft) {
        propList.push('border-left-width');
      }

      if(propList.length > 0) {
        const nodeStyle = domUtils.getStyle(node, propList, true);

        if(retPaddingLeft) {
          _paddingLeft = parseFloat(nodeStyle[0]);
        }

        if(retBorderLeft) {
          _borderLeft = parseFloat(nodeStyle[retPaddingLeft ? 1 : 0]);
        }
      }

      domUtils.removeClass(node, 'cell-hidden');

      node['__orb']._visible = true;
      if(text != node['__orb']._lastText || !node['__orb']._textWidth) {
        node['__orb']._lastText = text;
        node['__orb']._textWidth = domUtils.getSize(cellContentNode).width;
      }
      node['__orb']._colSpan = this.props.cell.hspan(true) || 1;
      node['__orb']._rowSpan = this.props.cell.vspan(true) || 1;
      node['__orb']._paddingLeft = _paddingLeft;
      node['__orb']._paddingRight = _paddingLeft;
      node['__orb']._borderLeftWidth = this.props.leftmost ? 0 : _borderLeft;
      node['__orb']._borderRightWidth = 0;
    }
  }
  componentDidMount() {
    // this.updateCellInfos();
  }
  componentDidUpdate() {
    // this.updateCellInfos();
  }
  shouldComponentUpdate(nextProps, nextState) {
    if(nextProps.cell && nextProps.cell == this.props.cell && !this._latestVisibleState && !nextProps.cell.visible()) {
      return false;
    }
    return true;
  }
  render() {
    const cell = this.props.cell;
    const divcontent = [];
    let value;
    let cellClick;
    let headerPushed = false;

    this._latestVisibleState = cell.visible();


    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        const isWrapper = cell.type === HeaderType.WRAPPER && (cell as Header).dim.field.subTotal.visible && (cell as Header).dim.field.subTotal.collapsible;
        const isSubtotal = cell.type === HeaderType.SUB_TOTAL && !cell.expanded;
        if(isWrapper || isSubtotal) {
          headerPushed = true;

          divcontent.push(<div key="header-value" ref="cellContent">
            <div className="orb-tgl-btn"><div className={'orb-tgl-btn-' + (isWrapper ? 'down' : 'right')} onClick={(isWrapper ? this.collapse : this.expand)}></div></div>
            <div className="hdr-val"><div dangerouslySetInnerHTML={{__html: cell.value || '&#160;'}}></div></div>
            </div>);
        } else {
          value = (cell.value || '&#160;') + (cell.type === HeaderType.SUB_TOTAL ? ' Total' : '');
        }
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      default:
        break;
    }

    if(!headerPushed) {
      let headerClassName;
      if(cell.template != 'cell-template-dataheader' && cell.type !== HeaderType.GRAND_TOTAL) {
        headerClassName = 'hdr-val';
      }
      divcontent.push(<div key="cell-value" ref="cellContent" className={headerClassName}><div dangerouslySetInnerHTML={{__html: value || '&#160;'}}></div></div>);
    }

    return <div
              // className={getClassname(this.props)}
              style={{width:'100%', height:'100%'}}
              onDoubleClick={ cellClick }
              >
                {divcontent}
           </div>;
  }
}

export interface PivotDataCellProps{
  key:number,
  cell:DataCell,
  pgridwidgetstore: PGridWidgetStore
}

export class PivotDataCell extends React.Component<PivotDataCellProps,{}>{

  _latestVisibleState: boolean = false;

  render() {
    const cell = this.props.cell;

    this._latestVisibleState = cell.visible();

    const value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
    const cellClick = () => this.props.pgridwidgetstore.drilldown(cell);

    const divcontent = [<div key="cell-value" ref="cellContent" className='cell-data'><div dangerouslySetInnerHTML={{__html: value || '&#160;'}}></div></div>];

    return <div
              // className={getClassname(this.props)}
              style={{width:'100%', height:'100%'}}
              onDoubleClick={ cellClick }
            >
                  {divcontent}
           </div>;
  }
}

function getClassname(compProps) {
    const cell = compProps.cell;
    let classname = cell.cssclass;
    const isEmpty = cell.template === 'cell-template-empty';

    if(!cell.visible()) {
      classname += ' cell-hidden';
    }

    if(cell.type === HeaderType.SUB_TOTAL && cell.expanded) {
      classname += ' header-st-exp';
    }

    if(cell.type === HeaderType.GRAND_TOTAL) {
      if(cell.dim.depth === 1) {
        classname += ' header-nofields';
      } else if(cell.dim.depth > 2) {
        classname += ' header-gt-exp';
      }
    }

    if(compProps.leftmost) {
      classname += ` ${cell.template === 'cell-template-datavalue' ? 'cell' : 'header'}-leftmost`;
    }

    if(compProps.topmost) {
      classname += ' cell-topmost';
    }

    return classname;
}
