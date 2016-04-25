import * as React from 'react';
import * as ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow';
import {AxeType} from '../orb.axe';

export default class RowHeadersComponent extends React.Component<any,any>{
  constructor(){
    super();
  }
  setColGroup(widths) {
      const node = ReactDOM.findDOMNode(this);
    const colGroupNode = this.refs.colgroup;
    node.style.tableLayout = 'auto';

    colGroupNode.innerHTML = '';
    for(let i = 0; i < widths.length; i++) {
      const col = document.createElement('col');
      col.style.width = `${widths[i]}${8}px`;
      colGroupNode.appendChild(col);
    }
    node.style.tableLayout = 'fixed';
  }
  render() {
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const rowHeaders = pgridwidget.rows.headers.map((headerRow, index) => {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={AxeType.ROWS}
                       layoutInfos={layoutInfos}
                       pivotTableComp={this.props.pivotTableComp}>
      </PivotRow>;
    });

    return  <div className={ 'inner-table-container' + cntrClass } onWheel={this.props.pivotTableComp.onWheel}>
      <table className="inner-table">
        <colgroup ref="colgroup">
        </colgroup>
        <tbody>
          {rowHeaders}
        </tbody>
      </table>
    </div>;
  }
};
