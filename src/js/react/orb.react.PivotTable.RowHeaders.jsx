import React from 'react';
import ReactDOM from 'react-dom';
import PivotRow from './orb.react.PivotRow.jsx';
import axe from '../orb.axe';

export default React.createClass({
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
  },
  render() {
    const self = this;
    const pgridwidget = this.props.pivotTableComp.pgridwidget;
    const cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';

    const layoutInfos = {
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    const rowHeaders = pgridwidget.rows.headers.map((headerRow, index) => {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={axe.Type.ROWS}
                       layoutInfos={layoutInfos}
                       pivotTableComp={self.props.pivotTableComp}>
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
});