import ReactDOM from 'react-dom'
import {updateTableColGroup, getSize} from '../orb.utils.dom';

export function synchronizeWidths(pivotComp) {
  if(pivotComp.pgridwidgetstore.pgrid.config.chartMode.enabled) {
    return synchronizePivotChartWidths(pivotComp);
  } else {
    synchronizePivotTableWidths(pivotComp);
  }
};

export function synchronizePivotChartWidths(pivotComp) {
  const pivotWrapperTable = pivotComp.refs.pivotWrapperTable;
  const pivot = new ComponentSizeInfo(pivotComp.refs.pivot);
  const topBtns = new ComponentSizeInfo(pivotComp.refs.upperButtons);
  const cBtns = new ComponentSizeInfo(pivotComp.refs.colButtons);
  const rBtnsTbl = new ComponentSizeInfo(pivotComp.refs.rowButtons);
  const chart = new ComponentSizeInfo(pivotComp.refs.chart);
  const rBtnsWidth = Math.max(rBtnsTbl.w, 67);
  const chartWidth = pivot.w - rBtnsWidth;
  const pivotHeight = pivotComp.pgridwidgetstore.pgrid.config.height;
  const chartHeight = !pivotHeight ? null : (pivotHeight - (topBtns.h + cBtns.h));

  // set pivotWrapperTable columns width to fixed value
  updateTableColGroup(pivotWrapperTable, [
      rBtnsWidth,
      chartWidth
  ]);

  return {
    width: chartWidth,
    height: chartHeight
  };
};

export function synchronizePivotTableWidths(pivotComp) {

  const pivotWrapperTable = pivotComp.refs.pivotWrapperTable;
  const pivot = new ComponentSizeInfo(pivotComp.refs.pivot);
  const toolbar = new ComponentSizeInfo(pivotComp.refs.toolbar);
  const cHeadersTbl = new ComponentSizeInfo(pivotComp.refs.colHeaders, true, 'table');
  const rHeadersTbl = new ComponentSizeInfo(pivotComp.refs.rowHeaders, true, 'table');
  const dataCellsTbl = new ComponentSizeInfo(pivotComp.refs.dataCells, true, 'table');
  const topBtns = new ComponentSizeInfo(pivotComp.refs.upperButtons);
  const cBtns = new ComponentSizeInfo(pivotComp.refs.colButtons);
  const rBtnsTbl = new ComponentSizeInfo(pivotComp.refs.rowButtons, true);
  const hScroll = new ComponentSizeInfo(pivotComp.refs.horizontalScrollBar);
  const vScroll = new ComponentSizeInfo(pivotComp.refs.verticalScrollBar);
  const dataCellsWidths = dataCellsTbl.getLargestWidths(cHeadersTbl);
  const rHeadersWidth = Math.max(rHeadersTbl.w, rBtnsTbl.w, 67);
  const dataCellsContainerWidth = Math.min(dataCellsWidths.total + 1, pivot.w - rHeadersWidth - vScroll.w);
  const pivotHeight = pivotComp.pgridwidgetstore.pgrid.config.height;
  const dataCellsRemHeight = !pivotHeight ? null : (pivotHeight - (toolbar ? toolbar.h + 17 : 0) - (topBtns.h + cBtns.h + cHeadersTbl.h + hScroll.h));
  const dataCellsTableHeight = !dataCellsRemHeight ? null : Math.ceil(Math.min(dataCellsRemHeight, dataCellsTbl.h));


  // get rowHeaders table width to match with rowButtons table width
  rHeadersTbl.addToWidth(rHeadersWidth - rHeadersTbl.w);

  // Set dataCellsTable cells widths according to the computed dataCellsWidths
  updateTableColGroup(dataCellsTbl.myNode, dataCellsWidths.max);

  // Set colHeadersTable cells widths according to the computed dataCellsWidths
  updateTableColGroup(cHeadersTbl.myNode, dataCellsWidths.max);

  // Set rowHeadersTable cells widths
  updateTableColGroup(rHeadersTbl.myNode, rHeadersTbl.colWidths);

  dataCellsTbl.setStyle('width', dataCellsWidths.total);
  cHeadersTbl.setStyle('width', dataCellsWidths.total);
  rHeadersTbl.setStyle('width', rHeadersWidth);

  // Adjust data cells container and column headers container width
  dataCellsTbl.setParentStyle('width', dataCellsContainerWidth);
  cHeadersTbl.setParentStyle('width', dataCellsContainerWidth);

  if(dataCellsTableHeight) {
    // Adjust data cells container and row headers container height
    dataCellsTbl.setParentStyle('height', dataCellsTableHeight);
    rHeadersTbl.setParentStyle('height', dataCellsTableHeight);
  }

  // set pivotWrapperTable columns width to fixed value
  updateTableColGroup(pivotWrapperTable, [
      rHeadersWidth,
      dataCellsContainerWidth,
      vScroll.w,
      Math.max(pivot.w - (rHeadersWidth + dataCellsContainerWidth + vScroll.w), 0)
  ]);

  pivotComp.refs.horizontalScrollBar.refresh();
  pivotComp.refs.verticalScrollBar.refresh();
}


class ComponentSizeInfo {
    size: { width: number; height: number; };
  public myNode;
  public w;
  public h;
  public colWidths;

  constructor(component, isWrapper?, childType?) {
    const myNode = ReactDOM.findDOMNode(component);
    let size;

    this.myNode = isWrapper ? myNode['children'][0] : myNode;

    this.size = getSize(this.myNode);
    this.w = this.size.width;
    this.h = this.size.height;

    if(childType === 'table') {
      // get array of column widths
      this.getAllColumnsWidth();
    }
   }

  setStyle (styleProp, value){
    this.myNode.style[styleProp] = `${value}px`;
  };

  setParentStyle (styleProp, value){
    this.myNode.parentNode.style[styleProp] = `${value}px`;
  };

  getLargestWidths (otherCompInfo){
    const result = {
      max: [],
      total: 0
    };

    // get the array of max widths between dataCellsTable and colHeadersTable
    for(let i = 0; i < this.colWidths.length; i++) {
      result.max.push(Math.max(this.colWidths[i], otherCompInfo.colWidths[i]));
      result.total += result.max[i];
    }

    return result;
  };

  addToWidth (value){
    if(value > 0) {
      this.w += value;
      this.colWidths[this.colWidths.length - 1] += value;
    }
  };


  /**
   * Gets the width of all columns (maximum width of all column cells) of a html table element
   * @param  {Object}  tblObject - object having a table element in its 'myNode' property
   * @returns {Array} An array of numeric values representing the width of each column.
   *                  Its length is equal to the greatest number of cells of all rows
   *                  (in case of cells having colSpan/rowSpan greater than 1.)
   */
   getAllColumnsWidth() {
    if(this.myNode) {

      const tbl = this.myNode;
      const colWidths = [];

      for(let rowIndex = 0; rowIndex < tbl.rows.length ; rowIndex++) {
        // current row
        const currRow = tbl.rows[rowIndex];
        // reset colWidths index
        let arrayIndex = 0;
        let currWidth = null;

        // get the width of each cell within current row
        for(let cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
          // current cell
          const currCell = currRow.cells[cellIndex];

          if(currCell.__orb._visible) {
            // cell width
            //var cellwidth = Math.ceil(domUtils.getSize(currCell.children[0]).width/currCell.colSpan);
            const cellwidth = Math.ceil((currCell.__orb._textWidth/currCell.__orb._colSpan) + currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth);
            // whether current cell spans vertically to the last row
            const rowsSpan = currCell.__orb._rowSpan > 1 && currCell.__orb._rowSpan >= tbl.rows.length - rowIndex;

            // if current cell spans over more than one column, add its width (its) 'colSpan' number of times
            for(let cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
              // If cell span over more than 1 row: insert its width into colWidths at arrayIndex
              // Else: either expand colWidths if necessary or replace the width if its smaller than current cell width

              currWidth = colWidths[arrayIndex];
              // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
              while(currWidth && currWidth.inhibit > 0) {
                currWidth.inhibit--;
                arrayIndex++;
                currWidth = colWidths[arrayIndex];
              }

              if(colWidths.length - 1 < arrayIndex) {
                colWidths.push({
                  width: cellwidth
                });
              } else if(cellwidth > colWidths[arrayIndex].width) {
                colWidths[arrayIndex].width = cellwidth;
              }

              colWidths[arrayIndex].inhibit = currCell.__orb._rowSpan - 1;

              // increment colWidths index
              arrayIndex++;
            }
          }
        }

        // decrement inhibited state of all widths unsed in colWidths (not reached by current row cells)
        currWidth = colWidths[arrayIndex];
        while(currWidth) {
          if(currWidth.inhibit > 0) {
            currWidth.inhibit--;
          }
          arrayIndex++;
          currWidth = colWidths[arrayIndex];
        }
      }

      // set colWidths to the tblObject
      this.w = 0;
      this.colWidths = colWidths.map((item, index) => {
        this.w += item.width;
        return item.width;
      });
    }
  }

/**
   * Sets the width of all cells of a html table element
   * @param  {Object}  tblObject - object having a table element in its 'myNode' property
   * @param  {Array}  colWidths - an array of numeric values representing the width of each individual cell.
   *                                  Its length is equal to the greatest number of cells of all rows
   *                                  (in case of cells having colSpan/rowSpan greater than 1.)
   */
   setTableWidths(tblObject) {
    if(this.myNode) {

      // reset table width
      (this.size = (this.size || {width:0, height: 0})).width = 0;

      const tbl = this.myNode;

      // for each row, set its cells width
      for(let rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {

        // current row
        const currRow = tbl.rows[rowIndex];
        // index in colWidths
        let arrayIndex = 0;
        let currWidth = null;

        // set width of each cell
        for(let cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {

          // current cell
          const currCell = currRow.cells[cellIndex];
          if(currCell.__orb._visible) {
            // cell width
            let newCellWidth = 0;
            // whether current cell spans vertically more than 1 row
            const rowsSpan = currCell.__orb._rowSpan > 1 && rowIndex < tbl.rows.length - 1;

            // current cell width is the sum of (its) "colspan" items in colWidths starting at 'arrayIndex'
            // 'arrayIndex' should be incremented by an amount equal to current cell 'colspan' but should also skip 'inhibited' cells
            for(let cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
              currWidth = this.colWidths[arrayIndex];
              // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
              while(currWidth && currWidth.inhibit > 0) {
                currWidth.inhibit--;
                arrayIndex++;
                currWidth = this.colWidths[arrayIndex];
              }

              if(currWidth) {
                // add width of cells participating in the span
                newCellWidth += currWidth.width;
                // if current cell spans vertically more than 1 row, mark its width as inhibited for all cells participating in this span
                if(rowsSpan) {
                  currWidth.inhibit = currCell.__orb._rowSpan - 1;
                }

                // advance colWidths index
                arrayIndex++;
              }
            }

            currCell.children[0].style.width = `${newCellWidth}px`;

            // set table width (only in first iteration)
            if(rowIndex === 0) {
              let outerCellWidth = 0;
              if(currCell.__orb) {
                outerCellWidth = currCell.__orb._colSpan * (Math.ceil(currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth));
              }
              this.w += newCellWidth + outerCellWidth;
            }
          }
        }

        // decrement inhibited state of all widths unsed in colWidths (not reached by current row cells)
        currWidth = this.colWidths[arrayIndex];
        while(currWidth) {
          if(currWidth.inhibit > 0) {
            currWidth.inhibit--;
          }
          arrayIndex++;
          currWidth = this.colWidths[arrayIndex];
        }
      }
    }
  }
}
