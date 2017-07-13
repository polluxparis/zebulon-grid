import { MEASURE_ID } from '../constants';
function replaceNullAndUndefined(val) {
  if (val === null || val === undefined) {
    return '';
  }
  return val;
}

function getSelectedText({
  selectedRange,
  rowLeaves,
  columnLeaves,
  rowDimensions,
  columnDimensions,
  measures,
  measureHeadersAxis,
  getCellValue,
  getCellDimensionInfos
}) {
  const mc = measureHeadersAxis === 'columns';
  const mr = measureHeadersAxis === 'rows';
  // Build rows headers array
  const rowsRange = [
    Math.min(
      selectedRange.selectedCellStart.rowIndex,
      selectedRange.selectedCellEnd.rowIndex
    ),
    Math.max(
      selectedRange.selectedCellStart.rowIndex,
      selectedRange.selectedCellEnd.rowIndex
    ) + 1
  ];
  const selectedRowLeaves = rowLeaves.slice(...rowsRange);

  // get rows captions
  const rowInfos = selectedRowLeaves.map(leaf =>
    getCellDimensionInfos(rowDimensions, 'rows', leaf, measures, [])
  );
  // Build columns headers array
  const columnsRange = [
    Math.min(
      selectedRange.selectedCellStart.columnIndex,
      selectedRange.selectedCellEnd.columnIndex
    ),
    Math.max(
      selectedRange.selectedCellStart.columnIndex,
      selectedRange.selectedCellEnd.columnIndex
    ) + 1
  ];
  const selectedColumnLeaves = columnLeaves.slice(...columnsRange);
  // get columns captions
  const columnsInfos = selectedColumnLeaves.map(leaf =>
    getCellDimensionInfos(columnDimensions, 'columns', leaf, measures, [])
  );
  // Build data array
  let measure;
  const cells = selectedRowLeaves.map(rowLeaf => {
    if (measureHeadersAxis === 'rows') {
      measure = measures[rowLeaf.id];
    }
    return (
      selectedColumnLeaves
        // get getCellValue from the store
        // maybe better to go without datacell and get caption directly
        // be careful about rendering function though
        .map(columnLeaf => {
          if (measureHeadersAxis === 'columns') {
            measure = measures[columnLeaf.id];
          }
          return getCellValue(
            measure.valueAccessor,
            rowLeaf.dataIndexes,
            columnLeaf.dataIndexes,
            measure.aggregation
          );
        })
    );
  });

  // build string with corner headers and column headers
  let output = '';
  let caption;
  let leaf;

  // // First rows with only the dimension and columns headers (corner)
  const depth = columnDimensions.length;
  const width = rowDimensions.length;
  for (let y = 0; y < depth; y += 1) {
    for (let x = 0; x < width; x += 1) {
      if (x === width - 1 && y < depth - mc) {
        caption = columnDimensions[y].caption;
        // output += `${replaceNullAndUndefined(columnDimensions[y])}\t`;
      } else if (y === depth - 1 && x < width - mr) {
        caption = rowDimensions[x].caption;
        // output += `${replaceNullAndUndefined(rowDimensions[x])}\t`;
      } else {
        caption = '';
      }
      output += `${replaceNullAndUndefined(caption)}\t`;
    }
    // column headers
    for (let x = 0; x < columnsInfos.length; x += 1) {
      output += `${replaceNullAndUndefined(columnsInfos[x][y].cell.caption)}\t`;
    }
    output = output.slice(0, -1);
    output += '\n';
  }

  // Other rows with rows headers and data
  for (let y = 0; y < rowInfos.length; y += 1) {
    // row headers
    for (let x = 0; x < width; x += 1) {
      output += `${replaceNullAndUndefined(rowInfos[y][x].cell.caption)}\t`;
    }
    // data cells
    for (let x = 0; x < columnLeaves.length; x += 1) {
      output += `${replaceNullAndUndefined(cells[y][x])}\t`;
    }
    output = output.slice(0, -1);
    output += '\n';
  }
  // output = output.slice(0, -1);
  return output;
}

export default function copy({
  selectedRange,
  columnLeaves,
  rowLeaves,
  rowDimensions,
  columnDimensions,
  measures,
  getCellValue,
  getCellDimensionInfos
}) {
  try {
    const bodyElement = document.getElementsByTagName('body')[0];
    const clipboardTextArea = document.createElement('textarea');
    const measureHeadersAxis = columnDimensions[columnDimensions.length - 1]
      .id === MEASURE_ID
      ? 'columns'
      : 'rows';
    clipboardTextArea.style.position = 'absolute';
    clipboardTextArea.style.left = '-10000px';
    bodyElement.appendChild(clipboardTextArea);
    clipboardTextArea.innerHTML = getSelectedText({
      selectedRange,
      rowLeaves,
      columnLeaves,
      rowDimensions,
      columnDimensions,
      measures,
      measureHeadersAxis,
      getCellValue,
      getCellDimensionInfos
    });
    clipboardTextArea.select();
    window.setTimeout(() => {
      bodyElement.removeChild(clipboardTextArea);
    }, 0);
  } catch (error) {
    /* eslint-disable no-console */
    console.error('error during copy', error);
    /* eslint-enable */
  }
}
