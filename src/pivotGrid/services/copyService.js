import { DataCell } from '../Cells';

function replaceNullAndUndefined(val) {
  if (val === null || val === undefined) {
    return '';
  }
  return val;
}

function getSelectedText(
  {
    selectedCellStart,
    selectedCellEnd,
    dataHeadersLocation,
    getCellValue,
    columnHeaders,
    rowHeaders,
    columnDimensionHeaders,
    rowDimensionHeaders,
    customFunctions
  }
) {
  // Build rows headers array
  const rowsRange = [
    Math.min(selectedCellStart[1], selectedCellEnd[1]),
    Math.max(selectedCellStart[1], selectedCellEnd[1]) + 1
  ];
  const rowHeaderLeafs = rowHeaders
    .slice(...rowsRange)
    .map(headers => headers[headers.length - 1]);
  const rows = rowHeaderLeafs.map(header => {
    const res = [];
    let currentHeader = header;
    while (currentHeader) {
      res.unshift(currentHeader.caption);
      currentHeader = currentHeader.parent;
    }
    return res;
  });

  // Build columns headers array
  const columnsRange = [
    Math.min(selectedCellStart[0], selectedCellEnd[0]),
    Math.max(selectedCellStart[0], selectedCellEnd[0]) + 1
  ];
  const columnHeaderLeafs = columnHeaders
    .slice(...columnsRange)
    .map(headers => headers[headers.length - 1]);
  const columns = columnHeaderLeafs.map(header => {
    const res = [];
    let currentHeader = header;
    while (currentHeader) {
      res.unshift(currentHeader.caption);
      currentHeader = currentHeader.parent;
    }
    return res;
  });

  // Build data array
  const cells = rowHeaderLeafs.map(rowHeader =>
    columnHeaderLeafs
      // get getCellValue from the store
      // maybe better to go without datacell and get caption directly
      // be careful about rendering function though
      .map(
        columnHeader =>
          new DataCell(
            getCellValue,
            true,
            rowHeader,
            columnHeader,
            customFunctions
          ).value
      ));
  const rowDimensions = rowDimensionHeaders.map(header => header.value.caption);
  const columnDimensions = columnDimensionHeaders.map(
    header => header.value.caption
  );

  // Format data to text
  let output = '';
  // First rows with only the dimension and columns headers
  const depth = columns[0].length;
  const width = rows[0].length;
  for (let y = 0; y < depth; y += 1) {
    for (let x = 0; x < width; x += 1) {
      if (x === width - 1 && y < depth - 1) {
        output += `${replaceNullAndUndefined(columnDimensions[y])}\t`;
      } else if (y === depth - 1 && x < width - 1) {
        output += `${replaceNullAndUndefined(rowDimensions[x])}\t`;
      } else if (y === depth - 1 && x === width - 1) {
        // Handle corner case
        // Dimension header in bottom right cell can refer to a column header
        // or a row header depending on data headers location
        if (dataHeadersLocation === 'columns') {
          output += `${replaceNullAndUndefined(rowDimensions[x])}\t`;
        } else {
          output += `${replaceNullAndUndefined(columnDimensions[y])}\t`;
        }
      } else {
        output += '\t';
      }
    }
    output = columns.reduce(
      (accumulator, column) =>
        `${accumulator}${replaceNullAndUndefined(column[y])}\t`,
      output
    );
    output = output.slice(0, -1);
    output += '\n';
  }
  // Other rows with rows headers and data
  for (let y = 0; y < rows.length; y += 1) {
    for (let x = 0; x < width; x += 1) {
      output += `${replaceNullAndUndefined(rows[y][x])}\t`;
    }
    for (let x = 0; x < columnHeaderLeafs.length; x += 1) {
      output += `${replaceNullAndUndefined(cells[y][x])}\t`;
    }
    output = output.slice(0, -1);
    output += '\n';
  }
  output = output.slice(0, -1);
  return output;
}

export default function copy(
  {
    selectedCellStart,
    selectedCellEnd,
    rowHeaders,
    columnHeaders,
    getCellValue,
    dataHeadersLocation,
    columnDimensionHeaders,
    rowDimensionHeaders,
    customFunctions
  }
) {
  try {
    const bodyElement = document.getElementsByTagName('body')[0];
    const clipboardTextArea = document.createElement('textarea');
    clipboardTextArea.style.position = 'absolute';
    clipboardTextArea.style.left = '-10000px';
    bodyElement.appendChild(clipboardTextArea);
    clipboardTextArea.innerHTML = getSelectedText({
      selectedCellStart,
      selectedCellEnd,
      getCellValue,
      rowHeaders,
      columnHeaders,
      dataHeadersLocation,
      columnDimensionHeaders,
      rowDimensionHeaders,
      customFunctions
    });
    clipboardTextArea.select();
    window.setTimeout(
      () => {
        bodyElement.removeChild(clipboardTextArea);
      },
      0
    );
  } catch (error) {
    /* eslint-disable no-console */
    console.error('error during copy', error);
    /* eslint-enable */
  }
}
