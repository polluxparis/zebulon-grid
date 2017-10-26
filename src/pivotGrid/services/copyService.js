import { MEASURE_ID } from "../constants";
import { getFilteredIndex } from "../selectors";

function replaceNullAndUndefined(val) {
  if (val === null || val === undefined) {
    return "";
  }
  return val;
}
// const getFilteredIndex = leaf => {
//   if (leaf.isParentCollapsed) {
//     return getFilteredIndex(leaf.parent);
//   } else {
//     return leaf.filteredIndexes;
//   }
// };
const getSelectedText = ({
  selectedRange,
  rowLeaves,
  columnLeaves,
  rowDimensions,
  columnDimensions,
  measures,
  measureHeadersAxis,
  getCellValue,
  getCellDimensionInfos
}) => {
  const mc = measureHeadersAxis === "columns";
  const mr = measureHeadersAxis === "rows";
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
  const selectedRowLeaves = rowLeaves.leaves
    .slice(...rowsRange)
    .filter(leaf => leaf.isVisible);

  // get rows captions
  const rowInfos = selectedRowLeaves.map(leaf => {
    leaf.dataIndexes = getFilteredIndex(leaf);
    return getCellDimensionInfos(rowDimensions, "rows", leaf, measures, []);
  });
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
  const selectedColumnLeaves = columnLeaves.leaves
    .slice(...columnsRange)
    .filter(leaf => leaf.isVisible);
  // get columns captions
  const columnsInfos = selectedColumnLeaves.map(leaf => {
    leaf.dataIndexes = getFilteredIndex(leaf);
    return getCellDimensionInfos(
      columnDimensions,
      "columns",
      leaf,
      measures,
      []
    );
  });
  // Build data array
  let measure;
  let t = Date.now(),
    t2;

  const cells = selectedRowLeaves.map((rowLeaf, index) => {
    if (measureHeadersAxis === "rows") {
      measure = measures[rowLeaf.id];
    }
    return (
      selectedColumnLeaves
        // get getCellValue from the store
        // maybe better to go without datacell and get caption directly
        // be careful about rendering function though
        .map(columnLeaf => {
          if (measureHeadersAxis === "columns") {
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
  t2 = Date.now();
  // console.log("cells", t2 - t);
  t = t2;
  // build string with corner headers and column headers

  const output = [];
  let caption, outputHeaders;
  // // First rows with only the dimensions (top left area) + columns headers
  const depth = columnDimensions.length;
  const width = rowDimensions.length;
  for (let y = 0; y < depth; y += 1) {
    outputHeaders = "";
    for (let x = 0; x < width; x += 1) {
      if (x === width - 1 && y < depth - mc) {
        caption = columnDimensions[y].caption;
      } else if (y === depth - 1 && x < width - mr) {
        caption = rowDimensions[x].caption;
      } else {
        caption = "";
      }
      outputHeaders += `${replaceNullAndUndefined(caption)}\t`;
    }
    // column headers
    for (let x = 0; x < columnsInfos.length; x += 1) {
      outputHeaders += `${replaceNullAndUndefined(
        columnsInfos[x][y].cell.caption
      )}\t`;
    }
    outputHeaders = outputHeaders.slice(0, -1);
    output.push(outputHeaders);
  }
  t2 = Date.now();
  // console.log("columns headers", t2 - t);
  t = t2;
  let outputRow = "";
  for (let y = 0; y < rowInfos.length; y += 1) {
    outputRow = "";
    // row headers
    for (let x = 0; x < width; x += 1) {
      outputRow += `${replaceNullAndUndefined(rowInfos[y][x].cell.caption)}\t`;
    }
    // data cells
    for (let x = 0; x < columnsInfos.length; x += 1) {
      outputRow += `${replaceNullAndUndefined(cells[y][x])}\t`;
    }
    outputRow = outputRow.slice(0, -1);
    output.push(outputRow);
  }

  t2 = Date.now();
  // console.log("rows", t2 - t, t2);
  t = t2;

  // output = output.slice(0, -1);
  return output.join("\n");
};

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
    const bodyElement = document.getElementsByTagName("body")[0];
    const clipboardTextArea = document.createElement("textarea");
    const measureHeadersAxis =
      columnDimensions[columnDimensions.length - 1].id === MEASURE_ID
        ? "columns"
        : "rows";
    clipboardTextArea.style.position = "absolute";
    clipboardTextArea.style.left = "-10000px";
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
    // console.log("rows0", Date.now());
    clipboardTextArea.select();
    // console.log("rows1", Date.now());
    window.setTimeout(() => {
      bodyElement.removeChild(clipboardTextArea);
    }, 0);
    // console.log("rows2", Date.now());
  } catch (error) {
    /* eslint-disable no-console */
    console.error("error during copy", error);
    /* eslint-enable */
  }
}
