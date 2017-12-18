import { MEASURE_ID } from "../constants";
import { getFilteredIndex } from "../selectors";
// import { FileSaver } from "file-saver";

function replaceNullAndUndefined(val) {
  if (val === null || val === undefined) {
    return "";
  }
  return JSON.stringify(val);
}
export const getElementsToPaste = ({
  rowLeaves,
  columnLeaves,
  measures,
  getCellValue,
  buidData,
  clipboard,
  cell
}) => {
  try {
    let { columnIndex, rowIndex } = cell;
    const lines = clipboard.replace("\r", "").split("\n");
    lines.pop();
    const cells = lines.map(line => line.split("\t"));
    const nRows = lines.length,
      nColumns = cells[0].length;
    let iRow = 0,
      iColumn = 0,
      measure,
      data = [];
    while (iRow < nRows && iRow < rowLeaves.leaves.length) {
      const rowLeaf = rowLeaves.leaves[rowIndex];
      if (rowLeaf.isVisible) {
        if (rowLeaf.dimensionId === MEASURE_ID) {
          measure = measures[rowLeaf.id];
        }
        while (iColumn < nColumns && iColumn < columnLeaves.leaves.length) {
          const columnLeaf = columnLeaves.leaves[columnIndex];
          if (columnLeaf.isVisible) {
            if (columnLeaf.dimensionId === MEASURE_ID) {
              measure = measures[columnLeaf.id];
            }
            const oldValue = getCellValue(
                measure.valueAccessor,
                rowLeaf.dataIndexes,
                columnLeaf.dataIndexes,
                measure.aggregation
              ).value,
              newValue =
                cells[iRow][iColumn] === ""
                  ? null
                  : Number(cells[iRow][iColumn]);
            if (isNaN(newValue)) {
              console.log("not numeric data", cells[iRow][iColumn]);
              return [];
            }
            data.push(buidData(rowLeaf, columnLeaf, oldValue, newValue));
            iColumn++;
          }
          columnIndex++;
        }
        iRow++;
        iColumn = 0;
        columnIndex = cell.columnIndex;
      }
      rowIndex++;
    }
    return data;
  } catch (error) {
    /* eslint-disable no-console */
    console.error("error during copy", error);
    return [];
    /* eslint-enable */
  }
};

export const getSelectedElements = ({
  range,
  rowLeaves,
  columnLeaves,
  rowDimensions,
  columnDimensions,
  measures,
  measureHeadersAxis,
  getCellValue,
  getCellDimensionInfos
}) => {
  // const mc = measureHeadersAxis === "columns";
  // const mr = measureHeadersAxis === "rows";
  // Build headers array
  let selectedRowLeaves = rowLeaves.leaves,
    selectedColumnLeaves = columnLeaves.leaves;
  if (range) {
    const rowsRange = [
      Math.min(range.start.rows, range.end.rows),
      Math.max(range.start.rows, range.end.rows) + 1
    ];
    selectedRowLeaves = selectedRowLeaves.slice(...rowsRange);
    const columnsRange = [
      Math.min(range.start.columns, range.end.columns),
      Math.max(range.start.columns, range.end.columns) + 1
    ];
    selectedColumnLeaves = selectedColumnLeaves.slice(...columnsRange);
  }
  selectedRowLeaves = selectedRowLeaves.filter(leaf => leaf.isVisible);
  selectedColumnLeaves = selectedColumnLeaves.filter(leaf => leaf.isVisible);
  // get rows captions
  const rows = selectedRowLeaves.map(leaf => {
    leaf.dataIndexes = getFilteredIndex(leaf);
    return getCellDimensionInfos(rowDimensions, "rows", leaf, measures, []);
  });
  // get columns captions
  const columns = selectedColumnLeaves.map(leaf => {
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
  const cells = selectedRowLeaves.map((rowLeaf, index) => {
    if (measureHeadersAxis === "rows") {
      measure = measures[rowLeaf.id];
    }
    return selectedColumnLeaves.map(columnLeaf => {
      if (measureHeadersAxis === "columns") {
        measure = measures[columnLeaf.id];
      }
      return getCellValue(
        measure.valueAccessor,
        rowLeaf.dataIndexes,
        columnLeaf.dataIndexes,
        measure.aggregation
      );
    });
  });
  return {
    rows,
    columns,
    cells,
    rowDimensions,
    columnDimensions,
    measureHeadersAxis
  };
};
export const getSelectedText = (elements, type) => {
  const columnSeparator = type === "csv" ? "," : "\t";
  const {
    rows,
    columns,
    cells,
    rowDimensions,
    columnDimensions,
    measureHeadersAxis
  } = elements;
  const mc = measureHeadersAxis === "columns";
  const mr = measureHeadersAxis === "rows";
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
      outputHeaders += `${replaceNullAndUndefined(caption)}${columnSeparator}`;
    }
    // column headers
    for (let x = 0; x < columns.length; x += 1) {
      outputHeaders += `${replaceNullAndUndefined(
        columns[x][y].cell.caption
      )}${columnSeparator}`;
    }
    outputHeaders = outputHeaders.slice(0, -1);
    output.push(outputHeaders);
  }
  // t2 = Date.now();
  // console.log("columns headers", t2 - t);
  // t = t2;
  let outputRow = "";
  for (let y = 0; y < rows.length; y += 1) {
    outputRow = "";
    // row headers
    for (let x = 0; x < width; x += 1) {
      outputRow += `${replaceNullAndUndefined(
        rows[y][x].cell.caption
      )}${columnSeparator}`;
    }
    // data cells
    for (let x = 0; x < columns.length; x += 1) {
      outputRow += `${replaceNullAndUndefined(
        cells[y][x].value
      )}${columnSeparator}`;
    }
    outputRow = outputRow.slice(0, -1);
    output.push(outputRow);
  }

  // t2 = Date.now();
  // console.log("rows", t2 - t, t2);
  // t = t2;

  // output = output.slice(0, -1);
  return output.join("\n");
};

export const copy = text => {
  try {
    const bodyElement = document.getElementsByTagName("body")[0];
    const clipboardTextArea = document.createElement("textarea");
    clipboardTextArea.style.position = "absolute";
    clipboardTextArea.style.left = "-10000px";
    bodyElement.appendChild(clipboardTextArea);
    clipboardTextArea.innerHTML = text;
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
};
export const exportFile = (content, fileName, mime) => {
  if (mime == null) {
    mime = "text/csv";
  }
  const blob = new Blob([content], { type: mime });
  const a = document.createElement("a");
  a.download = fileName;
  a.href = window.URL.createObjectURL(blob);
  a.dataset.downloadurl = [mime, a.download, a.href].join(":");
  // const e = document.createEvent("MouseEvents");
  // e.initMouseEvent(
  //   "click",
  //   true,
  //   false,
  //   window,
  //   0,
  //   0,
  //   0,
  //   0,
  //   0,
  //   false,
  //   false,
  //   false,
  //   false,
  //   0,
  //   null
  // );
  // return a.dispatchEvent(e);
};
const getFileBlob = (url, cb) => {
  var xhr = new XMLHttpRequest();
  xhr.onreadystatechange = function(event) {
    // XMLHttpRequest.DONE === 4
    if (this.readyState === XMLHttpRequest.DONE) {
      if (this.status === 200) {
        console.log("Réponse reçu: %s", this.responseText);
      } else {
        console.log(
          "Status de la réponse: %d (%s)",
          this.status,
          this.statusText
        );
      }
    }
  };
  xhr.open("GET", url, true);
  // xhr.responseType = "blob";
  // xhr.addEventListener("load", () => {
  //   cb(xhr.response);
  // });
  xhr.send(null);
};

const blobToFile = (blob, name) => {
  blob.lastModifiedDate = new Date();
  blob.name = name;
  return blob;
};

export const getFileObject = (filePathOrUrl, cb) => {
  getFileBlob(filePathOrUrl, blob => {
    const file = blobToFile(blob, "toto.json");
    const reader = new FileReader();
    reader.onload = e => cb(reader.result);
    reader.readAsText(file);
    // cb(blobToFile(blob, "toto.json"));
  });
};
