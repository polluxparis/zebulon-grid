import React, { Component } from "react";

import ZebulonGrid from "../pivotGrid";
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
import {
  getPromiseMockDatasource,
  basicConfig,
  getRandomMockDatasource
} from "./mock";
import { configurationFunctions } from "./configurationFunctions";
import { menuFunctions } from "./menuFunctions";
export const MEASURE_ID = "__measures__";
export const customConfigurationFunctions = (
  prevConfigurationFunctions,
  prevConfiguration
) => {
  const configurationFunctions = {
    formats: {
      ...prevConfigurationFunctions.formats,
      price: value => {
        if (Number.isFinite(value)) {
          return (
            <div style={{ color: "blue", textAlign: "right" }}>
              {`${Number(value).toFixed(2)} $`}
            </div>
          );
        }
        return value;
      }
    },
    accessors: {
      ...prevConfigurationFunctions.accessors,
      price: row => ({ amt: row.amt, qty: row.qty })
    },
    aggregations: {
      ...prevConfigurationFunctions.aggregations,
      price: values => {
        const price = values.reduce(
          (price, value) => {
            price.amt += value.amt;
            price.qty += value.qty;
            return price;
          },
          { amt: null, qty: null }
        );
        return price.amt === null || price.qty === null
          ? null
          : price.amt / price.qty;
      }
    },
    sorts: {
      titi: (a, b) => {
        const x =
          a % 2 < b % 2 || (a % 2 === b % 2 && a < b)
            ? -1
            : !(a % 2 === b % 2 && a === b) + 0;
        return x;
      }
    }
  };
  const configuration = {
    ...prevConfiguration,
    measures: [
      ...prevConfiguration.measures,
      {
        id: "price",
        caption: "Price",
        aggregation: "price",
        valueAccessor: "price",
        format: "price"
      }
    ],
    dimensions: [
      prevConfiguration.dimensions[0],
      {
        id: "titi",
        caption: "Titi",
        keyAccessor: "titi",
        labelAccessor: "titi_lb",
        sort: { keyAccessor: "titi", custom: "titi" }
      },
      ...prevConfiguration.dimensions.slice(2, 5)
    ],
    activeMeasures: [...prevConfiguration.activeMeasures, "price"]
  };
  const actionContent = (
    <div>
      <div>Add a new measure (Price) with new : </div>
      <div> - custom accessor function ( amounts and quantities)</div>
      <div>
        - custom agregation function ( average of amounts weighted by
        quantities)
      </div>
      <div> - custom format ( blue + $ symbol)</div>
      <div>
        Add a (dummy) sort function for dimension Titi : ordered by even ids,
        then odd ids.
      </div>
    </div>
  );
  return { configurationFunctions, configuration, actionContent };
};
const cellDisplay = cell => {
  console.log(cell);
  const rows = cell.dimensions
    .filter(dimension => dimension.axis === "rows")
    .map(dimension => (
      <li key={dimension.dimension.id}>
        {`${dimension.dimension.id === MEASURE_ID
          ? "Measure"
          : dimension.dimension.caption} : ${dimension.cell.caption}`}
      </li>
    ));
  const columns = cell.dimensions
    .filter(dimension => dimension.axis === "columns")
    .map(dimension => (
      <li key={dimension.dimension.id}>
        {`${dimension.dimension.id === MEASURE_ID
          ? "Measure"
          : dimension.dimension.caption} : ${dimension.cell.caption}`}
      </li>
    ));
  const value = <li key={"value"}>{Math.round(cell.value * 10000) / 10000}</li>;
  return (
    <div style={{ display: "flex" }}>
      <div style={{ width: 150 }}>
        <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>Rows</div>
        <ul style={{ paddingLeft: "20px" }}> {rows}</ul>
      </div>
      <div style={{ width: 150 }}>
        <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>Columns</div>
        <ul style={{ paddingLeft: "20px" }}> {columns}</ul>
      </div>
      <div style={{ width: 90 }}>
        <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>Value</div>
        <ul style={{ paddingLeft: "20px" }}> {value}</ul>
      </div>
    </div>
  );
};
const rangeDisplay = range => {
  const cellFrom = {
    dimensions: range.rows[0].concat(range.columns[0]),
    value: range.values[0][0]
  };
  const cellTo = {
    dimensions: range.rows[range.rows.length - 1].concat(
      range.columns[range.columns.length - 1]
    ),
    value: range.values[range.rows.length - 1][range.columns.length - 1]
  };
  const divFrom = cellDisplay(cellFrom);
  const divTo = cellDisplay(cellTo);
  let values = {};
  if (range.measureHeadersAxis === "columns") {
    values = range.columns.reduce((values, measure, indexColumn) => {
      if (values[measure.leaf.caption] === undefined) {
        values[measure.leaf.caption] = 0;
      }
      values[
        measure.leaf.caption
      ] += range.rows.reduce((value, dimension, indexRow) => {
        value += range.values[indexRow][indexColumn];
        return value;
      }, 0);
      return values;
    }, {});
  } else {
    values = range.rows.reduce((values, measure, indexRow) => {
      if (values[measure.leaf.caption] === undefined) {
        values[measure.leaf.caption] = 0;
      }
      values[
        measure.leaf.caption
      ] += range.columns.reduce((value, dimension, indexColumn) => {
        value += range.values[indexRow][indexColumn];
        return value;
      }, 0);
      return values;
    }, {});
  }
  const sums = Object.keys(values).map(measure => (
    <li key={measure}>{`${measure} : ${Math.round(values[measure] * 10000) /
      10000}`}</li>
  ));
  const divValues = (
    <div
      style={{
        width: 160,
        borderLeft: "solid 0.02em",
        height: 100
      }}
    >
      <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>
        Sum of values
      </div>
      <ul style={{ paddingLeft: "20px" }}> {sums}</ul>
    </div>
  );
  const div = (
    <div style={{ display: "flex" }}>
      <div style={{ borderLeft: "solid 0.02em", height: 100 }}>
        <div
          style={{
            paddingLeft: "5px",
            fontWeight: "bold",
            textAlign: "left"
          }}
        >
          From
        </div>
        {divFrom}
      </div>
      <div style={{ borderLeft: "solid 0.02em", height: 100 }}>
        <div
          style={{
            paddingLeft: "5px",
            fontWeight: "bold",
            textAlign: "Left"
          }}
        >
          To
        </div>
        {divTo}
      </div>
      {divValues}
    </div>
  );
  return div;
};
const gridDisplay = grid => {};
export const customMenuFunctions = (prevMenuFunctions, callback) => {
  const menuFunctions = {
    dataCellFunctions: {
      ...prevMenuFunctions.dataCellFunctions,
      cell: {
        code: "cell",
        caption: "Cell function",
        type: "MenuItem",
        function: cell => callback("cell", cellDisplay(cell))
      }
    },
    rangeFunctions: {
      ...prevMenuFunctions.rangeFunctions,
      range: {
        code: "range",
        caption: "Custom range function",
        type: "MenuItem",
        function: range => callback("range", rangeDisplay(range))
      }
    },
    gridFunctions: {
      ...prevMenuFunctions.gridFunctions,
      grid: {
        code: "grid",
        type: "MenuItem",
        caption: "Custom grid function",
        // function: () => <div>toto</div>
        function: grid => callback("grid", gridDisplay(grid))
      }
    }
  };
  const actionContent = (
    <div>
      <div>
        Add custom menu functions for cell (cells under the right ckick),
        selected range or grid, accesible by the contextual menu on the data
        cells area.
      </div>
      <div>The first cell function is called on doubleclick too.</div>
    </div>
  );
  return { menuFunctions, actionContent };
};

//   return { menuFunctions, actionContent };
// };
