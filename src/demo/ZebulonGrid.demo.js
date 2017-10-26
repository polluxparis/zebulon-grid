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
const MEASURE_ID = "__measures__";
class ZebulonGridDemo extends Component {
  constructor(props) {
    super(props);
    this.options = [100, 100, 5];
    this.state = {
      // focusCell: [],
      data: getPromiseMockDatasource(1, ...this.options),
      pushedData: [],
      configuration: basicConfig,
      configurationFunctions,
      menuFunctions,
      sizes: {
        height: 600,
        width: 1000
      },
      actionContent: null
    };
    this.bigDataSet = false;
  }
  handleDataSetOption = () => {
    this.bigDataSet = !this.bigDataSet;
    this.options = this.bigDataSet ? [500, 400, 5] : [200, 40, 3];
    this.setState({
      data: getPromiseMockDatasource(1, ...this.options),
      actionContent: this.bigDataSet ? "Reload a 1 million rows dataset" : null
    });
  };
  pushData = () => {
    this.setState({
      pushedData: getRandomMockDatasource(20, ...this.options),
      actionContent: (
        <div>
          <div>Push randomly new rows in the dataset. </div>
          <div>
            N.B. Only changes diplayed in the grid will be visible, so it may be
            prefereable to remove (using the contextual menu on the dimension
            header) the dimension Titi by example (or collapse all dimension
            Toto).
          </div>
        </div>
      )
    });
  };
  setCustomConfigurationFunctions = () => {
    if (this.state.configurationFunctions === configurationFunctions) {
      const configurationFunctions = {
        formats: {
          ...this.state.configurationFunctions.formats,
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
          ...this.state.configurationFunctions.accessors,
          price: row => ({ amt: row.amt, qty: row.qty })
        },
        aggregations: {
          ...this.state.configurationFunctions.aggregations,
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
        ...basicConfig,
        measures: [
          ...basicConfig.measures,
          {
            id: "price",
            caption: "Price",
            aggregation: "price",
            valueAccessor: "price",
            format: "price"
          }
        ],
        dimensions: [
          basicConfig.dimensions[0],
          {
            id: "titi",
            caption: "Titi",
            keyAccessor: "titi",
            labelAccessor: "titi_lb",
            sort: { keyAccessor: "titi", custom: "titi" }
          },
          ...basicConfig.dimensions.slice(2, 5)
        ],
        activeMeasures: [...basicConfig.activeMeasures, "price"]
      };
      this.setState({
        configurationFunctions,
        configuration,
        actionContent: (
          <div>
            <div>Add a new measure (Price) with new : </div>
            <div> - custom accessor function ( amounts and quantities)</div>
            <div>
              - custom agregation function ( average of amounts weighted by
              quantities)
            </div>
            <div> - custom format ( blue + $ symbol)</div>
            <div>
              Add a (dummy) sort function for dimension Titi : ordered by even
              ids, then odd ids.
            </div>
          </div>
        )
      });
    } else {
      this.setState({
        configurationFunctions,
        configuration: basicConfig,
        actionContent: null
      });
    }
  };
  cellDisplay = cell => {
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
    const value = <li key={"value"}>{cell.value}</li>;
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
  setCustomFunctions = () => {
    if (this.state.menuFunctions === menuFunctions) {
      const menuFunctions = {
        dataCellFunctions: {
          ...this.state.menuFunctions.dataCellFunctions,
          cell: {
            code: "cell",
            caption: "Cell function",
            type: "MenuItem",
            function: cell =>
              this.setState({
                actionContent: this.cellDisplay(cell)
              })
          }
        },
        rangeFunctions: {
          ...this.state.menuFunctions.rangeFunctions,
          range: {
            code: "range",
            caption: "Custom range function",
            type: "MenuItem",
            function: range => {
              const cellFrom = {
                dimensions: range.rows[0].concat(range.columns[0]),
                value: range.values[0][0]
              };
              const cellTo = {
                dimensions: range.rows[range.rows.length - 1].concat(
                  range.columns[range.columns.length - 1]
                ),
                value:
                  range.values[range.rows.length - 1][range.columns.length - 1]
              };
              const divFrom = this.cellDisplay(cellFrom);
              const divTo = this.cellDisplay(cellTo);
              let values = {};
              if (range.measureHeadersAxis === "columns") {
                values = range.columns.reduce(
                  (values, measure, indexColumn) => {
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
                  },
                  {}
                );
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
                <li key={measure}>{`${measure} : ${values[measure]}`}</li>
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
              this.setState({
                actionContent: div
              });
              console.log("range", range, values);
            }
          }
        },
        gridFunctions: {
          ...this.state.menuFunctions.gridFunctions,
          grid: {
            code: "grid",
            type: "MenuItem",
            caption: "Custom grid function",
            // function: () => <div>toto</div>
            function: grid => console.log("grid", grid)
          }
        }
      };
      this.setState({
        menuFunctions,
        actionContent: (
          <div>
            <div>
              Add custom menu functions for cell (cells under the right ckick),
              selected range or grid, accesible by the contextual menu on the
              data cells area.
            </div>
            <div>The first cell function is called on doubleclick too.</div>
          </div>
        )
      });
    } else {
      this.setState({ menuFunctions, actionContent: null });
    }
  };
  onResize = (e, data) => {
    this.setState({
      sizes: {
        ...this.state.sizes,
        height: data.size.height,
        width: data.size.width
      }
    });
  };
  render() {
    return (
      <div style={{ fontFamily: "sans-serif" }}>
        <ResizableBox
          height={this.state.sizes.height}
          width={this.state.sizes.width}
          onResize={this.onResize}
        >
          <ZebulonGrid
            configuration={this.state.configuration}
            data={this.state.data}
            pushedData={this.state.pushedData}
            menuFunctions={this.state.menuFunctions}
            configurationFunctions={this.state.configurationFunctions}
            sizes={this.state.sizes}
            ref={ref => {
              this.grid = ref;
            }}
          />
        </ResizableBox>
        <div
          style={{
            display: "flex",
            marginTop: ".5em",
            width: this.state.sizes.width
          }}
        >
          <button style={{ marginRight: ".5em" }} onClick={this.pushData}>
            Push data
          </button>
          <button
            style={{ marginRight: ".5em" }}
            onClick={this.setCustomConfigurationFunctions}
          >
            {`${this.state.configurationFunctions === configurationFunctions
              ? "Add"
              : "Remove"} custom configuration functions`}
          </button>
          <button
            style={{ marginRight: ".5em" }}
            onClick={this.setCustomFunctions}
          >
            {`${this.state.menuFunctions === menuFunctions
              ? "Add"
              : "Remove"} custom menu functions`}
          </button>
          <div>
            <input
              id="option"
              checked={this.bigDataSet}
              type="checkbox"
              onChange={this.handleDataSetOption}
            />
            <label htmlFor="option">Try a 1M rows dataset</label>
          </div>
        </div>
        <div
          style={{
            height: 100,
            width: this.state.sizes.width,
            fontSize: "smaller",
            marginTop: ".5em",
            border: "solid .05em"
          }}
        >
          {this.state.actionContent}
        </div>
        <div
          style={{
            fontFamily: "sans-serif",
            fontSize: "smaller",
            marginTop: "1em"
          }}
        >
          <div>
            <div style={{ fontWeight: "bold" }}> Key board and mouse: </div>
            <div style={{ paddingLeft: "2em" }}>
              <div>
                Ctrl+A : select all, Ctrl + : zoom in, Ctrl - : zoom out.
              </div>
              <div>
                Up, Down, Right, Left, PageDown, PageUp, Home, End to move
                selection.
              </div>
              <div>
                +Shift to extend selection, +Alt to move in column axis for
                PageDown, PageUp, Home, End.
              </div>
              <div>Mouse down and move over to select a range.</div>
              <div>Mouse wheel to scroll. +Alt to scroll in column</div>
            </div>
            <div style={{ fontWeight: "bold" }}> Dimension headers: </div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Click: toggle sort direction.</div>
              <div>
                Drag and drop on an other dimension header to reorder or change
                dimension axis.
              </div>
              <div>
                Collapse or expand button to hide or show attribute dimensions
                (in Italic).
              </div>
              <div>
                Right and bottom handle: drag and drop to resize rows or columns
                headers.
              </div>
              <div>
                Right click context menu to reverse order, filter, expand and
                collapse all, remove or add dimension, subtotals or grand total.
              </div>
            </div>
            <div style={{ fontWeight: "bold" }}>
              Measure, column and row headers:
            </div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Click: select children headers columns or rows.</div>
              <div>
                Collapse or expand button to hide or show children headers and
                recompute measures.
              </div>
              <div>
                Right and bottom handle: drag and drop to resize rows or
                columns.
              </div>
              <div>
                Drag and drop measure on an other measure to reorder them, on a
                dimension header to change measure axis.
              </div>
              <div>
                Right click context menu on measures to remove, add a measure or
                change the axis.
              </div>
            </div>
            <div style={{ fontWeight: "bold" }}>Cells and ranges:</div>
            <div style={{ paddingLeft: "2em" }}>
              <div> Ctrl+C to copy selected range</div>
              <div> Double click for drilldown main custom function</div>
              <div>
                Right click context menu to call cell, range or general custom
                functions,to filter any dimension or to modify some
                configuration parameters (as changing subtotals position).
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default ZebulonGridDemo;
