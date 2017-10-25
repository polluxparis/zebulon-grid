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
          price: (accessor, dataIndexes, data) => {
            const values = { amt: 0, qty: 0 };
            if (dataIndexes.length) {
              dataIndexes.forEach(index => {
                const v = accessor(data[index]);
                values.amt += v.amt;
                values.qty += v.qty;
              });
            }
            return values.amt === 0 && values.qty === 0
              ? null
              : values.amt / values.qty;
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
  setCustomFunctions = () => {
    if (this.state.menuFunctions === menuFunctions) {
      const menuFunctions = {
        dataCellFunctions: {
          ...this.state.menuFunctions.dataCellFunctions,
          cell: {
            code: "cell",
            caption: "Cell function",
            type: "MenuItem",
            function: cell => {
              const rows = cell.dimensions
                .filter(dimension => dimension.axis === "rows")
                .map(dimension => (
                  <li>
                    {`${dimension.dimension.caption} : ${dimension.cell
                      .caption}`}
                  </li>
                ));
              const columns = cell.dimensions
                .filter(dimension => dimension.axis === "columns")
                .map(dimension => (
                  <li>
                    {`${dimension.dimension.caption} : ${dimension.cell
                      .caption}`}
                  </li>
                ));
              this.setState({
                actionContent: (
                  <div style={{ display: "flex" }}>
                    <div style={{ width: 200 }}>
                      <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>
                        {" "}
                        Rows{" "}
                      </div>
                      <ul style={{ paddingLeft: "15px" }}> {rows}</ul>
                    </div>
                    <div style={{ width: 200 }}>
                      <div style={{ paddingLeft: "5px", fontWeight: "bold" }}>
                        {" "}
                        Columns{" "}
                      </div>
                      <ul style={{ paddingLeft: "15px" }}> {columns}</ul>
                    </div>
                    <div style={{ fontWeight: "bold" }}>
                      {`Value : ${cell.value}`}
                    </div>
                  </div>
                )
              });
            }
          }
        },
        rangeFunctions: {
          ...this.state.menuFunctions.rangeFunctions,
          range: {
            code: "range",
            caption: "Custom range function",
            type: "MenuItem",
            function: range => {
              console.log("range", range);
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
