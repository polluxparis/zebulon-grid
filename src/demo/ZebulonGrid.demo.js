import React, { Component } from "react";
// import ReactDOM from "react-dom";
import ZebulonGrid from "../pivotGrid";
import "react-resizable/css/styles.css";
import { ResizableBox } from "react-resizable";
import {
  getPromiseMockDatasource,
  basicConfig,
  getRandomMockDatasource
  // overwritedData
} from "./mock";
import { configurationFunctions } from "./configurationFunctions";
import { menuFunctions } from "./menuFunctions";
import { customConfigurationFunctions, customMenuFunctions } from "./demo";
import { exportFile } from "../pivotGrid/services/copyService";

class ZebulonGridDemo extends Component {
  constructor(props) {
    super(props);
    this.options = [200, 40, 3];
    this.state = {
      // focusCell: [],
      data: getPromiseMockDatasource(1, ...this.options),
      pushedData: [],
      configuration: basicConfig({ onEdit: this.onEdit }),
      configurationFunctions,
      menuFunctions,
      sizes: {
        height: 600,
        width: 1000
      },
      actionContent: null
    };
    this.bigDataSet = false;
    this.data = [];
  }
  // componentDidUpdate(prevProps) {
  //   const element = document.getElementById(
  //     `input ${this.props.selectedRange.end.rows} - ${this.props
  //       .selectedRange.end.columns}`
  //   );
  //   if (element) {
  //     element.select();
  //   }
  //   console.log("demo");
  // }
  handleDataSetOption = () => {
    this.bigDataSet = !this.bigDataSet;
    this.options = this.bigDataSet ? [500, 400, 5] : [200, 40, 3];
    // const node = document.getElementById("toto");
    this.setState({
      data: getPromiseMockDatasource(1, ...this.options),
      actionContent: this.bigDataSet ? (
        <div>
          <div>Reload a 1 million rows dataset :</div>
          <div>500 Totos * 400 Titis * 5 Tutus</div>
          <div>
            N.B. Performances are linked to the length in axis, so if you set
            Toto, Titi and Tutu in rows, you'll get 1M available rows in the
            grid and it may be a little slow; just try to set Tutu in columns
            (row / 5)
          </div>
        </div>
      ) : (
        <div>
          <div>Reload a 24000 rows dataset :</div>
          <div>200 Totos * 40 Titis * 3 Tutus</div>
        </div>
      )
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
      this.setState(
        customConfigurationFunctions(
          this.state.configurationFunctions,
          this.state.configuration
        )
      );
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
      this.setState(
        customMenuFunctions(this.state.menuFunctions, (type, action) =>
          this.setState({ actionContent: action })
        )
      );
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

  onEdit = data => {
    this.setState({
      actionContent: `Old value: ${data._oldValue} - New value:${data._newValue}`
    });
    this.data.push(data);
  };
  onSave = () => {
    exportFile(JSON.stringify(this.data), "toto.json");
  };
  onLoad = () => {
    // getFileObject("file:///c:/Users/thomas/Downloads/toto.json", fileObject => {
    //   console.log(fileObject);
    // });
    const file = document.getElementById("file-selector").files[0];
    const reader = new FileReader();
    reader.onload = e => {
      this.object = eval(reader.result);
      console.log(reader.result);
    };
    reader.readAsText(file);
  };
  onClickMenu = (e, id) => {
    if (e.button === 2) {
      e.preventDefault();
      e.persist();
      e.stopPropagation();
      // e.button = 0;
      this.setState({
        menuVisible: !this.state.menuVisible,
        menuParent: { top: e.target.offsetTop, left: e.target.offsetLeft }
      });
      console.log("click menu", e.target, id);
    }
    return false;
  };
  // <ContextualMenu key="contextual-menu" getMenu={getMenu} />
  //  // <ContextualMenuClient menuId={1}>
  //   <div id="-2" style={{ height: 30, width: 30, border: "solid" }} />
  // </ContextualMenuClient>
  render() {
    return (
      <div id="zebulon" style={{ fontFamily: "sans-serif" }}>
        <div style={{ height: 50 }}>
          <input
            type="checkbox"
            id="configuration"
            onChange={() => this.setState({ display: !this.state.display })}
            checked={this.state.display}
          />
          <label htmlFor="configuration">Configuration</label>
        </div>
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
            ref={ref => (this.zebulon = ref)}
            display={this.state.display ? "configuration" : "pivotgrid"}
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
          <div style={{ marginRight: ".5em" }}>
            <input
              id="option"
              checked={this.bigDataSet}
              type="checkbox"
              onChange={this.handleDataSetOption}
            />
            <label htmlFor="option">Try a 1M rows dataset</label>
          </div>
          <input
            type="file"
            id="file-selector"
            onChange={this.onLoad}
            style={{ marginRight: ".5em", display: "none" }}
          />
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
// (<button
//   style={{ marginRight: ".5em" }}
//   type="button"
//   onClick={this.onSave}
// >
//   Save
// </button>
// <button
//   style={{ marginRight: ".5em" }}
//   type="button"
//   onClick={() => document.getElementById("file-selector").click()}
// >
//   Load
// </button>)
