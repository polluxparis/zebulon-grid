import React, { Component } from "react";
// import ReactDOM from "react-dom";
import ZebulonGrid from "../pivotGrid";
import "react-resizable/css/styles.css";
import "zebulon-controls/lib/index.css";
// import "zebulon-table/lib/index.css";
import { ResizableBox } from "react-resizable";
import {
  getPromiseMockDatasource,
  basicConfig,
  getRandomMockDatasource
  // overwritedData
} from "./mock";
// import { functions } from "zebulon-controls";
import { configurationFunctions } from "./configurationFunctions";
import { menuFunctions } from "./menuFunctions";
import { customConfigurationFunctions, customMenuFunctions } from "./demo";
import { exportFile } from "../pivotGrid/services/copyService";
import { metaDescriptions } from "zebulon-table";
import { utils, accessors, functions } from "zebulon-controls";
// console.log("zebulon-table", metaDescriptions, functions, functionsTable);
// import { functionsTable } from "../table/utils";
class ZebulonGridDemo extends Component {
  constructor(props) {
    super(props);
    this.options = [200, 40, 3];
    // this.options = [2, 5, 2];
    this.state = {
      // focusCell: [],
      data: getPromiseMockDatasource(1, ...this.options),
      pushedData: [],
      configuration: basicConfig({ onEdit: this.onEdit }),
      functions: functions.functions([accessors, configurationFunctions]),
      customConfigurationFunctions: false,
      menuFunctions,
      sizes: {
        height: 600,
        width: 1000,
        rowHeight: 25
      },
      actionContent: null,
      keyEvent: null,
      selectedCell: { rows: 0, columns: 0 }
    };
    this.bigDataSet = false;
    this.data = [];
    // configuration
    // this.functions = utils.mergeFunctions([accessors, functions], "dataset");
    this.meta = metaDescriptions(
      "dataset",
      undefined,
      this.state.functions
    ).dataset;
    // configuration
    this.params = {};
  }
  componentDidMount() {
    document.addEventListener("copy", this.handleKeyEvent);
    document.addEventListener("paste", this.handleKeyEvent);
    document.addEventListener("keydown", this.handleKeyEvent);
    window.addEventListener("beforeunload", this.handleKeyEvent);
  }
  componentWillUnmount() {
    document.removeEventListener("copy", this.handleKeyEvent);
    document.removeEventListener("paste", this.handleKeyEvent);
    document.removeEventListener("keydown", this.handleKeyEvent);
    window.removeEventListener("beforeunload", this.handleKeyEvent);
  }
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
      // selectedCell: { rows: 12, columns: 0 },
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
    if (!this.state.customConfigurationFunctions) {
      const custom = customConfigurationFunctions(
        configurationFunctions,
        this.zebulon.getStore()
      );
      this.setState({
        functions: functions.functions([
          accessors,
          custom.configurationFunctions
        ]),
        actionContent: custom.actionContent,
        configuration: custom.configuration,
        customConfigurationFunctions: !this.state.customConfigurationFunctions
      });
    } else {
      this.setState({
        functions: functions.functions([accessors, configurationFunctions]),
        configuration: basicConfig({ onEdit: this.onEdit }),
        actionContent: null,
        customConfigurationFunctions: !this.state.customConfigurationFunctions
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
  handleKeyEvent = e => {
    this.setState({ keyEvent: e });
    return true;
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
  // onLoad = e => {
  //   // getFileObject("file:///c:/Users/thomas/Downloads/toto.json", fileObject => {
  //   //   console.log(fileObject);
  //   // });
  //   const file = document.getElementById("file-selector").files[0];
  //   const reader = new FileReader();
  //   reader.onload = e => {
  //     console.log("toto", reader.result);
  //     reader.result;
  //   };
  //   reader.readAsText(file);
  //   // console.log(reader.result);
  // };
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
      // console.log("click menu", e.target, id);
    }
    return false;
  };
  onChangeDisplay = () => {
    if (!this.state.display && this.zebulon) {
      const configuration = this.state.configuration;
      const axis = this.zebulon.getStore().axis;
    }
    this.setState({ display: !this.state.display });
  };
  render() {
    const {
      sizes,
      keyEvent,
      configuration,
      data,
      pushedData,
      actionContent,
      display
    } = this.state;
    const sizes2 = { ...this.state.sizes };
    sizes2.height = sizes2.height - 30; // - (display ? 20 : 0);
    return (
      <div id="zebulon" style={{ fontFamily: "sans-serif" }}>
        <ResizableBox
          height={sizes.height}
          width={sizes.width}
          onResize={this.onResize}
        >
          <div style={{ height: 20 }}>
            <input
              type="checkbox"
              id="configuration"
              onChange={this.onChangeDisplay}
              checked={display || false}
            />
            <label htmlFor="configuration">Configuration</label>
          </div>
          <ZebulonGrid
            configuration={configuration}
            data={data}
            pushedData={pushedData}
            menuFunctions={this.state.menuFunctions}
            functions={this.state.functions}
            sizes={sizes2}
            ref={ref => (this.zebulon = ref)}
            display={display ? "configuration" : "pivotgrid"}
            meta={this.meta}
            // functions={this.functions}
            params={this.params}
            keyEvent={keyEvent}
            selectedCell={this.state.selectedCell}
          />
        </ResizableBox>
        <div
          style={{
            display: "flex",
            marginTop: ".5em",
            width: sizes.width
          }}
        >
          <button style={{ marginRight: ".5em" }} onClick={this.pushData}>
            Push data
          </button>
          <button
            style={{ marginRight: ".5em" }}
            onClick={this.setCustomConfigurationFunctions}
          >
            {`${!this.state.customConfigurationFunctions
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
        </div>
        <div
          style={{
            minHeight: 100,
            height: "fitContent",
            width: sizes.width,
            fontSize: "smaller",
            marginTop: ".5em",
            border: "solid .05em"
          }}
        >
          {actionContent}
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
