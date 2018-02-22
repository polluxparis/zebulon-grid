import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";
import PivotGrid from "./containers/PivotGrid";
import Chart from "./containers/Chart";
// import {
//   ZebulonTableAndConfiguration,
//   getFunction,
//   functions,
//   functionsTable
// } from "zebulon-table";
import { utils } from "zebulon-controls";
import reducer from "./reducers";
import * as aggregations from "./utils/aggregation";
import "./index.css";
import {
  defaultMenuFunctions,
  defaultSizes,
  applySizesToStore,
  applyConfigurationToStore,
  setData,
  pushData
} from "./utils/configuration";
import * as actions from "./actions";
class ZebulonGrid extends Component {
  constructor(props) {
    super(props);
    // this.buildFunctionsTable(props);
    // if (props.display === "configuration")
    //   this.buildDimensionsAndMesures(props.configuration);
    this.state = { sizes: props.sizes };
    this.zoomValue = props.sizes.zoom || 1;
  }
  buildDimensionsAndMesures = configuration => {
    this.measures = configuration.measures.map((measure, index) => {
      measure.index_ = index;
      return measure;
    });
    this.dimensions = configuration.dimensions.map((dimension, index) => {
      dimension.index_ = index;
      return dimension;
    });
    this.tabs = [
      {
        id: "measures",
        caption: "Measures",
        data: this.measures
      },
      {
        id: "dimensions",
        caption: "Dimensions",
        data: this.dimensions
      }
    ];
  };
  // buildFunctionsTable = props => {
  //   // if[props.functions
  //   this.functions = props.functions || functions;
  //   if (!Array.isArray(this.functions)) {
  //     this.functions = functionsTable(this.functions);
  //   }
  //   this.functions = [...this.functions];
  //   const ff = [];
  //   Object.keys(aggregations).forEach(f => {
  //     ff.push({
  //       id: f,
  //       caption: f,
  //       visibility: "global",
  //       tp: "aggregation",
  //       functionJS: aggregations[f]
  //     });
  //   });
  //   Object.keys(props.configurationFunctions).forEach(type => {
  //     if (type !== "analytics") {
  //       // a voir
  //       const tp = type.slice(0, type.length - 1);
  //       Object.keys(props.configurationFunctions[type]).forEach(f => {
  //         ff.push({
  //           id: f,
  //           caption: f,
  //           visibility: "dataset",
  //           tp,
  //           functionJS: props.configurationFunctions[type][f]
  //         });
  //       });
  //     }
  //   });
  //   ff.forEach(f => {
  //     const index = this.functions.findIndex(
  //       fct =>
  //         fct.id === f.id && fct.visibility === f.visibility && fct.tp === f.tp
  //     );
  //     if (index > -1) {
  //       this.functions[index].functionJS = f.functionJS;
  //     } else {
  //       this.functions.push(f);
  //     }
  //   });
  // };
  componentWillReceiveProps(nextProps) {
    const {
      data,
      configuration,
      configurationFunctions,
      pushedData,
      sizes,
      keyEvent,
      display
    } = nextProps;
    // this.sizes = { ...defaultSizes, ...sizes };

    if (sizes !== this.props.sizes) {
      applySizesToStore(this.store, sizes);
      if (sizes.zoom) {
        this.zoomValue = sizes.zoom;
      }
      this.setState({ sizes: { ...sizes, zoom: this.zoomValue } });
    }
    // if (
    //   display === "configuration" &&
    //   (this.props.display !== display ||
    //     this.props.configuration !== configuration)
    // )
    //   this.buildDimensionsAndMesures(configuration);
    if (this.props.configuration !== configuration) {
      applyConfigurationToStore(
        this.store,
        configuration,
        configurationFunctions,
        data === this.props.data ? null : data
      );
    } else if (this.props.data !== data) {
      setData(this.store, data);
    } else if (this.props.pushedData !== pushedData && pushedData.length) {
      pushData(this.store, pushedData);
    }
    if (this.props.keyEvent !== keyEvent) this.handleKeyEvent(keyEvent);
    // if (
    //   nextProps.configurationFunctions !== this.props.configurationFunctions ||
    //   nextProps.functions !== this.props.functions ||
    //   !this.functions
    // ) {
    //   this.buildFunctionsTable(nextProps);
    // }
  }
  componentWillMount() {
    const { data, configuration, configurationFunctions, sizes } = this.props;
    this.store = createStore(
      reducer,
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
    );
    applySizesToStore(this.store, { ...defaultSizes, ...sizes });
    applyConfigurationToStore(
      this.store,
      configuration,
      configurationFunctions,
      data
    );
  }
  componentDidMount() {
    if (!this.props.keyEvent === undefined) {
      document.addEventListener("copy", this.handleCopy);
      document.addEventListener("paste", this.handlePaste);
      document.addEventListener("keydown", this.handleKeyDown);
    }
  }
  componentWillUnmount() {
    if (!this.props.keyEvent === undefined) {
      document.removeEventListener("copy", this.handleCopy);
      document.removeEventListener("paste", this.handlePaste);
      document.removeEventListener("keydown", this.handleKeyDown);
    }
  }
  shouldComponentUpdate() {
    if (this.updateKey) {
      this.updateKey = false;
      return false;
    }
    return true;
  }
  handleKeyEvent = e => {
    const zoom = utils.isZoom(e);
    if (zoom) {
      e.preventDefault();
      this.zoomValue *= zoom === 1 ? 1.1 : 1 / 1.1;
      this.store.dispatch(actions.zoom(this.zoomValue));
      this.setState({ sizes: { ...this.state.sizes, zoom: this.zoomValue } });
      return;
    }
    if (!this.display) return;
    else if (e.type === "copy") this.handleCopy(e);
    else if (e.type === "paste") this.handlepaste(e);
    else if (e.type === "keydown") this.handleKeyDown(e);
    this.updateKey = true;
  };
  handleKeyDown = e => {
    if (
      !e.defaultPrevented &&
      this.display.handleKeyDown &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handleKeyDown(e);
    }
  };
  handleCopy = e => {
    if (
      !e.defaultPrevented &&
      this.display.handleCopy &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handleCopy(e);
    }
  };
  handlePaste = e => {
    if (
      !e.defaultPrevented &&
      this.display.handlePaste &&
      (this.props.isActive === undefined || this.props.isActive)
    ) {
      this.display.handlePaste(e);
    }
  };
  buildObject = (item, excludes) => {
    if (Array.isArray(item)) {
      return item.map(item => this.buildObject(item, excludes));
    } else if (typeof item === "object") {
      return Object.keys(item).reduce((acc, key) => {
        if (!utils.isNullOrUndefined(item[key]) && !excludes[key]) {
          acc[key] = this.buildObject(item[key], excludes);
        }
        return acc;
      }, {});
    } else {
      return item;
    }
  };

  // applyDimensions = () => {
  //   const { configuration, configurationFunctions } = this.props;
  //   const dimensions = this.buildObject(this.tabs[1].data, { index_: true });
  //   configuration.dimensions = dimensions;
  //   dimensions.forEach(dimension => {
  //     configurationFunctions.accessors[dimension.keyAccessor] = getFunction(
  //       this.functions,
  //       "dataset",
  //       "accessor",
  //       dimension.keyAccessor
  //     );
  //     configurationFunctions.accessors[dimension.labelAccessor] = getFunction(
  //       this.functions,
  //       "dataset",
  //       "accessor",
  //       dimension.labelAccessor
  //     );
  //     configurationFunctions.accessors[dimension.sortAccessor] = getFunction(
  //       this.functions,
  //       "dataset",
  //       "accessor",
  //       dimension.sortAccessor
  //     );
  //     configurationFunctions.sorts[dimension.sortFunction] = getFunction(
  //       this.functions,
  //       "dataset",
  //       "sort",
  //       dimension.sortFunction
  //     );
  //     configurationFunctions.formats[dimension.format] = getFunction(
  //       this.functions,
  //       "dataset",
  //       "format",
  //       dimension.format
  //     );
  //   });
  //   applyConfigurationToStore(
  //     this.store,
  //     configuration,
  //     configurationFunctions,
  //     null
  //   );
  // };
  // applyMeasures = () => {
  //   const { configuration, configurationFunctions } = this.props;
  //   const measures = this.buildObject(this.tabs[0].data, { index_: true });
  //   configuration.measures = measures;
  //   measures.forEach(measure => {
  //     const f = getFunction(
  //       this.functions,
  //       "dataset",
  //       "accessor",
  //       measure.valueAccessor
  //     );
  //     configurationFunctions.accessors[measure.valueAccessor] = f;
  //     if (!aggregations[measure.aggregation]) {
  //       configurationFunctions.accessors[measure.aggregation] = getFunction(
  //         this.functions,
  //         "dataset",
  //         "accessor",
  //         measure.aggregation
  //       );
  //     }
  //   });
  //   applyConfigurationToStore(
  //     this.store,
  //     configuration,
  //     configurationFunctions,
  //     null
  //   );
  // };
  // applyMeta = () => {
  //   setData(this.store, [...this.data], this.meta);
  // };
  render() {
    this.displayId = `pivotgrid-${this.props.id || 0}`;
    let div = (
      <div>
        <Provider store={this.store}>
          <PivotGrid
            id={this.displayId}
            menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
            key={this.displayId}
            gridId={this.displayId}
            // isActive={this.props.isActive}
            getRef={ref => (this.display = ref)}
          />
        </Provider>
      </div>
    );

    // if (this.props.display === "configuration") {
    //   this.displayId = `configuration-${this.props.id || 0}`;
    //   const { data, status } = this.store.getState();
    //   this.meta = data.meta || {};
    //   this.data = data.data;
    //   div = (
    //     <div>
    //       <Provider store={this.store}>
    //         <ZebulonTableAndConfiguration
    //           key={this.displayId}
    //           configurationId={this.displayId}
    //           sizes={this.state.sizes}
    //           data={this.data}
    //           meta={this.meta}
    //           functions={this.functions}
    //           params={this.props.params || {}}
    //           status={status}
    //           tabs={this.tabs}
    //           ref={ref => (this.display = ref)}
    //           // applyConfiguration={this.applyConfiguration}
    //           callbacks={{
    //             ...this.props.callbacks,
    //             applyDimensions: this.applyDimensions,
    //             applyMeasures: this.applyMeasures,
    //             applyMeta: this.applyMeta
    //           }}
    //         />
    //       </Provider>
    //     </div>
    //   );
    // } else
    //
    // if (this.props.display === "chart") {
    //   this.displayId = `chart-${this.props.id || 0}`;
    //   div = (
    //     <div>
    //       <Provider store={this.store}>
    //         <Chart
    //           id={this.displayId}
    //           menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
    //           key={this.displayId}
    //           configurationId={this.displayId}
    //           // isActive={this.props.isActive}
    //           getRef={ref => (this.display = ref)}
    //           callbacks={this.props.callbacks}
    //         />
    //       </Provider>
    //     </div>
    //   );
    // }
    return div;
  }
}
// expose all actions
Object.keys(actions).forEach(action => {
  /* eslint-disable func-names */
  ZebulonGrid.prototype[action] = function(...args) {
    this.store.dispatch(actions[action](...args));
  };
  /* eslint-enable */
});
ZebulonGrid.prototype["setData"] = function(data) {
  setData(this.store, data);
};
ZebulonGrid.prototype["getStore"] = function() {
  return this.store.getState();
};
ZebulonGrid.prototype["setConfiguration"] = function(configuration, data) {
  applyConfigurationToStore(
    this.store,
    configuration,
    this.props.configurationFunctions,
    data
  );
};
ZebulonGrid.prototype["setSizes"] = function(sizes) {
  applySizesToStore(this.store, { ...defaultSizes, ...sizes });
};
export default ZebulonGrid;
