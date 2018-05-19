import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";
import PivotGrid from "./containers/PivotGrid";
import Chart from "./containers/Chart";
// configuration
import { ZebulonTableAndConfiguration, functions } from "zebulon-table";
// configuration
import { utils, accessors } from "zebulon-controls";
import reducer from "./reducers";
import * as aggregations from "./utils/aggregation";
import { configurationMenu } from "./utils/configurationMenu";
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
    // configuration
    this.buildFunctionsTable(props);
    if (props.display === "configuration")
      this.buildDimensionsAndMesures(props.configuration);
    // configuration
    this.state = { sizes: props.sizes, display: props.display };
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
        data: this.measures,
        updatedRows: {}
      },
      {
        id: "dimensions",
        caption: "Dimensions",
        data: this.dimensions,
        updatedRows: {}
      }
    ];
    if (this.props.tabs) {
      this.tabs = this.tabs.concat(this.props.tabs);
    }
  };
  // configuration
  buildFunctionsTable = props => {
    if (!Array.isArray(props.functions)) {
      this.functions = utils.mergeFunctions(
        [
          accessors,
          functions,
          props.configurationFunctions || {},
          props.configuration.functions || {}
        ],
        "dataset"
      );
    } else {
      this.functions = props.functions;
    }
  };
  // configuration
  componentWillReceiveProps(nextProps) {
    const {
      data,
      meta,
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
    if (
      display === "configuration" &&
      (this.props.display !== display ||
        this.props.configuration !== configuration)
    )
      this.buildDimensionsAndMesures(configuration);
    // configuration
    if (
      nextProps.configurationFunctions !== this.props.configurationFunctions ||
      nextProps.functions !== this.props.functions ||
      !this.functions
    ) {
      this.buildFunctionsTable(nextProps);
    }
    // configuration
    if (this.props.configuration !== configuration) {
      applyConfigurationToStore(
        this.store,
        configuration,
        this.functions,
        data === this.props.data ? null : data,
        this.props.utils || utils
      );
    } else if (this.props.data !== data) {
      setData(this.store, data, meta);
    } else if (this.props.pushedData !== pushedData && pushedData.length) {
      pushData(this.store, pushedData);
    }
    if (this.props.display !== display) {
      this.setState({ display });
    } else if (this.props.keyEvent !== keyEvent) {
      this.handleKeyEvent(keyEvent);
    }
  }
  componentWillMount() {
    const {
      data,
      configuration,
      configurationFunctions,
      sizes,
      functions
    } = this.props;
    this.store = createStore(
      reducer,
      window.__REDUX_DEVTOOLS_EXTENSION__ &&
        window.__REDUX_DEVTOOLS_EXTENSION__()
    );
    applySizesToStore(this.store, { ...defaultSizes, ...sizes });
    applyConfigurationToStore(
      this.store,
      configuration,
      this.functions,
      data,
      this.props.utils || utils
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
    if (this.display && this.display.handleKeyEvent) {
      return this.display.handleKeyEvent(e);
    }
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
  // configuration
  applyDimensions = () => {
    const { configuration, configurationFunctions } = this.props;
    const dimensions = this.buildObject(
      this.tabs[1].data.filter(
        d => !(this.tabs[1].updatedRows[d.index_] || {}).deleted_
      ),
      { index_: true }
    );
    let add = false;
    const axis = {};
    (configuration.rows || []).forEach(d => (axis[d] = "rows"));
    (configuration.columns || []).forEach(d => (axis[d] = "columns"));
    (configuration.dimensions || []).forEach(d => (axis[d] = "rows"));
    configuration.rows = [];
    configuration.columns = [];
    // configuration.dimensions = [];
    // if (
    //   (configuration.rows || []).length +
    //     (configuration.columns || []).length ===
    //   0
    // ) {
    //   configuration.rows = [];
    //   add = true;
    // }
    if (!configurationFunctions.dataset) {
      configurationFunctions.dataset = {
        accessors: {},
        sorts: {},
        formats: {}
      };
    } else {
      if (!configurationFunctions.dataset.accessors) {
        configurationFunctions.dataset.accessors = {};
      }
      if (!configurationFunctions.dataset.sorts) {
        configurationFunctions.dataset.sorts = {};
      }
      if (!configurationFunctions.dataset.formats) {
        configurationFunctions.dataset.formats = {};
      }
    }
    dimensions.forEach(dimension => {
      configurationFunctions.dataset.accessors[
        dimension.keyAccessor
      ] = utils.getFunction(
        this.functions,
        "accessor",
        dimension.keyAccessor,
        this.props.utils
      );
      configurationFunctions.dataset.accessors[
        dimension.labelAccessor
      ] = utils.getFunction(
        this.functions,
        "accessor",
        dimension.labelAccessor,
        this.props.utils
      );
      configurationFunctions.dataset.accessors[
        dimension.sortAccessor
      ] = utils.getFunction(
        this.functions,
        "accessor",
        dimension.sortAccessor,
        this.props.utils
      );
      configurationFunctions.dataset.sorts[
        dimension.sortFunction
      ] = utils.getFunction(
        this.functions,
        "sort",
        dimension.sortFunction,
        this.props.utils
      );
      configurationFunctions.dataset.formats[
        dimension.format
      ] = utils.getFunction(
        this.functions,
        "format",
        dimension.format,
        this.props.utils
      );
      if (axis[dimension.id] === "dimensions") {
        configuration.dimensions.push(dimension.id);
      } else if (axis[dimension.id] === "columns") {
        configuration.columns.push(dimension.id);
      } else {
        configuration.rows.push(dimension.id);
      }
    });
    configuration.dimensions = dimensions;
    configuration.axis = { measures: configuration.axis.measures };
    applyConfigurationToStore(
      this.store,
      configuration,
      this.functions,
      this.props.data,
      this.props.utils || utils
    );
    return true;
  };
  applyMeasures = () => {
    const { configuration, configurationFunctions } = this.props;
    const measures = this.buildObject(
      this.tabs[0].data.filter(
        m => !(this.tabs[0].updatedRows[m.index_] || {}).deleted_
      ),
      { index_: true }
    );
    const axis = {};
    (configuration.measures || []).forEach(m => (axis[m.id] = "out"));
    (configuration.activeMeasures || []).forEach(m => (axis[m] = "in"));
    configuration.activeMeasures = [];
    if (!configurationFunctions.dataset) {
      configurationFunctions.dataset = {
        accessors: {},
        aggregations: {},
        formats: {}
      };
    } else {
      if (!configurationFunctions.dataset.accessors) {
        configurationFunctions.dataset.accessors = {};
      }
      if (!configurationFunctions.dataset.aggregations) {
        configurationFunctions.dataset.aggregations = {};
      }
      if (!configurationFunctions.dataset.formats) {
        configurationFunctions.dataset.formats = {};
      }
    }
    measures.forEach(measure => {
      const f = utils.getFunction(
        this.functions,
        "accessor",
        measure.valueAccessor,
        this.props.utils
      );
      configurationFunctions.dataset.accessors[measure.valueAccessor] = f;
      configurationFunctions.dataset.formats[
        measure.format
      ] = utils.getFunction(
        this.functions,
        "format",
        measure.format,
        this.props.utils
      );
      if (!aggregations[measure.aggregation]) {
        configurationFunctions.dataset.aggregation[
          measure.aggregation
        ] = utils.getFunction(
          this.functions,
          "aggregation",
          measure.aggregation,
          this.props.utils
        );
      }
      if (axis[measure.id] !== "out") {
        configuration.activeMeasures.push(measure.id);
      }
    });
    configuration.measures = measures;
    delete configuration.axis.measures;
    applyConfigurationToStore(
      this.store,
      configuration,
      this.functions,
      this.props.data,
      this.props.utils || utils
    );
    return true;
  };
  applyFunctions = () => {
    const { configuration, configurationFunctions } = this.props;
    const functions =
      // this.buildObject(
      this.display.state.functions.filter(f => f.isLocal).reduce((acc, f) => {
        if (!acc.dataset[f.tp]) {
          acc.dataset[f.tp] = {};
        }
        acc.dataset[f.tp][f.id] = f.functionText;
        return acc;
      }, { dataset: {} });
    // {
    //   index_: true,
    //   functionJS: true,
    //   isLocal: true
    // }
    // );
    configuration.functions = functions;
    return true;
  };

  // configuration
  render() {
    // this.displayId = `pivotgrid-${this.props.id || 0}`;
    let div = (
      <div>
        <Provider store={this.store}>
          <PivotGrid
            id={this.props.id}
            menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
            // key={this.displayId}
            // gridId={this.displayId}
            // isActive={this.props.isActive}
            getRef={ref => (this.display = ref)}
            // serverLoading={this.props.serverLoading}
          />
        </Provider>
      </div>
    );

    if (this.state.display === "configuration") {
      // this.displayId = `configuration-${this.props.id || 0}`;
      const { data, status } = this.store.getState();
      this.meta = data.meta || this.props.meta || {};
      this.data = data.data;
      const sizes = { ...this.state.sizes };
      sizes.height = sizes.height - 30;
      // configuration
      div = (
        <div>
          <Provider store={this.store}>
            <ZebulonTableAndConfiguration
              id={this.props.id}
              sizes={sizes}
              data={this.data}
              meta={this.meta}
              functions={this.functions}
              params={this.props.params || {}}
              status={status}
              // serverLoading={this.props.serverLoading}
              tabs={this.tabs}
              ref={ref => (this.display = ref)}
              callbacks={{
                applyDimensions: this.applyDimensions,
                applyMeasures: this.applyMeasures,
                applyFunctions: this.applyFunctions,
                ...this.props.callbacks
              }}
              utils={this.props.utils}
              contextualMenu={configurationMenu}
            />
          </Provider>
        </div>
      );
    }
    // configuration
    // else
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
    this.Functions,
    data,
    this.props.utils || utils
  );
};
ZebulonGrid.prototype["setSizes"] = function(sizes) {
  applySizesToStore(this.store, { ...defaultSizes, ...sizes });
};
export default ZebulonGrid;
