import React, { Component } from "react";
import { createStore } from "redux";
import { Provider } from "react-redux";
import PivotGrid from "./containers/PivotGrid";
import Chart from "./containers/Chart";
// configuration
import { ZebulonTableAndConfiguration } from "zebulon-table";
// configuration
import { utils, accessors } from "zebulon-controls";
import reducer from "./reducers";
import * as aggregations from "./utils/aggregation";
import { configurationMenus } from "./utils/configurationMenus";
import { EventHandler } from "zebulon-controls";
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
    this.buildDimensionsAndMesures(props.configuration, props.tabs);
    // configuration
    this.state = {
      sizes: { ...props.sizes, height: props.sizes.height - 30 },
      display: props.display
    };
    this.zoomValue = props.sizes.zoom || 1;
  }
  initTabDimensions = dimensions => {
    this.dimensions = dimensions.map((dimension, index) => {
      dimension.index_ = index;
      return dimension;
    });
    return {
      id: "dimensions",
      caption: "Dimensions",
      data: this.dimensions,
      updatedRows: {}
    };
  };
  initTabMeasures = measures => {
    this.measures = measures.map((measure, index) => {
      measure.index_ = index;
      return measure;
    });
    return {
      id: "measures",
      caption: "Measures",
      data: this.measures,
      updatedRows: {}
    };
  };
  buildDimensionsAndMesures = (configuration, tabs) => {
    this.tabs = [
      this.initTabMeasures(configuration.measures),
      this.initTabDimensions(configuration.dimensions)
    ];
    if (tabs) {
      this.tabs = this.tabs.concat(tabs);
    }
  };
  // configuration
  componentWillReceiveProps(nextProps) {
    const {
      data,
      meta,
      configuration,
      // configurationFunctions,
      pushedData,
      sizes,
      keyEvent,
      display,
      tabs,
      functions
    } = nextProps;
    // this.sizes = { ...defaultSizes, ...sizes };

    if (sizes !== this.props.sizes) {
      applySizesToStore(this.store, sizes);
      if (sizes.zoom) {
        this.zoomValue = sizes.zoom;
      }
      this.setState({
        sizes: { ...sizes, height: sizes.height - 30, zoom: this.zoomValue }
      });
    }
    // configuration
    if (this.props.tabs !== tabs) {
      this.buildDimensionsAndMesures(configuration, tabs);
    }
    if (this.props.configuration !== configuration) {
      applyConfigurationToStore(
        this.store,
        configuration,
        functions,
        data === this.props.data ? null : data
      );
    } else if (this.props.data !== data) {
      setData(this.store, data, meta);
    } else if (this.props.pushedData !== pushedData && pushedData.length) {
      pushData(this.store, pushedData);
    }
    if (this.props.display !== display) {
      this.setState({ display });
    }
    // else if (this.props.keyEvent !== keyEvent) {
    //   this.handleKeyEvent(keyEvent);
    // }
  }
  componentWillMount() {
    const {
      data,
      configuration,
      // configurationFunctions,
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
      this.props.functions,
      data
    );
  }
  componentDidMount() {
    if (this.props.getComponent) {
      this.props.getComponent(this);
    }
    // if (!this.props.keyEvent === undefined) {
    //   document.addEventListener("copy", this.handleCopy);
    //   document.addEventListener("paste", this.handlePaste);
    //   document.addEventListener("keydown", this.handleKeyDown);
    // }
  }
  // componentWillUnmount() {
  //   if (!this.props.keyEvent === undefined) {
  //     document.removeEventListener("copy", this.handleCopy);
  //     document.removeEventListener("paste", this.handlePaste);
  //     document.removeEventListener("keydown", this.handleKeyDown);
  //   }
  // }
  // shouldComponentUpdate() {
  //   // if (this.updateKey) {
  //   //   this.updateKey = false;
  //   //   return false;
  //   // }
  //   return true;
  // }
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
    else if (e.type === "copy") this.display.handleCopy(e);
    else if (e.type === "paste") this.display.handlepaste(e);
    else if (e.type === "keydown") this.display.handleKeyDown(e);
    // this.updateKey = true;
  };
  // handleKeyDown = e => {
  //   if (
  //     !e.defaultPrevented &&
  //     this.display.handleKeyDown &&
  //     (this.props.isActive === undefined || this.props.isActive)
  //   ) {
  //     this.display.handleKeyDown(e);
  //   }
  // };
  // handleCopy = e => {
  //   if (
  //     !e.defaultPrevented &&
  //     this.display.handleCopy &&
  //     (this.props.isActive === undefined || this.props.isActive)
  //   ) {
  //     this.display.handleCopy(e);
  //   }
  // };
  // handlePaste = e => {
  //   if (
  //     !e.defaultPrevented &&
  //     this.display.handlePaste &&
  //     (this.props.isActive === undefined || this.props.isActive)
  //   ) {
  //     this.display.handlePaste(e);
  //   }
  // };
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
  applyDimensions = ({ data, updatedRows }) => {
    const { configuration, functions } = this.props;
    const notSet = configuration.dimensions
      .map(d => d.id)
      .filter(
        d =>
          !(
            configuration.axis.rows.includes(d) ||
            configuration.axis.columns.includes(d)
          )
      );
    configuration.dimensions = data.filter(
      row => !(updatedRows[row.index_] || {}).deleted_
    );
    // let dimensions allready set as they where
    const dimensions = configuration.dimensions.map(d => d.id);
    configuration.axis.columns = configuration.axis.columns.filter(d =>
      dimensions.includes(d)
    );
    configuration.axis.rows = dimensions.filter(
      d => !(configuration.axis.columns.includes(d) || notSet.includes(d))
    );
    // apply config
    applyConfigurationToStore(this.store, configuration, functions);
    this.initTabDimensions(configuration.dimensions);
    return true;
  };
  applyMeasures = ({ data, updatedRows }) => {
    const { configuration, functions } = this.props;
    const notSet = configuration.measures
      .map(m => m.id)
      .filter(m => !configuration.axis.measures.includes(m));
    configuration.measures = data.filter(
      row => !(updatedRows[row.index_] || {}).deleted_
    );
    // let measures allready set as they where
    configuration.axis.measures = configuration.measures
      .map(m => m.id)
      .filter(m => !notSet.includes(m));
    //apply config
    applyConfigurationToStore(this.store, configuration, functions);
    this.initTabMeasures(configuration.measures);
    return true;
  };
  applyFunctions = ({ updatedRows }) => {
    const { configuration, functions } = this.props;
    this.display.applyFunctions({ updatedRows });
    applyConfigurationToStore(this.store, configuration, functions);
    return true;
  };
  onClick = () => {
    if (this.props.onActivation) {
      this.props.onActivation(
        this.props.componentId || `zebulon-grid-${this.props.id}`
      );
    }
  };
  render() {
    const componentId =
      this.props.componentId || `zebulon-grid-${this.props.id}`;
    // pivot grid
    let div = (
      <EventHandler component={this} id={componentId} componentId={componentId}>
        <Provider store={this.store}>
          <PivotGrid
            componentId={componentId}
            id={this.props.id}
            menuFunctions={this.props.menuFunctions || defaultMenuFunctions}
            selectedCell={this.props.selectedCell}
            getComponent={ref => (this.display = ref)}
          />
        </Provider>
      </EventHandler>
    );

    if (
      this.state.display === "configuration" ||
      this.state.display === "error"
    ) {
      const { data, status } = this.store.getState();
      this.meta = data.meta || this.props.meta || {};
      this.data = data.data;
      // configuration
      div = (
        <EventHandler
          component={this}
          id={componentId}
          componentId={componentId}
        >
          <Provider store={this.store}>
            <ZebulonTableAndConfiguration
              id={this.props.id}
              sizes={this.state.sizes}
              data={this.data}
              meta={this.meta}
              functions={this.props.functions}
              params={this.props.params || {}}
              status={status}
              tabs={this.tabs}
              ref={ref => (this.display = ref)}
              callbacks={{
                applyDimensions: this.applyDimensions,
                applyMeasures: this.applyMeasures,
                applyFunctions: this.applyFunctions,
                ...this.props.callbacks
              }}
              // utils={this.props.utils}
              configurationMenus={{
                ...configurationMenus,
                ...(this.props.configurationMenus || {})
              }}
            />
          </Provider>
        </EventHandler>
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
