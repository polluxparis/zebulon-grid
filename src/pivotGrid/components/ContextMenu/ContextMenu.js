import React from "react";
import {
  ContextMenu as ReactContextMenu,
  MenuItem,
  SubMenu
} from "react-contextmenu";
import classnames from "classnames";

import { isNullOrUndefined } from "../../utils/generic";
import { MEASURE_ID, TOTAL_ID } from "../../constants";
import Filter from "../../containers/Filter";

const DimensionMenu = (id, trigger) => {
  const { isNotCollapsible, hasSubTotal, hasGrandTotal } = trigger;
  const availableDimensions = trigger.availableDimensions.filter(
    dimension => dimension.id !== MEASURE_ID
  );
  const addDimensionSubMenu = (
    <SubMenu title="Add dimension" disabled={availableDimensions.length === 0}>
      {availableDimensions.map(dimension => (
        <MenuItem
          key={dimension.id}
          onClick={trigger.onItemClick}
          data={{ action: "add", newDimensionId: dimension.id }}
        >
          {dimension.caption}
        </MenuItem>
      ))}
    </SubMenu>
  );
  if (trigger.dimensionId === MEASURE_ID) {
    return <ReactContextMenu id={id}>{addDimensionSubMenu}</ReactContextMenu>;
  }
  return (
    <ReactContextMenu id={id}>
      <MenuItem
        onClick={trigger.onItemClick}
        disabled={trigger.dimensionId === TOTAL_ID}
        data={{ action: "sort" }}
      >
        {`Sort  ${trigger.direction} `}
      </MenuItem>
      {isNotCollapsible ? null : (
        <div>
          <MenuItem
            onClick={trigger.onItemClick}
            data={{ action: "expand all" }}
            disabled={isNotCollapsible}
          >
            Expand all
          </MenuItem>
          <MenuItem
            onClick={trigger.onItemClick}
            data={{ action: "collapse all" }}
            disabled={isNotCollapsible}
          >
            Collapse all
          </MenuItem>
        </div>
      )}
      {hasSubTotal === null ? null : (
        <div>
          <MenuItem
            onClick={trigger.onItemClick}
            data={{ action: "toggle subtotal" }}
          >
            {(hasSubTotal ? "Remove" : "Add") + " subtotal"}
          </MenuItem>
        </div>
      )}
      {hasGrandTotal === null ? null : (
        <div>
          <MenuItem
            onClick={trigger.onItemClick}
            data={{ action: "toggle grandtotal" }}
          >
            {(hasGrandTotal ? "Remove" : "Add") + " grand total"}
          </MenuItem>
        </div>
      )}
      <SubMenu
        title="Filter"
        disabled={trigger.dimensionId === TOTAL_ID}
        style={
          !isNullOrUndefined(trigger.dimensionFilter) ? (
            { fontWeight: "bold" }
          ) : null
        }
      >
        <div style={{ maxHeight: 600 }}>
          <Filter dimensionId={trigger.dimensionId} />
        </div>
      </SubMenu>
      <MenuItem
        onClick={trigger.onItemClick}
        disabled={trigger.dimensionId === TOTAL_ID}
        data={{ action: "remove" }}
      >
        Remove
      </MenuItem>
      {addDimensionSubMenu}
    </ReactContextMenu>
  );
};

const MeasureMenu = (id, trigger) => {
  const isDisabled = trigger.availableMeasures.length === 0;
  return (
    <ReactContextMenu id={id}>
      <MenuItem onClick={trigger.onItemClick} data={{ action: "move" }}>
        Move measures
      </MenuItem>
      <MenuItem
        onClick={trigger.onItemClick}
        data={{ action: "remove" }}
        disabled={Object.keys(trigger.measures || {}).length < 2}
      >
        Remove
      </MenuItem>
      <SubMenu title="Add" disabled={isDisabled}>
        {trigger.availableMeasures.map(measure => (
          <MenuItem
            onClick={trigger.onItemClick}
            data={{ action: "add", newMeasureId: measure.id }}
          >
            {measure.caption}
          </MenuItem>
        ))}
      </SubMenu>
    </ReactContextMenu>
  );
};
const externalMenu = (functionType, externalFunction, onClick) => {
  if (externalFunction.type === "SubMenu") {
    return (
      <SubMenu
        key={externalFunction.code}
        title={externalFunction.caption}
        onClick={e => console.log("SubMenu", e)}
      >
        {0}
      </SubMenu>
    );
  } else if (externalFunction.type === "MenuItem") {
    return (
      <MenuItem
        key={externalFunction.code}
        onClick={onClick}
        data={{ action: externalFunction.code, functionType }}
      >
        {externalFunction.caption}
      </MenuItem>
    );
  }
};

// {externalFunction.function()}
const DataCellMenu = (id, trigger) => {
  let fct = trigger.menuFunctions.dataCellFunctions,
    keys,
    cellFunctions,
    rangeFunctions,
    gridFunctions;
  keys = Object.keys(fct);
  if (keys.length) {
    cellFunctions = (
      <SubMenu title="Cell">
        {keys.map(externalFunction =>
          externalMenu("cell", fct[externalFunction], trigger.onItemClick)
        )}
      </SubMenu>
    );
  }
  fct = trigger.menuFunctions.rangeFunctions;
  keys = Object.keys(fct);
  if (keys.length) {
    rangeFunctions = (
      <SubMenu title="Range">
        {keys.map(externalFunction =>
          externalMenu("range", fct[externalFunction], trigger.onItemClick)
        )}
      </SubMenu>
    );
  }
  fct = trigger.menuFunctions.gridFunctions;
  keys = Object.keys(fct);
  if (keys.length) {
    gridFunctions = (
      <SubMenu title="Grid">
        {keys.map(externalFunction =>
          externalMenu("function", fct[externalFunction], trigger.onItemClick)
        )}
      </SubMenu>
    );
  }
  const config = { ...trigger.configuration };
  return (
    <ReactContextMenu id={id}>
      {cellFunctions}
      {rangeFunctions}
      {gridFunctions}
      <SubMenu title="Filters">
        {trigger.dimensions
          .filter(dimension => dimension.id !== MEASURE_ID)
          .map(dimension => (
            <SubMenu
              key={dimension.id}
              title={
                <span
                  className={classnames({
                    "react-contextmenu-item-filtered": !isNullOrUndefined(
                      trigger.filters[dimension.id]
                    )
                  })}
                >
                  {dimension.caption}
                </span>
              }
            >
              <Filter dimensionId={dimension.id} />
            </SubMenu>
          ))}
      </SubMenu>
      <SubMenu
        key={"configuration"}
        title="Configuration"
        style={{ width: "fitContent" }}
      >
        <SubMenu key={"cell-height"} title={"Default cell height"}>
          <div style={{ textAlign: "right", backgroundColor: "lightgrey" }}>
            {trigger.configuration.cellHeight}
          </div>
        </SubMenu>
        <SubMenu key={"cell-width"} title={"Default cell width"}>
          <div style={{ textAlign: "right", backgroundColor: "lightgrey" }}>
            {trigger.configuration.cellWidth}
          </div>
        </SubMenu>
        <MenuItem
          key={"toggle-subtotals"}
          onClick={trigger.onItemClick}
          data={{
            action: "toggle-totals",
            value: !trigger.configuration.totalsFirst
          }}
        >
          {"Set totals " +
            (trigger.configuration.totalsFirst ? "after" : "before")}
        </MenuItem>
      </SubMenu>
    </ReactContextMenu>
  );
};

const ContextMenu = props => {
  const { id, trigger } = props;
  if (isNullOrUndefined(trigger)) {
    return <ReactContextMenu id={id}>''</ReactContextMenu>;
  }

  if (trigger.type === "dimension-header") {
    return DimensionMenu(id, trigger);
  } else if (trigger.type === `header-${trigger.axis}`) {
    return MeasureMenu(id, trigger);
  } else if (trigger.type === "data-cell") {
    return DataCellMenu(id, trigger);
  }
};

export default ContextMenu;
