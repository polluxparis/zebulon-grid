import React from "react";
import { AxisType, toAxis, TOTAL_ID, MEASURE_ID } from "../../constants";
import classnames from "classnames";
import { isNullOrUndefined } from "../../utils/generic";
import Filter from "../../containers/Filter";
import Dataset from "../../containers/Dataset";
import Properties from "../../containers/Properties";
import Dimensions from "../../containers/Dimensions";
import Measures from "../../containers/Measures";

const menuFunctions = (type, fct, data, menus) => {
  const keys = Object.keys(fct);
  if (keys.length) {
    menus.push({
      id: menus.length,
      type: "sub-menu",
      caption: type,
      children: keys.map((f, index) => ({
        id: 100 * (menus.length - 1) + index,
        type: "menu-item",
        caption: fct[f].caption,
        onClick: () =>
          data.onItemClick(type, fct[f].code, {
            rowIndex: data.rowIndex,
            columnIndex: data.columnIndex,
            value: null
          })
      }))
    });
  }
};
const getGridMenu = data => {
  const menus = [];
  const features = data.configuration.features;
  let fct = data.menuFunctions.dataCellFunctions || {},
    keys,
    cellFunctions,
    rangeFunctions,
    gridFunctions;
  menuFunctions(
    "Cell",
    data.menuFunctions.dataCellFunctions || {},
    data,
    menus
  );
  menuFunctions("Range", data.menuFunctions.rangeFunctions || {}, data, menus);
  menuFunctions("Grid", data.menuFunctions.gridFunctions || {}, data, menus);
  if (features.filters === "enabled") {
    menus.push({
      id: menus.length,
      type: "sub-menu",
      separation: menus.length > 0,
      caption: "Filters",
      children: data.dimensions
        .filter(dimension => dimension.id !== MEASURE_ID)
        .map((dimension, index) => ({
          id: 100 * (menus.length - 1) + index,
          type: "sub-menu",
          caption: dimension.caption,
          checked: data.filters[dimension.id] !== undefined,
          children: [
            {
              id: 1000 * (menus.length - 1) + index,
              type: "jsx",
              navigation: true,
              content: (
                <div
                  key={dimension.id}
                  style={{
                    maxHeight: 600,
                    backgroundColor: "white",
                    border: " solid 0.05em rgba(0, 0, 0, 0.5)"
                  }}
                >
                  <Filter dimensionId={dimension.id} />
                </div>
              )
            }
          ]
        }))
    });
  }
  if (features.configuration === "enabled") {
    const children = [
      {
        id: 100,
        type: "sub-menu",
        caption: "Default cell height",
        children: [
          {
            id: 1000,
            type: "jsx",
            content: (
              <div
                style={{
                  textAlign: "right",
                  backgroundColor: "lightgrey",
                  minWidth: 50,
                  padding: 2
                }}
              >
                {data.configuration.cellHeight}
              </div>
            )
          }
        ]
      },
      {
        id: 101,
        type: "sub-menu",
        caption: "Default cell width",
        children: [
          {
            id: 1001,
            type: "jsx",
            content: (
              <div
                style={{
                  textAlign: "right",
                  backgroundColor: "lightgrey",
                  minWidth: 50,
                  padding: 2
                }}
              >
                {data.configuration.cellWidth}
              </div>
            )
          }
        ]
      },
      {
        id: 100 * menus.length + 2,
        type: "menu-item",
        separation: true,
        caption: "Totals before",
        checked: data.configuration.totalsFirst,
        onClick: () =>
          data.onItemClick("Totals", null, {
            value: !data.configuration.totalsFirst
          })
      }
    ];
    if (data.configuration.edition.editable) {
      children.push({
        id: 100 * menus.length + children.length,
        type: "menu-item",
        separation: true,
        caption: "Edition mode",
        checked: data.configuration.edition.activated,
        onClick: () =>
          data.onItemClick("Edition", null, {
            value: !data.configuration.edition.activated
          })
      });
      children.push({
        id: 100 * menus.length + children.length,
        type: "menu-item",
        caption: "Comments tool tips",
        checked: data.configuration.edition.comments,
        onClick: () =>
          data.onItemClick("Comments", null, {
            value: !data.configuration.edition.comments
          })
      });
    }
    children.push({
      id: 100 * menus.length + children.length,
      type: "sub-menu",
      separation: true,
      caption: "Dataset",
      children: [
        {
          id: 10 * (100 * menus.length + children.length),
          type: "jsx",
          content: (
            <div
              key="dataset"
              style={{
                maxHeight: 1000,
                backgroundColor: "white",
                border: " solid 0.05em rgba(0, 0, 0, 0.5)"
              }}
            >
              <Dataset />
            </div>
          )
        }
      ]
    });
    children.push({
      id: 100 * menus.length + children.length,
      type: "sub-menu",
      caption: "Properties",
      children: [
        {
          id: 10 * (100 * menus.length + children.length),
          type: "jsx",
          content: (
            <div
              key="properties"
              style={{
                maxHeight: 1000,
                backgroundColor: "white",
                border: " solid 0.05em rgba(0, 0, 0, 0.5)"
              }}
            >
              <Properties />
            </div>
          )
        }
      ]
    });
    children.push({
      id: 100 * menus.length + children.length,
      type: "sub-menu",
      caption: "Measures",
      children: [
        {
          id: 10 * (100 * menus.length + children.length),
          type: "jsx",
          content: (
            <div
              key="measures"
              style={{
                maxHeight: 1000,
                backgroundColor: "white",
                border: " solid 0.05em rgba(0, 0, 0, 0.5)"
              }}
            >
              <Measures />
            </div>
          )
        }
      ]
    });
    children.push({
      id: 100 * menus.length + children.length,
      type: "sub-menu",
      caption: "Dimensions",
      children: [
        {
          id: 10 * (100 * menus.length + children.length),
          type: "jsx",
          content: (
            <div
              key="dimensions"
              style={{
                maxHeight: 1000,
                backgroundColor: "white",
                border: " solid 0.05em rgba(0, 0, 0, 0.5)"
              }}
            >
              <Dimensions />
            </div>
          )
        }
      ]
    });
    menus.push({
      id: menus.length,
      type: "sub-menu",
      separation: menus.length > 0,
      caption: "Configuration",
      children
    });
    console.log(menus);
  }
  if (menus.length) {
    return {
      type: "menu",
      position: "bottom",
      children: menus
    };
  } else {
    return null;
  }
};

const getMeasureMenu = data => {
  if (data.features.measures === "enabled") {
    return {
      type: "menu",
      position: "bottom",
      children: [
        {
          id: 1,
          type: "menu-item",
          caption: "Measures in rows",
          checked: data.axis === AxisType.ROWS,
          onClick: () =>
            data.toggleMeasuresAxis(
              data.axis === AxisType.ROWS ? "columns" : "rows"
            )
        },
        {
          id: 2,
          type: "menu-item",
          caption: `Remove ${data.header.caption}`,
          onClick: () => data.toggleMeasure(data.header.id),
          disable: data.measures.length === 1
        },
        {
          id: 3,
          type: "sub-menu",
          caption: "Add measure",
          disable: data.availableMeasures.length === 0,
          children: data.availableMeasures.map((measure, index) => ({
            id: 4 + index,
            type: "menu-item",
            caption: measure.caption,
            onClick: () => data.toggleMeasure(measure.id)
          }))
        }
      ]
    };
  } else {
    return null;
  }
};
const getDimensionMenu = data => {
  const {
    features,
    isNotCollapsible,
    dimension,
    toggleSubTotal,
    toggleSortOrder,
    collapseAll,
    moveDimension
  } = data;
  const { hasAttribute, hasSubTotal, hasGrandTotal } = dimension;
  const availableDimensions = data.availableDimensions.filter(
    dimension => dimension.id !== MEASURE_ID
  );
  const menus = [];
  if (features.sorting === "enabled") {
    menus.push({
      id: menus.length,
      type: "menu-item",
      caption: `Sort  ${dimension.sort.direction === "asc"
        ? "descending"
        : "ascending"}`,
      onClick: () => toggleSortOrder(dimension.id)
    });
  }
  if (!isNotCollapsible && features.expandCollapse === "enabled") {
    menus.push(
      {
        id: menus.length,
        type: "menu-item",
        caption: "Expand all",
        separation: menus.length > 0,
        disabled: isNotCollapsible,
        onClick: () => collapseAll(false)
      },
      {
        id: menus.length + 1,
        type: "menu-item",
        caption: "Collapse all",
        disabled: isNotCollapsible,
        onClick: () => collapseAll(true)
      }
    );
  }
  if (features.totals === "enabled") {
    if (hasSubTotal !== null) {
      menus.push({
        id: menus.length,
        type: "menu-item",
        separation: menus.length > 0,
        caption: "Sub total",
        checked: hasSubTotal,
        onClick: () => toggleSubTotal(dimension.id)
      });
    }
    if (hasGrandTotal !== null) {
      menus.push({
        id: menus.length,
        type: "menu-item",
        caption: "Grand total",
        checked: hasGrandTotal,
        onClick: () => toggleSubTotal(`${toAxis(dimension.axis)}${TOTAL_ID}`)
      });
    }
  }
  if (features.filters === "enabled" && dimension.id !== TOTAL_ID) {
    menus.push({
      id: menus.length,
      type: "sub-menu",
      caption: "Filter",
      separation: menus.length > 0,
      checked: data.dimensionFilter !== undefined,
      children: [
        {
          id: 100,
          type: "jsx",
          navigation: true,
          // caption: "Filter",
          content: (
            <div
              key="filters"
              style={{
                maxHeight: 600,
                backgroundColor: "white",
                border: " solid 0.05em rgba(0, 0, 0, 0.5)"
              }}
            >
              <Filter dimensionId={dimension.id} />
            </div>
          )
        }
      ]
    });
  }
  if (features.dimensions === "enabled") {
    if (dimension.id !== TOTAL_ID) {
      menus.push({
        id: menus.length,
        type: "menu-item",
        separation: menus.length > 0,
        caption: `Remove ${dimension.caption}`,
        onClick: () =>
          moveDimension(
            dimension.id,
            toAxis(dimension.axis),
            toAxis(AxisType.DIMENSION)
          )
      });
      menus.push({
        id: menus.length,
        type: "sub-menu",
        caption: "Add dimension",
        disable: availableDimensions.length === 0,
        children: availableDimensions.map((dim, index) => ({
          id: 100 * (menus.length - 1) + index,
          type: "menu-item",
          caption: dim.caption,
          onClick: () =>
            moveDimension(
              dim.id,
              toAxis(AxisType.DIMENSION),
              toAxis(dimension.axis),
              dimension.id === MEASURE_ID
                ? dimension.depth
                : dimension.depth + 1
            )
        }))
      });
    }
  }
  if (menus.length) {
    return {
      type: "menu",
      position: "bottom",
      children: menus
    };
  } else {
    return null;
  }
};
export const getMenu = (id, data) => {
  if (id === "dimension-menu") {
    return getDimensionMenu(data);
  } else if (id === "measure-menu") {
    return getMeasureMenu(data);
  } else if (id === "grid-menu") {
    return getGridMenu(data);
  }
};

// export default ContextMenu;
