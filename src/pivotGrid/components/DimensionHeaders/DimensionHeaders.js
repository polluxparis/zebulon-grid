import React, { Component } from "react";

import { AxisType, MEASURE_ID, TOTAL_ID } from "../../constants";
import DimensionHeader from "../DimensionHeader/DimensionHeader";
import { isNullOrUndefined } from "../../utils/generic";
class DimensionHeaders extends Component {
  componentDidMount() {
    this.setCollapsibles();
  }
  componentDidUpdate() {
    this.setCollapsibles();
  }
  setCollapsibles = () => {
    const getAxisCollapsibles = dimensions =>
      dimensions.reduceRight((acc, dim, index) => {
        let res;
        if (dim.isAttribute || dim.id === MEASURE_ID) {
          res = 0;
        } else {
          res = Object.values(acc).some(el => el >= 1) ? 2 : 1;
        }
        return { ...acc, [dim.id]: res };
      }, {});
    const columnCollapsibles = getAxisCollapsibles(this.props.columnDimensions);
    const rowCollapsibles = getAxisCollapsibles(this.props.rowDimensions);
    this.collapsibles = { ...columnCollapsibles, ...rowCollapsibles };
  };
  // -----------------------------------------------------
  collectMenu = props => {
    return {
      ...props,
      availableDimensions: this.props.availableDimensions,
      // direction: props.sortDirection === "asc" ? "descending" : "ascending",
      dimensionFilter: this.props.filters[props.dimension.id],
      isNotCollapsible: this.collapsibles[props.dimension.id] < 2,
      features: this.props.features
    };
  };
  // ---------------------------------------------------
  headersRenderer = (axis, dimensions, lastCrossDimensionId) => {
    const {
      previewSizes,
      gridId,
      toggleCollapseDimension,
      moveDimension,
      getExpandCollapseKeys,
      expandCollapseAll,
      toggleSortOrder,
      toggleSubTotal,
      crossPositions,
      height,
      width,
      features
    } = this.props;
    const headers = [];

    dimensions.forEach((dimension, index) => {
      if (
        dimension.isVisible &&
        (dimension.id !== MEASURE_ID || lastCrossDimensionId === TOTAL_ID)
      ) {
        let positions;
        if (axis === AxisType.ROWS) {
          positions = {
            left: crossPositions[AxisType.ROWS][dimension.id].position,
            top:
              height -
              crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size,
            width: crossPositions[AxisType.ROWS][dimension.id].size,
            height: crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size
          };
        } else {
          positions = {
            left:
              width - crossPositions[AxisType.ROWS][lastCrossDimensionId].size,
            top: crossPositions[AxisType.COLUMNS][dimension.id].position,
            width: crossPositions[AxisType.ROWS][lastCrossDimensionId].size,
            height: crossPositions[AxisType.COLUMNS][dimension.id].size
          };
        }
        headers.push(
          <DimensionHeader
            key={`dimension-header-${dimension.id}`}
            positions={positions}
            dimension={dimension}
            crossDimensionId={lastCrossDimensionId}
            previewSizes={previewSizes}
            gridId={gridId}
            toggleCollapseDimension={toggleCollapseDimension}
            toggleSortOrder={() => toggleSortOrder(axis, index)}
            moveDimension={moveDimension}
            toggleSubTotal={toggleSubTotal}
            getExpandCollapseKeys={getExpandCollapseKeys}
            expandCollapseAll={expandCollapseAll}
            collectMenu={this.collectMenu}
            isFiltered={!isNullOrUndefined(this.props.filters[dimension.id])}
            features={features}
          />
        );
      }
    });
    return headers;
  };

  render() {
    const { columnDimensions, rowDimensions, height, width } = this.props;
    let headers = [];
    let lastCrossDimensionId;
    let crossDimensions = rowDimensions.filter(
      dimension => dimension.isVisible
    );
    lastCrossDimensionId = crossDimensions[crossDimensions.length - 1].id;
    headers = this.headersRenderer(
      AxisType.COLUMNS,
      columnDimensions,
      lastCrossDimensionId
    );
    crossDimensions = columnDimensions.filter(dimension => dimension.isVisible);
    lastCrossDimensionId = crossDimensions[crossDimensions.length - 1].id;
    headers = headers.concat(
      this.headersRenderer(AxisType.ROWS, rowDimensions, lastCrossDimensionId)
    );

    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      <div
        style={{
          position: "relative",
          overflow: "hidden",
          height,
          width
        }}
        className="zebulon-grid-dimension-headers"
        onMouseDown={this.handleMouseDown}
      >
        {headers}
      </div>
    );
  }
}

export default DimensionHeaders;
