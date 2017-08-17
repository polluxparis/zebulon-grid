import React, { Component } from 'react';

import { AxisType, MEASURE_ID, ROOT_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader/DimensionHeader';
import { connectMenu } from 'react-contextmenu';
import ContextMenu from '../ContextMenu/ContextMenu';
import { isNullOrUndefined } from '../../utils/generic';
class DimensionHeaders extends Component {
  // -----------------------------------------------------
  collectMenu = props => {
    return {
      ...props,
      availableDimensions: this.props.availableDimensions,
      direction: props.sortDirection === 'asc' ? 'descending' : 'ascending',
      dimensionFilter: this.props.filters[props.dimensionId],
      isNotCollapsible:
        props.isAttribute ||
        props.index ===
          (props.axis === AxisType.COLUMNS
            ? this.props.columnDimensions.length
            : this.props.rowDimensions.length) -
            1
    };
  };
  // ---------------------------------------------------
  headersRenderer = (axis, dimensions, lastCrossDimensionId) => {
    const {
      previewSizes,
      gridId,
      toggleCollapseDimension,
      moveDimension,
      getDimensionKeys,
      expandCollapseAll,
      toggleSortOrder,
      crossPositions,
      height,
      width
    } = this.props;
    const headers = [];
    dimensions.forEach((dimension, index) => {
      if (
        (dimension.id !== MEASURE_ID || lastCrossDimensionId === ROOT_ID) &&
        dimension.id !== ROOT_ID
      ) {
        const isNotCollapsible =
          !dimension.isCollapsed &&
          (dimension.isAttribute ||
            index >=
              dimensions.length -
                1 -
                (dimensions[dimensions.length - 1].id === MEASURE_ID) ||
            !dimensions[index + 1].isAttribute);
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
            left={positions.left}
            top={positions.top}
            width={positions.width}
            height={positions.height}
            dimensionId={dimension.id}
            dimensionIndex={index}
            sortDirection={dimension.sort.direction}
            caption={dimension.caption}
            axis={axis}
            crossDimensionId={lastCrossDimensionId}
            isNotCollapsible={isNotCollapsible}
            isCollapsed={dimension.isCollapsed || false}
            previewSizes={previewSizes}
            gridId={gridId}
            toggleCollapseDimension={toggleCollapseDimension}
            toggleSortOrder={toggleSortOrder}
            moveDimension={moveDimension}
            getDimensionKeys={getDimensionKeys}
            expandCollapseAll={expandCollapseAll}
            isDropTarget={true}
            isAttribute={dimension.isAttribute}
            collectMenu={this.collectMenu}
            isFiltered={!isNullOrUndefined(this.props.filters[dimension.id])}
          />
        );
      }
    });
    return headers;
  };

  render() {
    const {
      columnDimensions,
      rowDimensions,
      height,
      width,
      zoom,
      gridId
    } = this.props;
    const ConnectedMenu = connectMenu(
      `context-menu-dimension-header-${gridId}`
    )(ContextMenu);
    let headers = [];
    let lastCrossDimensionId;
    if (rowDimensions.length === 0) {
      lastCrossDimensionId = ROOT_ID;
    } else {
      lastCrossDimensionId = rowDimensions[rowDimensions.length - 1].id;
    }

    headers = this.headersRenderer(
      AxisType.COLUMNS,
      columnDimensions.length ? columnDimensions : [{ id: ROOT_ID }],
      lastCrossDimensionId
    );

    if (columnDimensions.length === 0) {
      lastCrossDimensionId = ROOT_ID;
    } else {
      lastCrossDimensionId = columnDimensions[columnDimensions.length - 1].id;
    }
    headers = headers.concat(
      this.headersRenderer(
        AxisType.ROWS,
        rowDimensions.length ? rowDimensions : [{ id: ROOT_ID }],
        lastCrossDimensionId
      )
    );

    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      <div
        style={{
          position: 'relative',
          height,
          width,
          fontSize: `${zoom * 100}%`,
          overflow: 'hidden'
        }}
        className="pivotgrid-dimension-headers"
        onMouseDown={this.handleMouseDown}
      >
        {headers}
        <ConnectedMenu />
      </div>
    );
  }
}

export default DimensionHeaders;
