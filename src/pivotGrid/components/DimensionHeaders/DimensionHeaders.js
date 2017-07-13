import React, { Component } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID, ROOT_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader';
import {
  ContextMenu,
  MenuItem,
  ContextMenuTrigger,
  connectMenu,
  SubMenu
} from 'react-contextmenu';
import { isNullOrUndefined } from '../../utils/generic';
class DimensionHeaders extends Component {
  constructor() {
    super();
    this.headersRenderer = this.headersRenderer.bind(this);
  }
  shouldComponentUpdate(nextProps) {
    return nextProps.crossPositions !== this.props.crossPositions;
  }
  // -----------------------------------------------------
  handleMouseDown = e => {
    this.isRightClick = e.button === 2;
    console.log(['handleMouseDown', e.button]);
    return e;
  };
  DynamicMenu = props => {
    // if (!props.isRightClick) return <ContextMenu id={''} disabled={true} />;
    // else {
    const { id, trigger } = props;
    const handleItemClick = trigger ? trigger.onItemClick : null;
    if (isNullOrUndefined(trigger)) {
      return (
        <ContextMenu id={id} disabled={true}>
          action 1
        </ContextMenu>
      );
    }

    if (trigger.type === 'dimension-header') {
      const isDisable = trigger.availableDimensions.length === 0;
      return (
        <ContextMenu id={id}>
          <MenuItem onClick={trigger.onItemClick} data={{ action: 'remove' }}>
            {`Remove dimension ${trigger.caption}`}
          </MenuItem>
          <SubMenu title="Add dimension" disabled={isDisable}>
            {trigger.availableDimensions.map(dimension =>
              <MenuItem
                onClick={trigger.onItemClick}
                data={{ action: 'add', newDimensionId: dimension.id }}
              >
                {dimension.caption}
              </MenuItem>
            )}

          </SubMenu>
        </ContextMenu>
      );
    }
    // }
  };
  // ConnectedMenu = connectMenu(`context-menu- ${this.props.gridId}`)(
  //   this.DynamicMenu(this.props.availableDimensions, this.isRightClick || false)
  // );
  collectMenu = props => {
    console.log(this);
    return {
      ...props,
      availableDimensions: this.props.availableDimensions,
      isRightClick: this.isRightClick
    };
  };
  // ---------------------------------------------------
  headersRenderer = (axis, dimensions, lastCrossDimensionId) => {
    const {
      previewSizes,
      gridId,
      toggleCollapseDimension,
      moveDimension,
      toggleSortOrder,
      crossPositions,
      height,
      width
    } = this.props;
    const headers = [];
    dimensions.forEach((dimension, index) => {
      if (
        (dimension.id !== MEASURE_ID || lastCrossDimensionId == ROOT_ID) &&
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
            caption={dimension.caption}
            axis={axis}
            crossDimensionId={lastCrossDimensionId}
            isNotCollapsible={isNotCollapsible}
            isCollapsed={dimension.isCollapsed}
            previewSizes={previewSizes}
            gridId={gridId}
            toggleCollapseDimension={toggleCollapseDimension}
            toggleSortOrder={toggleSortOrder}
            moveDimension={moveDimension}
            isAttribute={dimension.isAttribute}
            collectMenu={this.collectMenu}
            isRightClick={this.isRightClick}
          />
        );
      }
    });
    return headers;
  };

  render() {
    const {
      columnDimensions,
      crossPositions,
      getColumnWidth,
      getRowHeight,
      rowDimensions,
      previewSizes,
      height,
      width,
      zoom,
      gridId,
      toggleCollapseDimension,
      availableDimensions
    } = this.props;
    const ConnectedMenu = connectMenu(`context-menu-${gridId}`)(
      this.DynamicMenu
    );
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
