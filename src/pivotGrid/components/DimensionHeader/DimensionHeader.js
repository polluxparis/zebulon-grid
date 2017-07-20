import React, { Component } from 'react';
import classNames from 'classnames';

import InnerHeader from '../InnerHeader/InnerHeader';
import ResizeHandle from '../ResizeHandle/ResizeHandle';

import { MEASURE_ID, ROOT_ID, AxisType, toAxis } from '../../constants';
import { ContextMenuTrigger } from 'react-contextmenu';

class DimensionHeader extends Component {
  handleClickCollapse = e => {
    if (e.button === 0) {
      const { toggleCollapseDimension, dimensionId } = this.props;
      toggleCollapseDimension(dimensionId);
    }
  };
  handleClickSort = e => {
    this.button = e.button;
    if (e.button === 0) {
      const { toggleSortOrder, dimensionId } = this.props;
      if (dimensionId !== MEASURE_ID && dimensionId !== ROOT_ID) {
        toggleSortOrder(dimensionId);
      }
    }
  };

  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      if (data.action === 'remove') {
        this.props.moveDimension(
          data.dimensionId,
          toAxis(data.axis),
          toAxis(AxisType.DIMENSION)
        );
      }
      if (data.action === 'add') {
        this.props.moveDimension(
          data.newDimensionId,
          toAxis(AxisType.DIMENSION),
          toAxis(data.axis),
          data.index + 1
        );
      }
      if (data.action === 'sort') {
        this.props.toggleSortOrder(data.dimensionId);
      }
      if (data.action === 'filter') {
      }
    }
  };

  render() {
    const {
      dimensionId,
      dimensionIndex,
      sortDirection,
      caption,
      left,
      top,
      height,
      width,
      crossDimensionId,
      axis,
      isNotCollapsible,
      isCollapsed,
      previewSizes,
      gridId,
      moveDimension,
      isAttribute,
      collectMenu,
      isDropTarget
    } = this.props;

    const ids = {};
    if (axis === AxisType.ROWS) {
      ids.right = dimensionId;
      ids.bottom = crossDimensionId;
    } else {
      ids.bottom = dimensionId;
      ids.right = crossDimensionId;
    }

    const className = classNames({
      'pivotgrid-cell': true,
      'pivotgrid-dimension-header': true,
      'pivotgrid-dimension-header-column': axis === AxisType.COLUMNS,
      'pivotgrid-dimension-header-row': axis === AxisType.ROWS,
      'pivotgrid-dimension-attribute-header': isAttribute
    });
    let header = (
      <div
        key={`fixed-dim-${dimensionId}`}
        className={className}
        style={{
          position: 'absolute',
          left,
          top,
          width,
          height,
          zIndex: 3,
          boxSizing: 'border-box',
          display: 'flex'
        }}
        onClick={this.handleClickMenu}
      >
        <InnerHeader
          axis={axis}
          id={dimensionId}
          index={dimensionIndex}
          caption={caption}
          isNotCollapsible={isNotCollapsible}
          isCollapsed={isCollapsed}
          handleClick={this.handleClickSort}
          handleClickCollapse={this.handleClickCollapse}
          moveDimension={moveDimension}
          isDropTarget={isDropTarget}
          gridId={gridId}
        />
        <ResizeHandle
          position="right"
          size={height}
          id={ids.right}
          axis={AxisType.ROWS}
          gridId={gridId}
          previewSize={previewSizes.height}
        />
        <ResizeHandle
          position="bottom"
          size={width}
          gridId={gridId}
          id={ids.bottom}
          axis={AxisType.COLUMNS}
          previewSize={previewSizes.width}
        />

      </div>
    );

    return (
      <ContextMenuTrigger
        id={`context-menu-dimension-header-${gridId}`}
        holdToDisplay={-1}
        collect={collectMenu}
        onItemClick={this.handleClickMenu}
        type={'dimension-header'}
        axis={axis}
        dimensionId={dimensionId}
        sortDirection={sortDirection}
        caption={caption}
        index={dimensionIndex}
        style={{ width: 'inherit' }}
      >
        {header}
      </ContextMenuTrigger>
    );
  }
}

export default DimensionHeader;
