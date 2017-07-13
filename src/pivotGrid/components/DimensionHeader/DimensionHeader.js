import React, { Component } from 'react';
import classNames from 'classnames';

import InnerHeader from '../InnerHeader/InnerHeader';
import { AxisType, toAxis } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

import { MEASURE_ID, ROOT_ID } from '../../constants';

import {
  ContextMenu,
  MenuItem,
  ContextMenuTrigger,
  connectMenu
} from 'react-contextmenu';

// const collectMenu = props => {
//   const a = 3;
//   return props;
// };

class DimensionHeader extends Component {
  handleClickCollapse = e => {
    if (e.button === 0) {
      const { toggleCollapseDimension, dimensionId, axis } = this.props;
      toggleCollapseDimension(dimensionId);
    }
  };
  handleClick = e => {
    this.button = e.button;
    if (e.button === 0) {
      const { toggleSortOrder, dimensionId } = this.props;
      if (dimensionId !== MEASURE_ID && dimensionId !== ROOT_ID) {
        toggleSortOrder(dimensionId);
      }
    }
  };

  // };
  // // this.props.handleMouseDown(columnIndex, rowIndex);
  // handleMouseUp = () => (this.isRightClick = false);
  // this.props.handleMouseDown(columnIndex, rowIndex);
  // collect = props => {
  //   return props;
  // };

  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      console.log(data);
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
          data.index
        );
      }
    }
  };

  render() {
    const {
      dimensionId,
      dimensionIndex,
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
      // toggleCollapseDimension,
      moveDimension,
      isAttribute,
      collectMenu
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
    return (
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
          handleClick={this.handleClick}
          handleClickCollapse={this.handleClickCollapse}
          handleClickMenu={this.handleClickMenu}
          moveDimension={moveDimension}
          collectMenu={collectMenu}
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
    // return (
    //   <div>
    //     <ContextMenuTrigger
    //       id={'toto2'}
    //       holdToDisplay={1000}
    //       disable={false}
    //       collect={collectMenu}
    //       onItemClick={this.handleClickMenu}
    //       type={'dimension-header'}
    //       axis={axis}
    //       dimensionId={dimensionId}
    //       caption={caption}
    //       index={dimensionIndex}
    //       button={this.button}
    //     >
    //       {header}
    //     </ContextMenuTrigger>
    //   </div>
    // );
  }
}
export default DimensionHeader;
