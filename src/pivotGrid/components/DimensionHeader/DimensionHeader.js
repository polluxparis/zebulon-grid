import React, { Component } from 'react';
import classNames from 'classnames';

import InnerHeader from '../InnerHeader/InnerHeader';
import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

import { MEASURE_ID, ROOT_ID } from '../../constants';

class DimensionHeader extends Component {
  handleClickCollapse = () => {
    const { toggleCollapseDimension, dimensionId, axis } = this.props;
    toggleCollapseDimension(dimensionId);
  };
  handleClickSort = () => {
    const { toggleSortOrder, dimensionId } = this.props;
    if (dimensionId !== MEASURE_ID && dimensionId !== ROOT_ID) {
      toggleSortOrder(dimensionId);
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
      isAttribute
    } = this.props;

    const ids = {};
    if (axis === AxisType.ROWS) {
      ids.right = dimensionId;
      ids.bottom = crossDimensionId;
    } else {
      ids.bottom = dimensionId;
      ids.right = crossDimensionId;
    }
    // const computedStyle = {
    //   whiteSpace: 'nowrap',
    //   overflow: 'hidden',
    //   display: 'flex',
    //   ...style
    // };
    // const computedStyle = {
    //   whiteSpace: 'nowrap',
    //   overflow: 'hidden',
    //   display: 'flex',
    //   width: 'inherit'
    // };
    // let collapsedIcon;

    // if (!isCollapsed && !isNotCollapsible) {
    //   collapsedIcon = (
    //     <div
    //       style={{
    //         background: downArrow,
    //         backgroundSize: 'cover',
    //         height: '1em',
    //         width: '1em',
    //         marginTop: '0.1em',
    //         marginRight: '0.1em'
    //       }}
    //       onClick={this.handleClickCollapse}
    //     />
    //   );
    // } else if (isCollapsed && !isNotCollapsible) {
    //   collapsedIcon = (
    //     <div
    //       style={{
    //         background: rightArrow,
    //         backgroundSize: 'cover',
    //         height: '1em',
    //         width: '1em',
    //         marginTop: '0.1em',
    //         marginRight: '0.1em'
    //       }}
    //       onClick={this.handleClickCollapse}
    //     />
    //   );
    // } else {
    //   collapsedIcon = <div> </div>;
    // }

    // const innerHeader = (
    //   <div
    //     className="pivotgrid-header-inner"
    //     style={computedStyle}
    //     onClick={this.handleClickSort}
    //     axis={axis}
    //     index={dimensionIndex}
    //   >
    //     {collapsedIcon}<div style={{ width: 'inherit' }}>{caption}</div>
    //   </div>
    // );
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
      >
        <InnerHeader
          axis={axis}
          id={dimensionId}
          index={dimensionIndex}
          caption={caption}
          isNotCollapsible={isNotCollapsible}
          isCollapsed={isCollapsed}
          handleClickSort={this.handleClickSort}
          handleClickCollapse={this.handleClickCollapse}
          moveDimension={moveDimension}
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
  }
}
export default DimensionHeader;
