import React, { Component } from 'react';

import { AxisType, toAxis } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import ResizeHandle from '../ResizeHandle';
import { HeaderType } from '../../Cells';
import { getLeaves } from '../../utils/generic';
import InnerHeader from '../InnerHeader/InnerHeader';

// function getLeafSubheaders(header, result) {
//   if (header.subheaders && header.subheaders.length) {
//     header.subheaders.forEach(subheader =>
//       getLeafSubheaders(subheader, result)
//     );
//     return result;
//   }
//   result.push(header);
//   return result;
// }
// const lastKey = header => {
//   let key = header.key;
//   if (header.children !== undefined) {
//     const childrenKeys = Object.keys(header.children);
//     if (childrenKeys.length > 0) {
//       key = lastKey(header.children[childrenKeys[childrenKeys.length - 1]]);
//     }
//   }
//   return key;
// };

class Header extends Component {
  handleClickCollapse = () => {
    const { toggleCollapse, header } = this.props;
    toggleCollapse(header.key);
  };
  handleClick = () => {
    this.props.selectAxis(this.props.header);
  };
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
      if (data.action === 'move') {
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
      axis,
      dimensionKey,
      gridId,
      header,
      caption,
      positionStyle,
      previewSizes,
      span,
      startIndex,
      scrollLeft,
      scrollTop,
      isNotCollapsible,
      isCollapsed,
      isAffixManaged,
      moveDimension
    } = this.props;
    //
    const innerHeaderStyle = {
      width: 'inherit'
    };
    let style = positionStyle;

    // affix management to keep labels on screen (except for leaves)
    // affix management stops where you reach the last leave size
    // header cell size are recalculated to fit from the left (or top) of the grid
    // and the beginning of the nect cell to keep formats (as cenetr left or right)
    let offset;
    if (isAffixManaged) {
      if (axis === AxisType.COLUMNS) {
        offset = scrollLeft - positionStyle.left;
        style = {
          ...style,
          left: positionStyle.left + offset,
          width: positionStyle.width - offset
        };
      } else {
        offset = scrollTop - positionStyle.top;
        style = {
          ...style,
          top: positionStyle.top + offset,
          height: positionStyle.height - offset
        };
      }
    }

    const rightKey = axis === AxisType.COLUMNS ? header.key : dimensionKey;
    const bottomKey = axis === AxisType.ROWS ? header.keykey : dimensionKey;
    const rightHeader = axis === AxisType.COLUMNS ? header : null;
    const bottomHeader = axis === AxisType.ROWS ? header : null;
    return (
      <div
        className="pivotgrid-cell pivotgrid-header pivotgrid-column-header"
        style={{
          boxSizing: 'border-box',
          overflow: 'hidden',
          zIndex: 1,
          display: 'flex',
          ...style
        }}
      >
        <InnerHeader
          axis={axis}
          id={dimensionKey}
          index={null}
          caption={caption}
          isNotCollapsible={isNotCollapsible}
          isCollapsed={isCollapsed}
          handleClickCollapse={this.handleClickCollapse}
          handleClick={this.handleClick}
          handleClickMenu={this.handleClickMenu}
          moveDimension={moveDimension}
          collectMenu={this.collectMenu}
          gridId={gridId}
        />
        <ResizeHandle
          position="right"
          size={positionStyle.height}
          id={rightKey}
          axis={axis}
          header={rightHeader}
          previewSize={previewSizes.height}
          gridId={gridId}
        />
        <ResizeHandle
          position="bottom"
          size={positionStyle.width}
          id={bottomKey}
          gridId={gridId}
          axis={axis}
          header={bottomHeader}
          previewSize={previewSizes.width}
        />
      </div>
    );
  }
}

export default Header;
