import React, { Component } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import ResizeHandle from '../ResizeHandle';

const rightArrow =
  'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAIElEQVQ4jWNgGAX4QB0UU2zAMDCEIgMGTjOyAaOAAAAA6dUK1fxYl1IAAAAASUVORK5CYII=)';
const downArrow =
  'url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAJ0lEQVQ4jWNgGAWDE9RBMbHiOBXWERAj2hCSNeMyhCxAkeZRQCQAAFO3CtUd1w9cAAAAAElFTkSuQmCC)';

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
const lastKey = header => {
  let key = header.key;
  if (header.children !== undefined) {
    const childrenKeys = Object.keys(header.children);
    if (childrenKeys.length > 0) {
      key = lastKey(header.children[childrenKeys[childrenKeys.length - 1]]);
    }
  }
  return key;
};

class Header extends Component {
  constructor() {
    super();
    this.handleClick = this.handleClick.bind(this);
  }

  handleClick() {
    const { toggleCollapse, header } = this.props;
    toggleCollapse(header.key);
  }

  render() {
    const {
      axis,
      dimensionKey,
      getLastChildSize,
      gridId,
      header,
      caption,
      positionStyle,
      previewSizes,
      span,
      startIndex,
      scrollLeft,
      scrollTop,
      isLastDimension,
      isAffixManaged,
      toggleCollapse
    } = this.props;
    const { left, top, width, height } = positionStyle;
    const { x, y } = header;
    let style = { height, width };
    // Handle affix
    // if (span > 1 && x <= startIndex) {
    if (isAffixManaged) {
      let offset;
      const lastChildSize = getLastChildSize(header);
      if (axis === AxisType.COLUMNS) {
        offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
        style.left = offset;
        style.position = 'relative';
        style.width -= offset;
      } else {
        offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
        style.top = offset;
        style.position = 'relative';
        style.height -= offset;
      }
    }
    // if (span > 1 && x <= startIndex) {
    //   let offset;
    //   const lastChildSize = getLastChildSize(header);
    //   if (axis === AxisType.COLUMNS) {
    //     offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
    //     style = { position: 'relative', left: offset };
    //   } else {
    //     offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
    //     style = { position: 'relative', top: offset };
    //   }
    // }
    const computedStyle = {
      whiteSpace: 'nowrap',
      overflow: 'hidden',
      display: 'flex',
      ...style
    };

    let collapsedIcon;
    if (header.span > 1 && !isLastDimension) {
      collapsedIcon = (
        <div
          style={{
            background: rightArrow,
            backgroundSize: 'cover',
            height: '1em',
            width: '1em',
            marginTop: '0.1em',
            marginRight: '0.1em'
          }}
          onClick={this.handleClick}
        />
      );
    } else if (header.span === 1 && header.isCollapsed) {
      collapsedIcon = (
        <div
          style={{
            background: downArrow,
            backgroundSize: 'cover',
            height: '1em',
            width: '1em',
            marginTop: '0.1em',
            marginRight: '0.1em'
          }}
          onClick={this.handleClick}
        />
      );
    } else {
      collapsedIcon = <div> </div>;
    }
    const innerHeader = (
      <div className="pivotgrid-header-inner" style={computedStyle}>
        {collapsedIcon}<div style={{ width: 'inherit' }}>{caption}</div>
      </div>
    );
    // if (!header.dim) {
    //   // Measure header
    //   dimensionId = MEASURE_ID;
    // } else if (header.dim.dimension) {
    //   // Normal header
    //   dimensionId = header.dim.dimension.id;
    // } else {
    //   // Total header
    //   dimensionId = TOTAL_ID;
    // }
    //const leafSubheaders = header.subheaders ? getLeafSubheaders(header, []) : [];
    const key = lastKey(header);
    const rightKey = axis === AxisType.COLUMNS ? key : dimensionKey;
    const bottomKey = axis === AxisType.ROWS ? key : dimensionKey;
    return (
      <div
        key={`fixed-${axis}-${x}-${y}`}
        className="pivotgrid-cell pivotgrid-header pivotgrid-column-header"
        style={{
          boxSizing: 'border-box',
          overflow: 'hidden',
          // border: 'solid lightgrey thin',
          // backgroundColor: '#eef8fb',
          zIndex: 1,
          display: 'flex',
          ...positionStyle
        }}
      >
        {innerHeader}
        <ResizeHandle
          position="right"
          size={height}
          id={rightKey}
          // lastKey={leafSubheaders}
          axis={axis}
          previewSize={previewSizes.height}
          gridId={gridId}
        />
        <ResizeHandle
          position="bottom"
          size={width}
          id={bottomKey}
          gridId={gridId}
          // leafSubheaders={leafSubheaders}
          axis={axis}
          previewSize={previewSizes.width}
        />
      </div>
    );
  }
}

export default Header;
