import React, { Component } from 'react';
import classNames from 'classnames';

import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';
import { rightArrow, downArrow } from '../../icons';

class DimensionHeader extends Component {
  constructor() {
    super();
    this.handleClick = this.handleClick.bind(this);
  }

  handleClick() {
    const { toggleCollapseDimension, dimensionId, axis } = this.props;
    toggleCollapseDimension(dimensionId);
  }

  render() {
    const {
      dimensionId,
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
      toggleCollapseDimension,
      isAttribute
    } = this.props;

    const ids = {};
    if (axis === 'rows') {
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
    const computedStyle = {
      whiteSpace: 'nowrap',
      overflow: 'hidden',
      display: 'flex'
    };
    let collapsedIcon;

    if (!isCollapsed && !isNotCollapsible) {
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
    } else if (isCollapsed && !isNotCollapsible) {
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
    const className = classNames({
      'pivotgrid-cell': true,
      'pivotgrid-dimension-header': true,
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
        {innerHeader}
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
