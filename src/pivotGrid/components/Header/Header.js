import React, { Component } from 'react';

import { AxisType, toAxis, MEASURE_ID } from '../../constants';
import ResizeHandle from '../ResizeHandle/ResizeHandle';
import InnerHeader from '../InnerHeader/InnerHeader';
import { ContextMenuTrigger } from 'react-contextmenu';
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
      if (data.action === 'move') {
        this.props.moveDimension(
          MEASURE_ID,
          toAxis(data.axis),
          toAxis(
            data.axis === AxisType.COLUMNS ? AxisType.ROWS : AxisType.COLUMNS
          )
        );
      }
      if (data.action === 'remove') {
        this.props.toggleMeasure(data.measureId);
      }
      if (data.action === 'add') {
        this.props.toggleMeasure(data.newMeasureId);
      }
    }
  };
  render() {
    const {
      axis,
      dimensionId,
      // dimensionKey,
      // measureIndex,
      gridId,
      header,
      caption,
      positionStyle,
      previewSizes,
      scrollLeft,
      scrollTop,
      isNotCollapsible,
      isCollapsed,
      isAffixManaged,
      moveDimension,
      collectMenu,
      isDropTarget
    } = this.props;
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

    const rightKey = axis === AxisType.COLUMNS ? header.key : dimensionId;
    const bottomKey = axis === AxisType.ROWS ? header.key : dimensionId;
    const rightHeader = axis === AxisType.COLUMNS ? header : null;
    const bottomHeader = axis === AxisType.ROWS ? header : null;

    const head = (
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
          id={dimensionId}
          index={null}
          caption={caption}
          isNotCollapsible={isNotCollapsible}
          isCollapsed={isCollapsed}
          handleClickCollapse={this.handleClickCollapse}
          handleClick={this.handleClick}
          handleClickMenu={this.handleClickMenu}
          moveDimension={moveDimension}
          isDropTarget={isDropTarget}
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
    if (dimensionId === MEASURE_ID) {
      return (
        <ContextMenuTrigger
          id={`context-menu-${axis}-${gridId}`}
          holdToDisplay={-1}
          collect={collectMenu}
          onItemClick={this.handleClickMenu}
          type={`header-${axis}`}
          axis={axis}
          dimensionId={dimensionId}
          measureId={header.id}
          caption={caption}
          style={{ width: 'inherit' }}
        >
          {head}
        </ContextMenuTrigger>
      );
    } else {
      return head;
    }
  }
}

export default Header;
