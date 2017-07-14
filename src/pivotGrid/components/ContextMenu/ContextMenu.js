import React from 'react';
import {
  ContextMenu as ReactContextMenu,
  MenuItem,
  SubMenu
} from 'react-contextmenu';
import { isNullOrUndefined } from '../../utils/generic';

const ContextMenu = props => {
  const { id, trigger } = props;
  if (isNullOrUndefined(trigger)) {
    return <ReactContextMenu id={id} disabled={true} />;
  }

  if (trigger.type === 'dimension-header') {
    const isDisable = trigger.availableDimensions.length === 0;
    return (
      <ReactContextMenu id={id}>
        <MenuItem onClick={trigger.onItemClick} data={{ action: 'sort' }}>
          {`Sort  ${trigger.direction} `}
        </MenuItem>
        <MenuItem onClick={trigger.onItemClick} data={{ action: 'filter' }}>
          Filter
        </MenuItem>
        <MenuItem onClick={trigger.onItemClick} data={{ action: 'remove' }}>
          Remove
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
      </ReactContextMenu>
    );
  } else if (trigger.type === `header-${trigger.axis}`) {
    const isDisable = trigger.availableMeasure.length === 0;
    return (
      <ReactContextMenu id={id}>
        <MenuItem onClick={trigger.onItemClick} data={{ action: 'move' }}>
          Move measures
        </MenuItem>
        <MenuItem onClick={trigger.onItemClick} data={{ action: 'remove' }}>
          Remove
        </MenuItem>
        <SubMenu title="Add" disabled={isDisable}>
          {trigger.availableMeasure.map(measure =>
            <MenuItem
              onClick={trigger.onItemClick}
              data={{ action: 'add', newMeasureId: measure.id }}
            >
              {measure.caption}
            </MenuItem>
          )}

        </SubMenu>
      </ReactContextMenu>
    );
  }
};

export default ContextMenu;
