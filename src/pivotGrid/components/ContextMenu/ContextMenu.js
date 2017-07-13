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
      </ReactContextMenu>
    );
  }
};

export default ContextMenu;
