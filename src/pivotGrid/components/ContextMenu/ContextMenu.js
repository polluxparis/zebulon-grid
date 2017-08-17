import React from 'react';
import {
  ContextMenu as ReactContextMenu,
  MenuItem,
  SubMenu
} from 'react-contextmenu';
import classnames from 'classnames';

import { isNullOrUndefined } from '../../utils/generic';
import { MEASURE_ID } from '../../constants';
import Filter from '../../containers/Filter';

const DimensionMenu = (id, trigger) => {
  const isNotCollapsible = trigger.isNotCollapsible;
  const isAddDimensionDisable = trigger.availableDimensions.length === 0;
  return (
    <ReactContextMenu id={id}>
      <MenuItem onClick={trigger.onItemClick} data={{ action: 'sort' }}>
        {`Sort  ${trigger.direction} `}
      </MenuItem>
      <MenuItem
        onClick={trigger.onItemClick}
        data={{ action: 'expand all' }}
        disabled={isNotCollapsible}
      >
        Expand all
      </MenuItem>
      <MenuItem
        onClick={trigger.onItemClick}
        data={{ action: 'collapse all' }}
        disabled={isNotCollapsible}
      >
        Collapse all
      </MenuItem>
      <SubMenu
        title="Filter"
        style={
          !isNullOrUndefined(trigger.dimensionFilter)
            ? { fontWeight: 'bold' }
            : null
        }
      >
        <div style={{ height: 400 }}>
          <Filter dimensionId={trigger.dimensionId} />
        </div>
      </SubMenu>

      <MenuItem onClick={trigger.onItemClick} data={{ action: 'remove' }}>
        Remove
      </MenuItem>

      <SubMenu title="Add dimension" disabled={isAddDimensionDisable}>
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
};

const MeasureMenu = (id, trigger) => {
  const isDisabled = trigger.availableMeasures.length === 0;
  return (
    <ReactContextMenu id={id}>
      <MenuItem onClick={trigger.onItemClick} data={{ action: 'move' }}>
        Move measures
      </MenuItem>
      <MenuItem
        onClick={trigger.onItemClick}
        data={{ action: 'remove' }}
        disabled={Object.keys(trigger.measures).length < 2}
      >
        Remove
      </MenuItem>
      <SubMenu title="Add" disabled={isDisabled}>
        {trigger.availableMeasures.map(measure =>
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
};

const DataCellMenu = (id, trigger) => {
  let fct = trigger.externalFunctions.dataCellFunctions;
  const externalCellFunctions = Object.keys(fct).map(externalFunction =>
    <MenuItem
      onClick={trigger.onItemClick}
      data={{ action: fct[externalFunction].code, functionType: 'cell' }}
    >
      {fct[externalFunction].caption}
    </MenuItem>
  );
  fct = trigger.externalFunctions.rangeFunctions;
  const externalRangeFunctions = Object.keys(fct).map(externalFunction =>
    <MenuItem
      onClick={trigger.onItemClick}
      data={{ action: fct[externalFunction].code, functionType: 'range' }}
    >
      {fct[externalFunction].caption}
    </MenuItem>
  );
  fct = trigger.externalFunctions.functions;
  const externalFunctions = Object.keys(fct).map(externalFunction =>
    <MenuItem
      onClick={trigger.onItemClick}
      data={{ action: fct[externalFunction].code, functionType: 'function' }}
    >
      {fct[externalFunction].caption}
    </MenuItem>
  );
  return (
    <ReactContextMenu id={id}>
      <MenuItem onClick={trigger.onItemClick} data={{ action: 'drilldown' }}>
        DrillDown
      </MenuItem>
      {externalCellFunctions}
      {externalRangeFunctions}
      {externalFunctions}
      <SubMenu title="Filters">
        {trigger.dimensions
          .filter(dimension => dimension.id !== MEASURE_ID)
          .map(dimension =>
            <SubMenu
              key={dimension.id}
              title={
                <span
                  className={classnames({
                    'react-contextmenu-item-filtered': !isNullOrUndefined(
                      trigger.filters[dimension.id]
                    )
                  })}
                >
                  {dimension.caption}
                </span>
              }
            >
              <Filter dimensionId={dimension.id} />
            </SubMenu>
          )}
      </SubMenu>
    </ReactContextMenu>
  );
};

const ContextMenu = props => {
  const { id, trigger } = props;
  if (isNullOrUndefined(trigger)) {
    return null;
  }

  if (trigger.type === 'dimension-header') {
    return DimensionMenu(id, trigger);
  } else if (trigger.type === `header-${trigger.axis}`) {
    return MeasureMenu(id, trigger);
  } else if (trigger.type === 'data-cell') {
    return DataCellMenu(id, trigger);
  }
};

export default ContextMenu;
