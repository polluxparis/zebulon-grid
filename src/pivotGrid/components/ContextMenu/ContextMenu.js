import React from 'react';
import {
  ContextMenu as ReactContextMenu,
  MenuItem,
  SubMenu
} from 'react-contextmenu';
import { isNullOrUndefined } from '../../utils/generic';
import { MEASURE_ID } from '../../constants';
import Filter from '../../containers/Filter';

// const Filter = (dimensionId, values, filter) => {
//   const checkedValues = values.map(val => ({
//     checked: filter.values.includes(val.key),
//     label: val.label,
//     value: val.key
//   }));
//   return (
//     <VirtualizedCheckbox
//       items={checkedValues}
//       //     onOk={(all, result) =>
//       //       handleFilter(all, '', '', result.map(box => box.value), false)}
//       //     onCancel={onHide}
//       maxHeight={100}
//     />
//   );
// };

const DimensionMenu = (id, trigger) => {
  const isNotCollapsible = trigger.isNotCollapsible;
  const isAddDimensionDisable = trigger.availableDimensions.length === 0;
  const filterSubmenuStyle = !isNullOrUndefined(trigger.dimensionFilter)
    ? { fontWeight: 'bold' }
    : null;
  // <SubMenu title="filter" style={filterSubmenuStyle}>
  //   <MenuItem onClick={trigger.onItemClick}>
  //     Filter(
  //     trigger.dimensionId,
  //     trigger.dimensionValues,
  //     filter={trigger.dimensionFilter})
  //   </MenuItem>
  // </SubMenu>

  // <SubMenu title="Find">
  //   <Find dimensionId={trigger.dimensionId} />
  // </SubMenu>
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
        <Filter dimensionId={trigger.dimensionId} />
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
  const isDisable = trigger.availableMeasures.length === 0;
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
      <SubMenu title="Add" disabled={isDisable}>
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
const externalMenu = (functionType, externalFunction, onClick) => {
  if (externalFunction.type === 'subMenu') {
    return (
      <SubMenu title={externalFunction.caption}>
        {externalFunction.function}
      </SubMenu>
    );
  } else {
    return (
      <MenuItem
        onClick={onClick}
        data={{ action: externalFunction.code, functionType }}
      >
        {externalFunction.caption}
      </MenuItem>
    );
  }
};
const DataCellMenu = (id, trigger) => {
  const filterSubmenuStyle = !isNullOrUndefined(trigger.filter)
    ? { fontWeight: 'bold' }
    : null;
  let fct = trigger.externalFunctions.dataCellFunctions;
  const externalCellFunctions = Object.keys(fct).map(externalFunction =>
    externalMenu('cell', fct[externalFunction], trigger.onItemClick)
  );
  fct = trigger.externalFunctions.rangeFunctions;
  const externalRangeFunctions = Object.keys(fct).map(externalFunction =>
    externalMenu('range', fct[externalFunction], trigger.onItemClick)
  );
  fct = trigger.externalFunctions.functions;
  const externalFunctions = Object.keys(fct).map(externalFunction =>
    externalMenu('function', fct[externalFunction], trigger.onItemClick)
  );
  // const externalCellFunctions = Object.keys(fct).map(externalFunction =>
  //   <MenuItem
  //     onClick={trigger.onItemClick}
  //     data={{ action: fct[externalFunction].code, functionType: 'cell' }}
  //   >
  //     {fct[externalFunction].caption}
  //   </MenuItem>
  // );
  // fct = trigger.externalFunctions.rangeFunctions;
  // const externalRangeFunctions = Object.keys(fct).map(externalFunction =>
  //   <MenuItem
  //     onClick={trigger.onItemClick}
  //     data={{ action: fct[externalFunction].code, functionType: 'range' }}
  //   >
  //     {fct[externalFunction].caption}
  //   </MenuItem>
  // );
  // fct = trigger.externalFunctions.functions;
  // const externalFunctions = Object.keys(fct).map(externalFunction =>
  //   <MenuItem
  //     onClick={trigger.onItemClick}
  //     data={{ action: fct[externalFunction].code, functionType: 'function' }}
  //   >
  //     {fct[externalFunction].caption}
  //   </MenuItem>
  // );
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
            <SubMenu title={dimension.caption}>
              <Filter dimensionId={dimension.id} style={filterSubmenuStyle} />
            </SubMenu>
          )}

      </SubMenu>
    </ReactContextMenu>
  );
};
const ContextMenu = props => {
  const { id, trigger } = props;

  if (isNullOrUndefined(trigger)) {
    return <ReactContextMenu id={id} disabled={true} />;
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
