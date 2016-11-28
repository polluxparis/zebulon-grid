import React from 'react';
import { ResizableBox } from 'react-resizable';
import VirtualizedCheckbox from 'react-virtualized-checkbox';

import { twoArraysIntersect } from '../../utils/generic';

const startingHeight = 223;
const startingWidth = 301;

const FilterPanel = ({ values, filter, handleFilter, onHide, style }) => {
  let checkedValues;
  if (filter && filter.staticValue.length < values.length) {
    checkedValues = twoArraysIntersect(values.map(val => val.value), filter.staticValue);
  } else {
    checkedValues = values.map(value => value.value);
  }

  const options = values.map(val => ({
    checked: checkedValues.includes(val.value),
    label: val.label,
    value: val.value,
  }));

  const checkboxes = (
    <VirtualizedCheckbox
      items={options}
      onOk={(all, result) => handleFilter(all, '', '', result.map(box => box.value), false)}
      onCancel={onHide}
      maxHeight={startingHeight}
    />);

  const divStyle = {
    backgroundColor: 'white',
    border: 'solid 1px',
    boxShadow: '0 5px 15px #9d9d9d',
    display: 'flex',
    flexDirection: 'column',
    fontSize: '90%',
    height: '100%',
    justifyContent: 'space-between',
    padding: '3px',
    width: '100%',
    zIndex: 100,
  };

  return (
    <div style={{ position: 'absolute', ...style }}>
      <ResizableBox
        width={startingWidth}
        height={startingHeight}
        minConstraints={[startingWidth, startingHeight]}
      >
        <div style={divStyle}>
          {checkboxes}
        </div>
      </ResizableBox>
    </div>
  );
};

export default FilterPanel;
