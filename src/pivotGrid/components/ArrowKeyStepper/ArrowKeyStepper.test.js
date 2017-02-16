import React from 'react';
import { shallow } from 'enzyme';
import ArrowKeyStepper from './ArrowKeyStepper';

describe('ArrowKeyStepper', () => {
  it('renders without crashing', () => {
    shallow(
      <ArrowKeyStepper
        columnCount={10}
        mode="align:top-left"
        rowCount={20}
        scrollToRow={4}
        scrollToColumn={5}
      >
        {() => <div>mock grid</div>}
      </ArrowKeyStepper>,
    );
  });
});
