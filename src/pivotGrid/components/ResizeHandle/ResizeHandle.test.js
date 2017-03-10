import React from 'react';
import { shallow } from 'enzyme';
import ResizeHandle from './ResizeHandle';

const OriginalResizeHandle = ResizeHandle.DecoratedComponent;

describe('ResizeHandle renders without crashing', () => {
  test('when no position', () => {
    shallow(<OriginalResizeHandle connectDragSource={item => item} />);
  });
  test('with position = right', () => {
    shallow(
      <OriginalResizeHandle
        connectDragSource={item => item}
        position={'right'}
      />
    );
  });
  test('with position = bottom', () => {
    shallow(
      <OriginalResizeHandle
        connectDragSource={item => item}
        position={'bottom'}
      />
    );
  });
});
