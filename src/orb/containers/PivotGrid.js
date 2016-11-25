import { connect } from 'react-redux';

import PivotGrid from '../components/Grid/Grid';
import { getLayout, getHeaderSizes, getCellSizes } from '../selectors';
import { updateCellSize } from '../actions';
import { AxisType } from '../Axis';

const mapStateToProps = state => ({
  width: state.config.width,
  layout: getLayout(state),
  defaultCellSizes: getCellSizes(state),
  headerSizes: getHeaderSizes(state),
  sizes: state.sizes,
});

const mapDispatchToProps = dispatch => ({
  updateCellSize: ({ handle, offset, initialOffset, sizes, defaultCellSizes }) => {
    if (handle.leafSubheaders && handle.leafSubheaders.length &&
      (
        (handle.axis === AxisType.COLUMNS && handle.position === 'right')
        || (handle.axis === AxisType.ROWS && handle.position === 'bottom')
      )
    ) {
      const fractionalOffset = {
        x: (offset.x - initialOffset.x) / handle.leafSubheaders.length,
        y: (offset.y - initialOffset.y) / handle.leafSubheaders.length,
      };
      handle.leafSubheaders.forEach((subheader) => {
        dispatch(
          updateCellSize({
            handle: { ...handle, leafSubheaders: [], id: subheader.key },
            offset: fractionalOffset,
            initialOffset: { x: 0, y: 0 },
            defaultCellSizes,
            sizes,
          }),
      );
      });
    } else {
      dispatch(updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }));
    }
  },
});

const mergeProps = (
  { height, width, layout, headerSizes, sizes, defaultCellSizes },
  { updateCellSize },
) => ({
  height,
  width,
  layout,
  headerSizes,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
});

export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(PivotGrid);
