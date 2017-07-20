import { connect } from 'react-redux';
import { dimensionValuesSelector } from '../selectors';
import { Filter } from '../components/Forms/Filter';
import { addFilter, deleteFilter } from '../actions';
// import {
//   columnDimensionsSelector,
//   rowDimensionsSelector,
//   columnHeadersWidthSelector,
//   rowHeadersWidthSelector,
//   crossPositionsSelector,
//   previewSizesSelector,
//   availableDimensionsSelector
// } from '../selectors';
// import DimensionHeaders from '../components/DimensionHeaders/DimensionHeaders';
// import {
//   toggleCollapseDimension,
//   toggleSortOrder,
//   moveDimension
// } from '../actions';

const mapStateToProps = (state, ownProps) => {
	return {
		dimensionValues: dimensionValuesSelector(state)
	};
};
const mapDispatchToProps = dispatch => ({
	setFilter: (dimensionId, operator, filterKeys) =>
		dispatch(addFilter(dimensionId, operator, null, filterKeys, false)),
	deleteFilter: dimensionId => dispatch(deleteFilter(dimensionId))
});
// const mapDispatchToProps = dispatch => ({});
export default connect(mapStateToProps, mapDispatchToProps)(Filter);
