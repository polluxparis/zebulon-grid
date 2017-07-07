import PropTypes from 'prop-types';
import React, { PureComponent } from 'react';
import { isNull, isNullOrUndefined, OsInRange } from '../../utils/generic';
/**
 * This HOC decorates a virtualized component and responds to arrow-key events by scrolling one row or column at a time.
 */
export default class ArrowKeyStepper extends PureComponent {
  constructor(props, context) {
    super(props, context);

    this.state = {
      scrollToColumn: props.scrollToColumn,
      scrollToRow: props.scrollToRow
    };

    this.columnStartIndex = 0;
    this.columnStopIndex = 0;
    this.rowStartIndex = 0;
    this.rowStopIndex = 0;
    this.shouldUpdate = true;
    this.onKeyDown = this.onKeyDown.bind(this);
    this.onSectionRendered = this.onSectionRendered.bind(this);
  }

  componentWillReceiveProps(nextProps) {
    const { scrollToColumn, scrollToRow } = nextProps;
    this.shouldUpdate = true;
    const {
      scrollToColumn: prevScrollToColumn,
      scrollToRow: prevScrollToRow
    } = this.props;

    if (
      prevScrollToColumn !== scrollToColumn &&
      prevScrollToRow !== scrollToRow
    ) {
      this.setState({
        scrollToColumn,
        scrollToRow
      });
    } else if (prevScrollToColumn !== scrollToColumn) {
      this.setState({ scrollToColumn });
    } else if (prevScrollToRow !== scrollToRow) {
      this.setState({ scrollToRow });
    } else {
      this.shouldUpdate = false;
    }
  }
  // shouldComponentUpdate() {
  //   return this.shouldUpdate;
  // }
  // componentDidUpdate() {
  //   this.setState({
  //     scrollToRow: this.rowStartIndex,
  //     scrollToColumn: this.columnStartIndex
  //   });
  // }
  onKeyDown(event) {
    const { columnCount, disabled, mode, rowCount } = this.props;

    if (disabled) {
      return;
    }

    const {
      scrollToColumn: scrollToColumnPrevious,
      scrollToRow: scrollToRowPrevious
    } = this.state;

    let { scrollToColumn, scrollToRow } = this.state;
    // The above cases all prevent default event event behavior.
    // This is to keep the grid from scrolling after the snap-to update.

    console.log('arrowKeyStepper');
    let isLastSelected, rowSelectedIndex, columnSelectedIndex;
    const rowVisibleCount = this.rowStopIndex - this.rowStartIndex - 1;
    const columnVisibleCount = this.columnStopIndex - this.columnStartIndex - 1;
    switch (event.key) {
      case 'ArrowDown':
        if (mode === 'cells') {
          scrollToRow = Math.min(scrollToRow + 1, rowCount - 1);
        } else if (mode === 'align:top-left') {
          // To avoid weird behaviour when the grid has scrolled to the bottom
          rowSelectedIndex = this.props.selectedRange.selectedCellEnd.rowIndex;
          if (
            isNullOrUndefined(this.props.selectedRange.selectedCellStart) ||
            rowSelectedIndex > this.rowStopIndex ||
            rowSelectedIndex < this.rowStartIndex ||
            this.props.selectedRange.selectedCellStart.columnIndex >
              this.columnStopIndex ||
            this.props.selectedRange.selectedCellEnd.columnIndex <
              this.columnStartIndex
          ) {
            rowSelectedIndex = this.rowStopIndex - 1;
          }
          isLastSelected = this.rowStopIndex - 1 <= rowSelectedIndex + 1;
          if (isLastSelected) {
            console.log([
              scrollToRow,
              this.rowStartIndex,
              rowSelectedIndex,
              rowVisibleCount
            ]);
            scrollToRow = Math.min(scrollToRow + 1, rowCount - 1);
          }
          // if (this.rowStopIndex + 1 < rowCount) {
          //   scrollToRow = Math.min(this.rowStartIndex + 1, rowCount - 1);
          // }
        } else if (mode === 'align:bottom-right') {
          scrollToRow = Math.min(this.rowStopIndex + 1, rowCount - 1);
        } else {
          scrollToRow = Math.min(this.rowStopIndex + 1, rowCount - 1);
        }
        break;
      case 'ArrowUp':
        if (mode === 'cells') {
          scrollToRow = Math.max(scrollToRow - 1, 0);
        } else if (mode === 'align:top-left') {
          rowSelectedIndex = this.props.selectedRange.selectedCellStart
            .rowIndex;
          if (
            isNullOrUndefined(this.props.selectedRange.selectedCellStart) ||
            rowSelectedIndex > this.rowStopIndex ||
            rowSelectedIndex < this.rowStartIndex ||
            this.props.selectedRange.selectedCellStart.columnIndex >
              this.columnStopIndex ||
            this.props.selectedRange.selectedCellEnd.columnIndex <
              this.columnStartIndex
          ) {
            rowSelectedIndex = this.rowStartIndex;
          }
          isLastSelected = this.rowStartIndex >= rowSelectedIndex - 1;
          if (isLastSelected) {
            scrollToRow = Math.max((scrollToRow || this.rowStartIndex) - 1, 0);
          }
        } else if (mode === 'align:bottom-right') {
          scrollToRow = Math.max(this.rowStopIndex - 1, 0);
        } else {
          scrollToRow = Math.max(this.rowStartIndex - 1, 0);
        }
        break;
      case 'ArrowLeft':
        if (mode === 'cells') {
          scrollToColumn = Math.max(scrollToColumn - 1, 0);
        } else if (mode === 'align:top-left') {
          columnSelectedIndex = this.props.selectedRange.selectedCellStart
            .columnIndex;
          if (
            isNullOrUndefined(this.props.selectedRange.selectedCellStart) ||
            columnSelectedIndex > this.columnStopIndex ||
            columnSelectedIndex < this.columnStartIndex ||
            this.props.selectedRange.selectedCellStart.rowIndex >
              this.rowStopIndex ||
            this.props.selectedRange.selectedCellEnd.rowIndex <
              this.rowStartIndex
          ) {
            columnSelectedIndex = this.columnStartIndex;
          }
          isLastSelected = this.columnStartIndex >= columnSelectedIndex - 1;
          if (isLastSelected) {
            scrollToColumn = Math.max(
              (scrollToColumn || this.columnStartIndex) - 1,
              0
            );
          }
        } else if (mode === 'align:bottom-right') {
          scrollToColumn = Math.max(this.columnStopIndex - 1, 0);
        } else {
          scrollToColumn = Math.max(this.columnStartIndex - 1, 0);
        }
        break;
      case 'ArrowRight':
        if (mode === 'cells') {
          scrollToColumn = Math.min(
            (scrollToColumn || this.columnStartIndex) + 1,
            columnCount - 1
          );
        } else if (mode === 'align:top-left') {
          // To avoid weird behaviour when the grid has scrolled to the right
          columnSelectedIndex = this.props.selectedRange.selectedCellEnd
            .columnIndex;
          if (
            isNullOrUndefined(this.props.selectedRange.selectedCellStart) ||
            columnSelectedIndex > this.columnStopIndex ||
            columnSelectedIndex < this.columnStartIndex ||
            this.props.selectedRange.selectedCellStart.rowIndex >
              this.rowStopIndex ||
            this.props.selectedRange.selectedCellEnd.rowIndex <
              this.rowStartIndex
          ) {
            rowSelectedIndex = this.rowStopIndex - 1;
          }
          isLastSelected = this.columnStopIndex - 1 <= columnSelectedIndex;
          if (isLastSelected) {
            scrollToColumn = Math.min(
              (scrollToColumn || this.columnStartIndex) + 1,
              columnCount - 1
            );
          }
        } else if (mode === 'align:bottom-right') {
          scrollToColumn = Math.min(this.columnStopIndex + 1, columnCount - 1);
        } else {
          scrollToColumn = Math.min(this.columnStopIndex + 1, columnCount - 1);
        }
        break;
      default:
        break;
    }
    if (!isNull(isLastSelected)) {
      event.preventDefault();
    }
    if (
      scrollToColumn !== scrollToColumnPrevious ||
      scrollToRow !== scrollToRowPrevious
    ) {
      this.setState({ scrollToColumn, scrollToRow });
    }
  }

  onSectionRendered({
    columnStartIndex,
    columnStopIndex,
    rowStartIndex,
    rowStopIndex
  }) {
    this.columnStartIndex = columnStartIndex;
    this.columnStopIndex = columnStopIndex;
    this.rowStartIndex = rowStartIndex;
    this.rowStopIndex = rowStopIndex;
    console.log(['rendered', rowStartIndex]);
    // this.setScrollIndexes(columnStartIndex, rowStartIndex);
    // this.setState({
    //   scrollToRow: rowStartIndex,
    //   scrollToColumn: columnStartIndex
    // });
    //
  }

  setScrollIndexes({ scrollToColumn, scrollToRow }) {
    this.setState({
      scrollToColumn,
      scrollToRow
    });
  }

  render() {
    const { className, children } = this.props;
    const { scrollToColumn, scrollToRow } = this.state;
    return (
      /* eslint-disable jsx-a11y/no-static-element-interactions */
      <div className={className} onKeyDown={this.onKeyDown}>
        {children({
          onSectionRendered: this.onSectionRendered,
          scrollToColumn,
          scrollToRow
        })}
      </div>
      /* eslint-enable */
    );
  }
}

ArrowKeyStepper.defaultProps = {
  disabled: false,
  mode: 'edges',
  scrollToColumn: 0,
  scrollToRow: 0,
  className: undefined
};

ArrowKeyStepper.propTypes = {
  children: PropTypes.func.isRequired,
  className: PropTypes.string,
  columnCount: PropTypes.number.isRequired,
  disabled: PropTypes.bool.isRequired,
  mode: PropTypes.oneOf([
    'cells',
    'edges',
    'align:top-left',
    'align:bottom-right'
  ]),
  rowCount: PropTypes.number.isRequired,
  scrollToColumn: PropTypes.number.isRequired,
  scrollToRow: PropTypes.number.isRequired
};
