import React, { PureComponent, PropTypes } from 'react';

/**
 * This HOC decorates a virtualized component and responds to arrow-key events
 by scrolling one row or column at a time.
 */
export default class ArrowKeyStepper extends PureComponent {
  constructor(props, context) {
    super(props, context);

    this.state = {
      scrollToColumn: props.scrollToColumn,
      scrollToRow: props.scrollToRow,
    };

    this.columnStartIndex = 0;
    this.columnStopIndex = 0;
    this.rowStartIndex = 0;
    this.rowStopIndex = 0;

    this.onKeyDown = this.onKeyDown.bind(this);
    this.onSectionRendered = this.onSectionRendered.bind(this);
  }

  componentWillUpdate(nextProps) {
    const { scrollToColumn, scrollToRow } = nextProps;

    if (this.props.scrollToColumn !== scrollToColumn) {
      this.setState({ scrollToColumn });
    }

    if (this.props.scrollToRow !== scrollToRow) {
      this.setState({ scrollToRow });
    }
  }

  onKeyDown(event) {
    const { columnCount, disabled, mode, rowCount } = this.props;

    if (disabled) {
      return;
    }

    const {
      scrollToColumn: scrollToColumnPrevious,
      scrollToRow: scrollToRowPrevious,
    } = this.state;

    let { scrollToColumn, scrollToRow } = this.state;

    // The above cases all prevent default event event behavior.
    // This is to keep the grid from scrolling after the snap-to update.
    switch (event.key) {
      case 'ArrowDown':
        if (mode === 'cells') {
          scrollToRow = Math.min(scrollToRow + 1, rowCount - 1);
        } else if (mode === 'align:top-left') {
          // To avoid weird behaviour when the grid has scrolled to the bottom
          if (this.rowStopIndex + 1 < rowCount) {
            scrollToRow = Math.min(this.rowStartIndex + 1, rowCount - 1);
          }
        } else if (mode === 'align:bottom-right') {
          scrollToRow = Math.min(this.rowStopIndex + 1, rowCount - 1);
        } else {
          scrollToRow = Math.min(this.rowStopIndex + 1, rowCount - 1);
        }
        break;
      case 'ArrowLeft':
        if (mode === 'cells') {
          scrollToColumn = Math.max(scrollToColumn - 1, 0);
        } else if (mode === 'align:top-left') {
          scrollToColumn = Math.max(this.columnStartIndex - 1, 0);
        } else if (mode === 'align:bottom-right') {
          scrollToColumn = Math.max(this.columnStopIndex - 1, 0);
        } else {
          scrollToColumn = Math.max(this.columnStartIndex - 1, 0);
        }
        break;
      case 'ArrowRight':
        if (mode === 'cells') {
          scrollToColumn = Math.min(scrollToColumn + 1, columnCount - 1);
        } else if (mode === 'align:top-left') {
          // To avoid weird behaviour when the grid has scrolled to the right
          if (this.columnStopIndex + 1 < columnCount) {
            scrollToColumn = Math.min(
              this.columnStartIndex + 1,
              columnCount - 1,
            );
          }
        } else if (mode === 'align:bottom-right') {
          scrollToColumn = Math.min(this.columnStopIndex + 1, columnCount - 1);
        } else {
          scrollToColumn = Math.min(this.columnStopIndex + 1, columnCount - 1);
        }
        break;
      case 'ArrowUp':
        if (mode === 'cells') {
          scrollToRow = Math.max(scrollToRow - 1, 0);
        } else if (mode === 'align:top-left') {
          scrollToRow = Math.max(this.rowStartIndex - 1, 0);
        } else if (mode === 'align:bottom-right') {
          scrollToRow = Math.max(this.rowStopIndex - 1, 0);
        } else {
          scrollToRow = Math.max(this.rowStartIndex - 1, 0);
        }
        break;
      default:
        break;
    }

    if (
      scrollToColumn !== scrollToColumnPrevious ||
      scrollToRow !== scrollToRowPrevious
    ) {
      event.preventDefault();

      this.setState({ scrollToColumn, scrollToRow });
    }
  }

  onSectionRendered(
    { columnStartIndex, columnStopIndex, rowStartIndex, rowStopIndex },
  ) {
    this.columnStartIndex = columnStartIndex;
    this.columnStopIndex = columnStopIndex;
    this.rowStartIndex = rowStartIndex;
    this.rowStopIndex = rowStopIndex;
  }

  render() {
    const { className, children } = this.props;
    const { scrollToColumn, scrollToRow } = this.state;

    return (
      <div className={className} onKeyDown={this.onKeyDown}>
        {children({
          onSectionRendered: this.onSectionRendered,
          scrollToColumn,
          scrollToRow,
        })}
      </div>
    );
  }
}

ArrowKeyStepper.defaultProps = {
  disabled: false,
  mode: 'edges',
  scrollToColumn: 0,
  scrollToRow: 0,
  className: '',
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
    'align:bottom-right',
  ]),
  rowCount: PropTypes.number.isRequired,
  scrollToColumn: PropTypes.number.isRequired,
  scrollToRow: PropTypes.number.isRequired,
};
