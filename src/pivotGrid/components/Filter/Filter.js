import React, { Component } from "react";
// import { addFilter, deleteFilter } from "../actions";
// import { List } from "react-virtualized";
import { ScrollableArea } from "../controls/ScrollableArea";
class FilterValues extends ScrollableArea {
  getRatios = () => {
    const { height, rowCount, rowHeight, startIndex } = this.props;
    return {
      vertical: {
        display: height / (rowCount * rowHeight),
        position: startIndex / rowCount
      },
      horizontal: {
        display: 1.2,
        position: 0
      }
    };
  };
  getContent = () => {
    const items = [];
    let i = 0,
      index = this.props.startIndex;
    const {
      filter,
      onChangeCheck,
      rowCount,
      height,
      rowHeight,
      width
    } = this.props;
    while (index < rowCount && i < height / rowHeight) {
      const { id, label } = this.props.items[index];
      items.push(
        <div key={index} style={{ height: rowHeight, width }}>
          <input
            type="checkbox"
            checked={filter[id] !== undefined}
            onChange={() => onChangeCheck(id, index)}
          />
          <span onClick={() => onChangeCheck(id, index)}>{label}</span>
        </div>
      );
      index++;
      i++;
    }
    return items;
  };
  onScroll = e => {
    if (e.type === "scrollbar") {
      const startIndex = Math.round(this.props.rowCount * e.positionRatio);
      this.ratios.vertical.position = startIndex / this.props.rowCount;
      this.props.onScroll(startIndex);
    }
  };
}
export class Filter extends Component {
  constructor(props) {
    super(props);
    this.state = {
      items: [...props.items],
      filter: { ...props.filter },
      rowCount: props.items.length,
      startIndex: 0,
      checkAll: false
    };
  }
  componentWillReceiveProps(nextProps) {
    this.setState({
      items: [...nextProps.items],
      filter: { ...nextProps.filter },
      rowCount: nextProps.items.length
    });
  }
  onScroll = startIndex => {
    this.setState({ startIndex });
  };
  onWheel = e => {
    e.preventDefault();
    const direction = Math.sign(e.deltaY);
    const startIndex =
      direction === 1
        ? Math.min(this.state.startIndex + 1, this.state.rowCount - 1)
        : Math.max(this.state.startIndex - 1, 0);
    this.onScroll(startIndex);
  };
  filterItems = value => {
    const v = value.toLowerCase();
    const items = this.props.items.filter(item => {
      return item.label.toLowerCase().startsWith(v);
    });
    this.setState({
      items,
      rowCount: items.length,
      changed: !this.state.changed
    });
  };
  onChangeCheck = (id, index) => {
    const filter = this.state.filter;
    const checked = filter[id] === undefined;
    if (checked) {
      filter[id] = id;
    } else {
      delete filter[id];
    }
    this.setState({ filter, index, changed: !this.state.changed });
  };
  onChangeCheckAll = () => {
    const filter = this.state.items.reduce(
      (acc, item) => {
        if (this.state.checkAll) {
          delete acc[item.id];
          return acc;
        } else {
          acc[item.id] = item.id;
          return acc;
        }
      },
      { ...this.state.filter }
    );
    this.setState({
      filter,
      checkAll: !this.state.checkAll,
      changed: !this.state.changed
    });
  };
  handleClick = () => {
    const filterKeys = Object.values(this.state.filter);
    const filter =
      filterKeys.length === this.props.items.length || filterKeys.length === 0
        ? null
        : this.state.filter;
    // this.props.filterLeaves(filter);
    this.props.onOk(filter);
  };
  render() {
    const rowHeight = 20;
    return (
      <div
        style={{
          width: "fit-content",
          height: "fit-content"
        }}
        onWheel={this.onWheel}
      >
        <div style={{ textAlign: "center" }}>
          {this.props.title || "Filter"}
        </div>
        <div>
          <input
            type="text"
            style={{
              width: "94%",
              margin: "2%",
              placeholder: "toto",
              textAlign: "left"
            }}
            autoFocus={true}
            value={this.value}
            onChange={e => this.filterItems(e.target.value)}
          />
        </div>
        <div>
          <input
            id="-1"
            value="0"
            type="checkbox"
            checked={this.state.checkAll}
            onChange={this.onChangeCheckAll}
          />
          Select all
          <FilterValues
            width={200}
            height={10 * rowHeight}
            rowCount={this.state.rowCount}
            rowHeight={rowHeight}
            items={this.state.items}
            startIndex={this.state.startIndex}
            onScroll={this.onScroll}
            filter={this.state.filter}
            onChangeCheck={this.onChangeCheck}
            // rowRenderer={this.itemRenderer}
            changed={this.state.changed}
          />
        </div>
        <button
          style={{ width: "96%", margin: "2%" }}
          onClick={this.handleClick}
        >
          Apply filter
        </button>
      </div>
    );
  }
}
