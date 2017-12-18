import React, { Component } from "react";
import classnames from "classnames";
import { Headers, Status, Filters } from "./TableHeaders";
import { Rows } from "./Rows";
export class Table extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: props.data,
      filteredData: props.data,
      meta: props.meta,
      updatedRows: {},
      scroll: {
        rows: { index: 0, direction: 1, startIndex: 0, shift: 0, position: 0 },
        columns: {
          index: 0,
          direction: 1,
          startIndex: 0,
          shift: 0,
          position: 0
        }
      },
      sorts: {},
      filters: {},
      selectedRange: { start: {}, end: {} }
    };
    // this.meta = this.getMeta(props.meta, props.data);
  }
  componentWillReceiveProps(nextProps) {
    if (
      nextProps.data !== this.props.data ||
      nextProps.meta !== this.props.meta
    ) {
      this.setState({
        data: nextProps.data,
        filteredData: this.filters(nextProps.data),
        meta: nextProps.meta
      });
    }
  }
  onScroll = scroll => {
    this.setState({ scroll });
  };
  handleNavigationKeys = e => {
    if (this.rows && this.rows.handleNavigationKeys) {
      return this.rows.handleNavigationKeys(e);
    }
  };
  //-----------------------------------------
  // buttons and actions management
  //-----------------------------------------
  handleAction = button => {
    if (button.action) {
      const { selectedRange, updatedRows, data, meta } = this.state;
      button.action({ selectedRange, updatedRows, data, meta });
    }
  };
  rowCheck = row => true;
  onChange = (e, row, column) => {
    let updatedRow = this.state.updatedRows[row.index_];
    if (!updatedRow) {
      updatedRow = { row: { ...row } }; // a voir pour rollback ->valeur initial
    }
    if (!updatedRow.updated_) {
      updatedRow.updated_ = true;
      this.setState({
        updatedRows: { ...this.state.updatedRows, [row.index_]: updatedRow }
      });
    }
    row[column.id] = e;
  };

  handleDelete = row => {
    let updatedRow = this.state.updatedRows[row.index_];
    if (!updatedRow) {
      updatedRow = { row: { ...row } }; // a voir pour rollback ->valeur initial
    }
    if (updatedRow.new_) {
      row.deletedInsert_ = true;
    } else {
      updatedRow.deleted_ = !updatedRow.deleted_;
    }
    this.setState({
      updatedRows: { ...this.state.updatedRows, [row.index_]: updatedRow }
    });
  };
  handleNew = index => {
    let filteredData = [...this.state.filteredData];
    const updatedRows = {
      ...this.state.updatedRows,
      [this.state.data.length]: { new_: true }
    };
    const row = { index_: this.state.data.length };
    this.state.data.push(row);
    filteredData = filteredData
      .slice(0, index)
      .concat(row)
      .concat(filteredData.slice(index));
    this.setState({ filteredData, updatedRows });
  };
  // ----------------------------------------
  // filtering
  // ----------------------------------------
  filters = data => {
    const filters = Object.values(this.state.filters);
    if (!filters.length) {
      return data;
    }
    const filter = row =>
      filters.reduce((acc, filter) => acc && filter.f(row), true);
    return data.filter(filter);
  };
  onChangeFilter = (e, column) => {
    // console.log("filter", e, column);
    if (e !== column.v) {
      if (column.dataType === "number") {
        column.f = row => row[column.id] >= e;
      } else if (column.dataType === "string") {
        column.f = row => (row[column.id] || "").startsWith(e || "");
      }
      column.v = e;
      this.setState({
        filters: { ...this.state.filters, [column.id]: column },
        filteredData: this.filters(this.state.data)
      });
    }
  };

  // ----------------------------------------
  //sorting
  // ----------------------------------------
  sorts = (data, sorting) => {
    const sorts = Object.keys(sorting).filter(
      key => sorting[key] !== undefined
    );
    const sort = (rowA, rowB) =>
      sorts.reduce(
        (acc, sort) =>
          acc === 0
            ? ((rowA[sort] > rowB[sort]) - (rowB[sort] > rowA[sort])) *
              (sorting[sort].direction === "asc" ? 1 : -1)
            : acc,
        0
      );
    data.sort(sort);
  };
  onSort = (column, double) => {
    // if (this.state.filteredData===this.state.data)
    let sorts = { ...this.state.sorts };
    if (double) {
      Object.keys(sorts).forEach(
        columnId => (this.props.meta[sorts[columnId].index].sort = undefined)
      );
      sorts = {};
    }
    console.log("click", column, double);
    if (column.sort === undefined) {
      column.sort = "asc";
    } else if (column.sort === "asc") {
      column.sort = "desc";
    } else if (column.sort === "desc") {
      column.sort = undefined;
    }
    sorts[column.id] = { index: column.index_, direction: column.sort };
    if (column.sort === undefined) {
      delete sorts[column.id];
    }
    this.sorts(this.state.filteredData, sorts);
    this.setState({ filteredData: this.state.filteredData, sorts });
  };
  // handleSave = () => {
  //   let ok = true;
  //   let rows;
  //   const updatedData = this.props.checkable
  //     ? this.state.checkedData
  //     : this.state.updatedData;
  //   if (this.props.editable) {
  //     rows = Object.keys(updatedData).map(key => {
  //       const updatedRow = updatedData[key];
  //       const row = this.props.metaColumn.reduce((acc, column) => {
  //         acc[column.code] = column.setAccessor(updatedRow.row[column.code]);
  //         return acc;
  //       }, {});
  //       row.index_ = updatedRow.row._index;
  //       row.status_ = updatedRow.deleted
  //         ? "deleted"
  //         : updatedRow.inserted ? "inserted" : "updated";
  //       const checked = this.rowCheck(updatedRow);
  //       if (checked !== true) {
  //         ok = false;
  //       } else {
  //         ok = true;
  //       }
  //       return row;
  //     });
  //   }
  //   if (ok && this.props.save) {
  //     if (this.props.editable) {
  //       this.props.save({ updatedData: rows });
  //       this.setState({ updatedData: {} });
  //     } else {
  //       console.log("handleSave", updatedData);
  //       this.props.save({ checkedData: updatedData });
  //     }
  //   }
  //   return ok;
  // };
  handleClickButton = index => {
    const button = this.props.actions[index];
    const rowIndex = this.state.selectedRange.rowIndex;
    let row = {};
    if (rowIndex !== undefined) {
      row = this.state.data[rowIndex];
    }
    if (
      this.state.selectedRange ||
      !(button.type === "delete" || button.type === "select")
    ) {
      if (button.type === "delete") {
        return this.handleDelete(row);
      } else if (button.type === "new") {
        return this.handleNew(rowIndex || 0);
      } else if (button.type === "save") {
        const ok = this.handleSave();
        return ok;
      } else {
        return this.handleAction(button);
      }
    }
  };
  selectRange = range => {
    this.setState({ selectedRange: range });
  };
  render() {
    const rowHeight = 25,
      height = this.props.height,
      width = this.props.width;
    const { visible } = this.props;

    if (!visible) {
      return null;
    }
    //   constructor(props) {
    const actions = (this.props.actions || []).map((action, index) => {
      if (!this.doubleclickAction && action.type === "select") {
        this.doubleclickAction = action;
      }
      let disable = false;
      if (typeof action.disable === "function") {
        const row =
          this.state.selectedRange.end.rows !== undefined
            ? this.state.data[this.state.selectedRange.end.rows]
            : {};
        const status = this.state.updatedRows[row.index_];
        disable = action.disable(row, status);
      }
      return (
        <button
          key={index}
          disabled={disable}
          style={{
            width: `${98 / this.props.actions.length}%`,
            margin: 2,
            backgroundColor: "lightgrey"
          }}
          onClick={() => this.handleClickButton(index)}
        >
          {action.caption}
        </button>
      );
    });
    return (
      <div
        style={{
          width: "max-content",
          height: "fit-content"
        }}
      >
        <Headers
          type="header"
          onSort={this.onSort}
          meta={this.state.meta}
          data={this.state.data}
          height={rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
        />
        <Headers
          type="filter"
          onChange={this.onChangeFilter}
          meta={this.state.meta}
          data={this.state.data}
          height={rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
        />
        <div style={{ display: "-webkit-box" }}>
          <Status
            data={this.state.filteredData}
            height={height - 2 * rowHeight - (actions.length ? 30 : 0)}
            rowHeight={rowHeight}
            scroll={this.state.scroll.rows}
            updatedRows={this.state.updatedRows}
          />
          <Rows
            meta={this.state.meta}
            data={this.state.filteredData}
            height={height - 2 * rowHeight - (actions.length ? 30 : 0)}
            width={width - rowHeight}
            rowHeight={rowHeight}
            scroll={this.state.scroll}
            onScroll={this.onScroll}
            // selectedRange={this.state.selectedRange}
            selectRange={this.selectCell}
            onChange={this.onChange}
            updatedRows={this.state.updatedRows}
            ref={ref => (this.rows = ref)}
          />
        </div>
        <div style={{ height: actions.length ? 30 : 0 }}>{actions}</div>
      </div>
    );
  }
}
