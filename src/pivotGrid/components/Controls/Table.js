import React, { Component } from "react";
import classnames from "classnames";
import { Headers, Status, Filters } from "./TableHeaders";
import { Rows } from "./Rows";
import { Filter } from "../Filter/Filter";
export class Table extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: props.data,
      filteredData: props.data,
      meta: props.meta,
      filters: {},
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
    this.rowHeight = 25;
    // this.meta = this.getMeta(props.meta, props.data);
  }
  componentWillReceiveProps(nextProps) {
    if (
      nextProps.data !== this.props.data ||
      nextProps.meta !== this.props.meta
    ) {
      this.setState({
        data: nextProps.data,
        filteredData: this.filters(nextProps.data, this.state.filters),
        meta: nextProps.meta
      });
    }
  }
  onScroll = scroll => {
    if (this.state.openedFilter) {
      this.setState({ openedFilter: false });
    }
    this.setState({ scroll });
  };
  hasParent(element, id) {
    if (!element.parentElement) {
      return false;
    } else if (element.parentElement.id === id) {
      return true;
    } else {
      return this.hasParent(element.parentElement, id);
    }
  }
  handleNavigationKeys = e => {
    // a voir
    const isFilter = this.hasParent(document.activeElement, "filter");
    if (this.state.openedFilter) {
      if (e.key === "Escape") {
        this.setState({ openedFilter: false });
        return false;
      } else if (e.key === "Tab") {
        return false;
      }
    }
    if (!isFilter && this.rows && this.rows.handleNavigationKeys) {
      if (this.state.openedFilter) {
        this.setState({ openedFilter: false });
      }
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
  filters = (data, filters) => {
    const f = Object.values(filters).filter(filter => filter.v !== null);
    if (!f.length) {
      return data;
    }
    const filter = row => f.reduce((acc, filter) => acc && filter.f(row), true);
    return data.filter(filter);
  };
  openFilter = (e, column) => {
    let filter = this.state.filters[column.id];
    if (!filter) {
      const items = {};
      this.props.data.forEach(row => {
        items[row[column.id]] = { id: row[column.id], label: row[column.id] };
      });
      column.items = Object.values(items);
      filter = column;
    }
    const rect = e.target.getBoundingClientRect();
    const { x, y } = rect;
    filter.top = 2 * this.rowHeight; // pourquoi 2?
    filter.left =
      column.position + this.rowHeight - this.state.scroll.columns.position;
    this.setState({
      openedFilter: column.id,
      filters: { ...this.state.filters, [column.id]: filter }
    });
  };

  onChangeFilter = (e, column, filterTo) => {
    // console.log("filter", e, column);
    if (this.state.openedFilter) {
      this.setState({ openedFilter: false });
    }
    if (e !== column.v) {
      if (column.filterType === "=") {
        column.f = row => row[column.id] === e;
      } else if (column.filterType === ">=") {
        column.f = row => row[column.id] >= e;
      } else if (column.filterType === "between" && filterTo) {
        column.f = row =>
          row[column.id] <= e &&
          (column.v !== null ? row[column.id] >= column.v : true);
      } else if (column.filterType === "between" && !filterTo) {
        column.f = row =>
          row[column.id] >= e &&
          (column.vTo !== null ? row[column.id] <= column.vTo : true);
      } else if (column.filterType === "<=") {
        column.f = row => row[column.id] <= e;
      } else {
        column.f = row =>
          String(row[column.id] || "").startsWith(String(e || ""));
      }
      if (column.filterType === "between" && filterTo) {
        column.vTo = e;
      } else {
        column.v = e;
      }
      const filters = { ...this.state.filters, [column.id]: column };
      const filteredData = this.filters(this.state.data, filters);
      this.sorts(filteredData, this.state.sorts);
      this.setState({
        filters,
        filteredData
      });
    }
  };
  onChangeFilterValues = filter => {
    // console.log("filter", e, column);
    const id = this.state.openedFilter;
    const column = this.state.filters[this.state.openedFilter];
    column.f = row => filter[row[id]] !== undefined;
    column.v = filter;
    const filters = { ...this.state.filters, [column.id]: column };
    const filteredData = this.filters(this.state.data, filters);
    this.sorts(filteredData, this.state.sorts);

    this.setState({
      filters,
      filteredData,
      openedFilter: null
    });
  };

  // ----------------------------------------
  //sorting
  // ----------------------------------------
  sorts = (data, sorting) => {
    const sorts = Object.keys(sorting).filter(
      key => sorting[key] !== undefined
    );
    if (sorts.length) {
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
    }
  };
  onSort = (column, doubleClick) => {
    if (this.state.openedFilter) {
      this.setState({ openedFilter: false });
    }
    // if (this.state.filteredData===this.state.data)
    let sorts = { ...this.state.sorts };
    if (doubleClick) {
      Object.keys(sorts).forEach(
        columnId => (this.props.meta[sorts[columnId].index].sort = undefined)
      );
      sorts = {};
    }
    // console.log("click", column, doubleClick);
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
    if (this.state.openedFilter) {
      this.setState({ openedFilter: false });
    }
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
    if (this.state.openedFilter) {
      this.setState({ openedFilter: false });
    }
    this.setState({ selectedRange: range });
  };
  render() {
    const height = this.props.height,
      width = this.props.width;
    const { visible } = this.props;
    // let filter;

    if (!visible) {
      return null;
    }
    // -----------------------------
    //   action buttons
    // -----------------------------
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
    // -----------------------------------
    // filters
    // -----------------------------------
    // between
    let betweens = null;
    if (
      this.props.meta.findIndex(column => column.filterType === "between") !==
      -1
    ) {
      betweens = (
        <Headers
          type="filter"
          openFilter={this.openFilter}
          onChange={this.onChangeFilter}
          meta={this.state.meta}
          data={this.state.data}
          height={this.rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
          filterTo={true}
        />
      );
    }

    // check list
    const { openedFilter, filters } = this.state;
    if (openedFilter) {
      const { top, left, items, v } = filters[openedFilter];
      this.filter = (
        <Filter
          items={items}
          title={openedFilter}
          filter={v}
          style={{
            position: "absolute",
            border: "solid 0.1em rgba(0, 0, 0, 0.5)",
            backgroundColor: "white",
            top,
            left,
            zIndex: 3,
            opacity: 1
          }}
          onOk={this.onChangeFilterValues}
        />
      );
    } else {
      this.filter = undefined;
    }
    // ----------------------------------
    return (
      <div
        style={{
          width: "max-content",
          height: "fit-content"
        }}
        // onMouseUp={e => console.log("mouseup", e.target)}
        // onMouseleave={e => console.log("mouseleave", e.target)}
      >
        {this.filter}
        <Headers
          type="header"
          onSort={this.onSort}
          meta={this.state.meta}
          data={this.state.data}
          height={this.rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
        />
        <Headers
          type="filter"
          openFilter={this.openFilter}
          onChange={this.onChangeFilter}
          meta={this.state.meta}
          data={this.state.data}
          height={this.rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
        />
        {betweens}
        <div style={{ display: "-webkit-box" }}>
          <Status
            data={this.state.filteredData}
            height={
              height -
              (2 + (betweens !== null)) * this.rowHeight -
              (actions.length ? 30 : 0)
            }
            rowHeight={this.rowHeight}
            scroll={this.state.scroll.rows}
            updatedRows={this.state.updatedRows}
          />
          <Rows
            meta={this.state.meta}
            data={this.state.filteredData}
            height={
              height -
              (2 + (betweens !== null)) * this.rowHeight -
              (actions.length ? 30 : 0)
            }
            width={width - this.rowHeight}
            rowHeight={this.rowHeight}
            scroll={this.state.scroll}
            onScroll={this.onScroll}
            // selectedRange={this.state.selectedRange}
            selectRange={this.selectRange}
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
