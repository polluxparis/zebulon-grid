import React, { Component } from "react";
import classnames from "classnames";
import { Headers, Status, Filters } from "./TableHeaders";
import { Rows } from "./Rows";
export class Table extends Component {
  constructor(props) {
    super(props);
    this.state = {
      data: props.data,
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
      const { selectedCell, updatedRows, data, meta } = this.state;
      button.action({ selectedCell, updatedRows, data, meta });
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
    let data = [...this.state.data];
    const updatedRows = {
      ...this.state.updatedRows,
      [data.length]: { new_: true }
    };
    data = data
      .slice(0, index)
      .concat({ index_: data.length })
      .concat(data.slice(index));
    this.setState({ data, updatedRows });
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
    const rowIndex = this.state.selectedCell.rowIndex;
    let row = {};
    if (rowIndex !== undefined) {
      row = this.state.data[rowIndex];
    }
    if (
      this.state.selectedCell ||
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

    //    <Filters meta={meta} data={data} scroll={this.state.scroll.column}>
    //      {filters}
    //    </Filters>)
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
          meta={this.state.meta}
          data={this.state.data}
          height={rowHeight}
          width={width}
          scroll={this.state.scroll.columns}
        />
        <div style={{ display: "-webkit-box" }}>
          <Status
            data={this.state.data}
            height={height - rowHeight - (actions.length ? 30 : 0)}
            rowHeight={rowHeight}
            scroll={this.state.scroll.rows}
            updatedRows={this.state.updatedRows}
          />
          <Rows
            meta={this.state.meta}
            data={this.state.data}
            height={height - rowHeight - (actions.length ? 30 : 0)}
            width={width - rowHeight}
            rowHeight={rowHeight}
            scroll={this.state.scroll}
            onScroll={this.onScroll}
            // selectedCell={this.state.selectedCell}
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
