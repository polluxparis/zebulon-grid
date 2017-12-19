import React, { Component } from "react";
import { dateToString, stringToDate } from "../../utils/generic";

const formatValue = (value, dataType, format) => {
  let v = value;
  if (dataType === "boolean" && value === "") {
    v = null;
  } else if (dataType === "date" && value !== null) {
    v = dateToString(v, format || "dd/mm/yyyy");
  }
  // console.log(value, v, type);
  return v;
};
export class Input extends Component {
  constructor(props) {
    super(props);
    this.state = {
      value: formatValue(props.value, props.dataType, props.format)
    };
  }
  componentWillReceiveProps(nextProps) {
    this.setState({
      value: formatValue(nextProps.value, nextProps.dataType, nextProps.format)
    });
  }
  value = () => {
    return this.state.value || null;
  };
  validateInput = value => {
    let v = value;
    if (this.props.validateInput) {
      v = this.props.validateInput(value);
    } else if (this.props.dataType === "number") {
      v =
        value === "" ? null : isNaN(Number(value)) ? undefined : Number(value);
    } else if (this.props.dataType === "date") {
      v =
        value === "" || value === value.match(/[0123456789.:/ ]+/g)[0]
          ? value
          : undefined;
    }
    return v;
  };

  handleChange = e => {
    let value = e.target.value;
    let validatedValue = this.validateInput(value);
    if (this.props.dataType === "boolean") {
      if (this.props.inputType === "filter" && this.state.value === false) {
        validatedValue = null;
      } else {
        validatedValue = !this.state.value;
      }
      value = validatedValue;
    }
    if (validatedValue !== undefined) {
      this.setState({ value: value });
      if (this.props.dataType === "date") {
        validatedValue = stringToDate(validatedValue, this.props.format);
        if (validatedValue !== undefined) {
          this.setState({
            value: formatValue(
              validatedValue,
              this.props.dataType,
              this.props.format
            )
          });
        }
      }
      if (this.props.onChange && validatedValue !== undefined) {
        this.props.onChange(validatedValue);
      }
    }
  };
  handleBlur = e => {
    let value = e.target.value;
    if (this.props.dataType === "date") {
      const value = stringToDate(value, this.props.format);
      if (value === undefined) {
        this.setState({ value: null });
      }
    }
    if (this.props.onBlur) {
      this.props.onBlur(value);
    }
  };

  render() {
    let divStyle = {};
    let style = {
      textAlign: this.props.style.textAlign
    };
    if (this.props.inputType == "filter") {
      style.padding = ".4em";
    }
    let type = "text",
      input,
      checkboxLabel;
    if (this.props.dataType === "boolean") {
      type = "checkbox";
      checkboxLabel = (
        <div
          style={{
            ...style,
            width: style.width - 20,
            marginLeft: 0,
            paddingTop: 3
          }}
          onClick={this.handleChange}
        >
          {this.props.label}
        </div>
      );
    }
    if (this.props.select) {
      input = (
        <select
          className="zebulon-input zebulon-grid-select"
          onChange={this.handleChange}
          value={this.state.value}
          style={style}
          autoFocus={true}

          // onEnter={() => console.log("enter")}
        >
          {this.props.select.map((item, index) => (
            <option key={index} value={typeof item === "object" ? 1 : item}>
              {typeof item === "object" ? 1 : item}
            </option>
          ))}
        </select>
      );
    } else {
      input = (
        <input
          type={type}
          id={this.props.id || "input"}
          key={this.props.id}
          className="zebulon-input"
          autoFocus={this.props.inputType !== "filter"}
          style={style}
          value={this.state.value || ""}
          checked={this.state.value || false}
          disabled={!this.props.editable}
          onChange={this.handleChange}
          ref={ref => (this.focused = ref)}
          tabIndex={0}
          onFocus={e => (this.props.openFilter || (() => {}))(e)}
        />
      );
    }
    // style.width = 20;
    // divStyle.display = "flex";
    // if (this.props.inputType === "filter" && this.state.value === null) {
    //   divStyle.backgroundColor = "lightgrey";
    // }
    // }
    // const { onChange } = this.props;
    // console.log("style", this.props.style);
    return (
      <div
        id="div-input"
        key={this.props.id}
        className={this.props.className}
        style={this.props.style}
        onClick={this.props.onClick || (() => {})}
        onDoubleClick={this.props.onDoubleClick || (() => {})}
        // onFocus={() => console.log("focus")}
      >
        {input}
        {checkboxLabel}
      </div>
    );
  }
}

export class InputInterval extends Component {
  constructor(props) {
    super(props);
    const initialValues = props.initialValues || [null, null];
    this.state = { from: initialValues[0], to: initialValues[1] };
  }
  // componentDidMount() {
  //   if (this.props.hasFocus) {
  //     document.getElementById("focused").focus();
  //     console.log("int", document.activeElement);
  //   }
  // }
  getFrom = () => {
    return this.state.from;
  };
  getTo = () => {
    return this.state.to;
  };
  handleChange = (type, value) => {
    this.setState({ [type]: value });
    if (this.props.onChange) {
      this.props.onChange([
        type === "from" ? value : this.getFrom(),
        type === "to" ? value : this.getTo()
      ]);
    }
  };
  handleBlur = (type, value) => {
    if (this.props.onChange) {
      this.props.onChange([this.getFrom(), this.getTo()]);
    }
  };

  render() {
    const { dataType, format, style, hasFocus } = this.props;
    return (
      <div key={this.props.id} style={this.props.style}>
        <div
          style={{
            textAlign: "center",
            fontWeight: "bold",
            paddingBottom: this.props.title ? 10 : 0
          }}
        >
          {this.props.title}
        </div>
        <div>
          <Input
            hasFocus={hasFocus}
            style={style}
            value={this.getFrom()}
            dataType={dataType}
            format={format}
            onChange={value => this.handleChange("from", value)}
            onBlur={value => this.handleBlur("from", value)}
            editable={true}
          />
        </div>
        <div>
          <Input
            hasFocus={false}
            style={style}
            value={this.getTo()}
            dataType={dataType}
            format={format}
            onChange={value => this.handleChange("to", value)}
            onBlur={value => this.handleBlur("to", value)}
            editable={true}
          />
        </div>
      </div>
    );
  }
}
