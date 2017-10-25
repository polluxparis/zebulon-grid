import React from "react";
export const configurationFunctions = {
  formats: {
    id: value => (
      <div style={{ color: "blue", textAlign: "right" }}>{value}</div>
    ),
    date: value => {
      if (value instanceof Date) {
        return (
          <div style={{ color: "black", textAlign: "center" }}>
            {value.toString()}
          </div>
        );
      }
      return value;
    },
    quantity: value => {
      if (Number.isFinite(value)) {
        return (
          <div style={{ color: "black", textAlign: "right" }}>
            {Number(value).toFixed(0)}
          </div>
        );
      }
      return value;
    },
    amount: value => {
      if (Number.isFinite(value)) {
        return (
          <div style={{ color: "black", textAlign: "right" }}>
            {Number(value).toFixed(0)}
          </div>
        );
      }
      return value;
    }
  },
  accessors: {},
  sorts: {},
  aggregations: {}
};
