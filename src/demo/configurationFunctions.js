import React from "react";
import { utils } from "zebulon-controls";

export const configurationFunctions = {
  formats: {
    id: ({ value }) => (
      <div style={{ color: "blue", textAlign: "right" }}>{value}</div>
    ),
    date: ({ value }) => {
      if (value instanceof Date) {
        return (
          <div style={{ color: "black", textAlign: "center" }}>
            {value.toString()}
          </div>
        );
      }
      return value;
    },
    quantity: ({ value }) => {
      if (Number.isFinite(value)) {
        return (
          <div style={{ color: "black", textAlign: "right" }}>
            {Number(value).toFixed(0)}
          </div>
        );
      }
      return value;
    },
    amount: ({ value }) => {
      if (Number.isFinite(value)) {
        return (
          <div style={{ color: "black", textAlign: "right" }}>
            {Number(value).toFixed(0)}
          </div>
        );
      }
      return value;
    },
    price: ({ value }) => {
      return (
        <div style={{ display: "flex", justifyContent: "space-between" }}>
          <div>€</div>
          <div style={{ textAlign: "right" }}>
            {utils.formatValue(value, null, 2)}
          </div>
        </div>
      );
    }
  },
  accessors: {
    price: ({ row }) => ({ v0: row.amt, v1: row.qty })
  },
  sorts: {},
  aggregations: {}
};
