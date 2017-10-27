import {
  SET_DIMENSIONS,
  SET_MEASURES,
  SET_CONFIG_PROPERTY,
  TOGGLE_MEASURE,
  MOVE_DIMENSION
} from "../constants";
import {
  setConfigurationProperty,
  toggleMeasure,
  moveDimension,
  setDimensions,
  setMeasures
} from "./configuration.action";

describe("setConfigurationProperty creates an action to set a config property", () => {
  test("when a property  value is given", () => {
    const configObject = { toto: 33 };
    expect(setConfigurationProperty(configObject, "toto", 666)).toEqual({
      type: SET_CONFIG_PROPERTY,
      property: "toto",
      value: 33
    });
  });
  test("when no property  value is given", () => {
    const configObject = {};
    expect(setConfigurationProperty(configObject, "toto", 666)).toEqual({
      type: SET_CONFIG_PROPERTY,
      property: "toto",
      value: 666
    });
  });
});

describe("toggleMeasure create correct action", () => {
  test("in normal case", () => {
    expect(toggleMeasure("toto")).toEqual({
      type: TOGGLE_MEASURE,
      id: "toto"
    });
  });
});

describe("moveDimension creates correct action", () => {
  test("in normal case", () => {
    expect(moveDimension("toto", "dimensions", "rows", 0)).toEqual({
      type: MOVE_DIMENSION,
      id: "toto",
      oldAxis: "dimensions",
      newAxis: "rows",
      position: 0
    });
  });
});

describe("setDimensions creates correct action", () => {
  test("with three dimensions", () => {
    const dimensions = [
      {
        name: "toto_lb",
        accessor: "toto",
        caption: "Toto",
        sort: {
          order: "asc"
        }
      },
      {
        accessor: "titi",
        caption: "Titi"
      },
      {
        accessor: "tutu",
        caption: "Tutu"
      }
    ];
    expect(setDimensions({ dimensions })).toMatchObject({
      type: SET_DIMENSIONS,
      dimensions: [
        {
          id: "toto",
          name: "toto_lb",
          caption: "Toto",
          sort: {
            order: "asc"
          },
          subTotal: {}
        },
        {
          id: "titi",
          name: "titi",
          caption: "Titi",
          sort: {
            order: null
          },
          subTotal: {}
        },
        {
          id: "tutu",
          name: "tutu",
          caption: "Tutu",
          sort: {
            order: null
          },
          subTotal: {}
        }
      ]
    });
  });
});

describe("setMeasures creates correct action", () => {
  test("with two measures", () => {
    const measures = [
      {
        accessor: "qty",
        caption: "Quantity",
        aggregation: "sum"
      },
      {
        accessor: "amt",
        caption: "Amount",
        aggregation: "sum",
        aggregationName: "whatever",
        render: value => {
          if (value || value === 0) {
            return `${Number(value).toFixed(0)} $`;
          }
          return "";
        }
      }
    ];
    expect(setMeasures({ measures })).toMatchObject({
      type: SET_MEASURES,
      measures: [
        {
          id: "qty",
          name: "qty",
          caption: "Quantity",
          aggregationName: "sum"
        },
        {
          id: "amt",
          name: "amt",
          caption: "Amount",
          aggregationName: "whatever"
        }
      ]
    });
  });
});
