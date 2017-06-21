import reducer from "./measures.reducer";
import { SET_MEASURES, TOGGLE_MEASURE } from "../constants";

describe("measure reducer", () => {
  test("with setMeasure action", () => {
    const state = reducer(
      {},
      {
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
      }
    );
    expect(state).toEqual({
      qty: {
        id: "qty",
        name: "qty",
        caption: "Quantity",
        aggregationName: "sum"
      },
      amt: {
        id: "amt",
        name: "amt",
        caption: "Amount",
        aggregationName: "whatever"
      }
    });
  });
  test("with toggleMeasure action", () => {
    const state = reducer(
      {
        qty: {
          id: "qty",
          name: "qty",
          caption: "Quantity",
          aggregationName: "sum"
        },
        amt: {
          id: "amt",
          name: "amt",
          caption: "Amount",
          aggregationName: "whatever"
        }
      },
      { type: TOGGLE_MEASURE, id: "qty" }
    );
    expect(state).toEqual({
      qty: {
        id: "qty",
        name: "qty",
        caption: "Quantity",
        aggregationName: "sum",
        activated: true
      },
      amt: {
        id: "amt",
        name: "amt",
        caption: "Amount",
        aggregationName: "whatever"
      }
    });
  });
  test("with __FOO__ action", () => {
    const bogusState = { foo: "bar" };
    expect(reducer(bogusState, { type: "__FOO__" })).toEqual(bogusState);
  });
});
