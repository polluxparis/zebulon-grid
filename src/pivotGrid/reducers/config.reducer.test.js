import reducer from "./config.reducer";
import { SET_CONFIG_PROPERTY } from "../constants";

test("setConfigurationProperty reducer", () => {
  const state = reducer(
    {},
    {
      type: SET_CONFIG_PROPERTY,
      property: "measureHeadersAxis",
      value: "rows"
    }
  );
  expect(state).toEqual({
    measureHeadersAxis: "columns"
  });
  test("with __FOO__ action", () => {
    const bogusState = { foo: "bar" };
    expect(reducer(bogusState, { type: "__FOO__" })).toEqual(bogusState);
  });
});
