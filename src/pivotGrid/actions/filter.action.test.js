import { ADD_FILTER, DELETE_FILTER } from "../constants";
import { addFilter, deleteFilter } from "./filter.action";

describe("addFilter creates correct action", () => {
  test("in normal case", () => {
    expect(addFilter("toto", "in", "", [1, 4, 5], false)).toEqual({
      type: ADD_FILTER,
      dimension: "toto",
      filter: {
        dimensionId: "toto",
        operator: "in",
        term: "",
        staticValue: [1, 4, 5],
        excludeStatic: false
      }
    });
  });
});

describe("deleteFilter creates correct action", () => {
  test("in normal case", () => {
    expect(deleteFilter("toto")).toEqual({
      type: DELETE_FILTER,
      dimension: "toto"
    });
  });
});
