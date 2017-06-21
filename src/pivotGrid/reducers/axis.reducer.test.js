import reducer from "./axis.reducer";
import { MOVE_DIMENSION } from "../constants";

describe("axis reducer", () => {
  describe("with MOVE_DIMENSION action", () => {
    test("to another axis", () => {
      const state = { rows: ["a"], columns: ["b"] };
      const newState = reducer(state, {
        type: MOVE_DIMENSION,
        id: "a",
        oldAxis: "rows",
        newAxis: "columns",
        position: 0
      });
      expect(newState).toEqual({ rows: [], columns: ["a", "b"] });
    });

    test("to an empty axis", () => {
      const state = { rows: ["a"], columns: [] };
      const newState = reducer(state, {
        type: MOVE_DIMENSION,
        id: "a",
        oldAxis: "rows",
        newAxis: "columns",
        position: 0
      });
      expect(newState).toEqual({ rows: [], columns: ["a"] });
    });

    test("to another axis with correct position", () => {
      const state = { rows: ["a"], columns: ["b"] };
      const newState = reducer(state, {
        type: MOVE_DIMENSION,
        id: "a",
        oldAxis: "rows",
        newAxis: "columns",
        position: 1
      });
      expect(newState).toEqual({ rows: [], columns: ["b", "a"] });
    });

    test("to the same axis", () => {
      const state = { rows: ["a", "c"], columns: ["b"] };
      const newState = reducer(state, {
        type: MOVE_DIMENSION,
        id: "a",
        oldAxis: "rows",
        newAxis: "rows",
        position: 2
      });
      expect(newState).toEqual({ rows: ["c", "a"], columns: ["b"] });
    });

    test("to the same position", () => {
      const state = { rows: ["a", "c"], columns: ["b"] };
      const newState = reducer(state, {
        type: MOVE_DIMENSION,
        id: "a",
        oldAxis: "rows",
        newAxis: "rows",
        position: 0
      });
      expect(newState).toEqual({ rows: ["a", "c"], columns: ["b"] });
    });
  });
  test("with __FOO__ action", () => {
    const bogusState = { foo: "bar" };
    expect(reducer(bogusState, { type: "__FOO__" })).toEqual(bogusState);
  });
});
