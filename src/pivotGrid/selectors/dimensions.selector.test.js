import {
  getActivatedMeasures,
  getAvailableDimensions,
  getRowDimensions,
  getColumnDimensions
} from "./dimensions.selector";

describe("axis dimensions are computed correctly", () => {
  const dimensions = {
    dimension1: { id: "dimension1" },
    dimension2: { id: "dimension2" }
  };
  test("with 1 dimension on each axis", () => {
    const axis = { rows: ["dimension1"], columns: ["dimension2"] };

    const rowDimensions = getRowDimensions({ axis, dimensions });
    expect(rowDimensions.length).toEqual(1);
    expect(rowDimensions[0].id).toEqual("dimension1");

    const columnDimensions = getColumnDimensions({ axis, dimensions });
    expect(columnDimensions.length).toEqual(1);
    expect(columnDimensions[0].id).toEqual("dimension2");
  });

  test("with 2 dimensions on one axis and 0 on the other", () => {
    const axis = { rows: ["dimension1", "dimension2"], columns: [] };

    const rowDimensions = getRowDimensions({ axis, dimensions });
    expect(rowDimensions.length).toEqual(2);

    const columnDimensions = getColumnDimensions({ axis, dimensions });
    expect(columnDimensions.length).toEqual(0);
  });
});

describe("activated measures are computed correctly", () => {
  test("with one activated dimension", () => {
    const measures = {
      measure1: { id: "measure1", activated: true },
      measure2: { id: "measure2", activated: false }
    };
    const actual = getActivatedMeasures({ measures });
    expect(actual.length).toEqual(1);
    expect(actual[0]).toEqual(measures.measure1);
  });
  test("with no activated dimension", () => {
    const measures = {
      measure1: { id: "measure1", activated: false },
      measure2: { id: "measure2", activated: false }
    };
    const actual = getActivatedMeasures({ measures });
    expect(actual.length).toEqual(0);
  });
});

describe("available measures are computed correctly", () => {
  const dimensions = {
    dimension1: { id: "dimension1" },
    dimension2: { id: "dimension2" }
  };
  test("with one available dimension", () => {
    const axis = { dimensions: ["dimension2"] };
    const actual = getAvailableDimensions({ dimensions, axis });
    expect(actual.length).toEqual(1);
    expect(actual[0]).toEqual({ id: "dimension2" });
  });
  test("with no available dimension", () => {
    const axis = { dimensions: [] };
    const actual = getAvailableDimensions({ dimensions, axis });
    expect(actual.length).toEqual(0);
  });
  test("with all available dimension", () => {
    const axis = { dimensions: ["dimension2", "dimension1"] };
    const actual = getAvailableDimensions({ dimensions, axis });
    expect(actual.length).toEqual(2);
    expect(actual).toEqual([{ id: "dimension2" }, { id: "dimension1" }]);
  });
});
