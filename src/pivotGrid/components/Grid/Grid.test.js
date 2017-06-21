import React from "react";
import { shallow } from "enzyme";
import Grid from "./Grid";

const OriginalGrid = Grid.DecoratedComponent;
describe("Grid", () => {
  test("renders without crashing", () => {
    shallow(
      <OriginalGrid
        connectDropTarget={i => i}
        width={1000}
        layout={{ columnHorizontalCount: 22, rowVerticalCount: 33 }}
        customFunctions={{}}
        drilldown={() => 33}
        columnDimensions={[]}
        columnHeaders={[]}
        dataDimensionsCount={1}
        rowDimensions={[]}
        rowHeaders={[]}
        setSizes={() => {}}
      />
    );
  });
});
