import React from "react";
import { shallow } from "enzyme";
import DimensionHeaders from "./DimensionHeaders";

describe("DimensionHeaders", () => {
  test("renders without crashing", () => {
    const props = {
      gridId: 0,
      columnDimensionHeaders: [
        {
          axisType: 1,
          type: 9,
          template: "cell-template-dimensionheader",
          value: {
            id: "titi",
            name: "titi",
            caption: "Titi",
            sort: { order: "asc" },
            subTotal: {}
          },
          expanded: true,
          cssclass: "empty",
          key: "titi"
        }
      ],
      columnDimensions: [
        {
          id: "titi",
          name: "titi",
          caption: "Titi",
          sort: { order: "asc" },
          subTotal: {}
        }
      ],
      dataHeadersLocation: "columns",
      dimensionPositions: {
        columns: { titi: 0, __measures__: 30 },
        rows: { toto: 0, tutu: 100 }
      },
      height: 60,
      previewSizes: { height: 586, width: 1084 },
      rowDimensionHeaders: [
        {
          axisType: 2,
          type: 9,
          template: "cell-template-dimensionheader",
          value: {
            id: "toto",
            name: "toto_lb",
            caption: "Toto",
            sort: { order: "asc" },
            subTotal: {}
          },
          expanded: true,
          cssclass: "empty",
          key: "toto"
        },
        {
          axisType: 2,
          type: 9,
          template: "cell-template-dimensionheader",
          value: {
            id: "tutu",
            name: "tutu",
            caption: "Tutu",
            sort: { order: "asc" },
            subTotal: {}
          },
          expanded: true,
          cssclass: "empty",
          key: "tutu"
        }
      ],
      rowDimensions: [
        {
          id: "toto",
          name: "toto_lb",
          caption: "Toto",
          sort: { order: "asc" },
          subTotal: {}
        },
        {
          id: "tutu",
          name: "tutu",
          caption: "Tutu",
          sort: { order: "asc" },
          subTotal: {}
        }
      ],
      width: 200,
      zoom: 1,
      getDimensionSize: () => 33
    };
    shallow(<DimensionHeaders {...props} />);
  });
});
