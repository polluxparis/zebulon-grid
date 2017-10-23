# Zebulon pivot grid
React pivot grid component for large datasets.
Manage measures (aggregated figures) by dimensions (axises) in rows or columns directly on the grid. 
## available demo at: http://polluxparis.github.io/zebulon-grid/
## Main features
* Pivoting by drag & drop of dimensions.
* Sorting and filtering.
* Expand/collapse of dimensions hierarchy.
* Resizing of columns and rows.
* Drilldown facilities.
* Add custom measures, formats, sorts and external functions in the configuration objects.


## Getting started

Install `zebulon-pivotgrid` using npm.

```shell
npm install zebulon-pivotgrid --save
```

And in your code:
```js
import zebulon-grid from 'zebulon-pivotgrid'
```

## Simple Example

```js
import React, { Component } from "react";
import ZebulonGrid from "zebulon-pivotgrid";

const buildData = (n0, n1, n2) => {
  let data = [];
  for (let k = 0; k < dataRepetition; k += 1) {
    for (let i0 = 0; i0 < n0; i0++) {
      for (let i1 = 0; i1 < n1; i1++) {
        for (let i2 = 0; i2 < n2; i2++) {
          data.push({
            totoId: i0,
            totoLabel: `toto${i0}`,
            titi: i1,
            tutu: `tutu${i2}`,
            qty: 1,
            amt: 1
          });
        }
      }
    }
    return data;
  }
};
const buildConfiguration = () => ({
  measureHeadersAxis: "columns",
  width: 1000,
  height: 800,
  cellHeight: 30,
  cellWidth: 100,
  dimensions: [
    {
      id: "toto",
      caption: "Toto",
      keyAccessor: "totoId",
      labelAccessor: "totoLabel",
      sort: {
        keyAccessor: "totoLabel"
      }
    },
    {
      id: "titi",
      caption: "Titi",
      keyAccessor: "titi"
    },
    {
      id: "tutu",
      caption: "Tutu",
      keyAccessor: "tutu"
    }
  ],
  measures: [
    {
      valueAccessor: "qty",
      id: "qty",
      caption: "Quantity",
      aggregation: "sum"
    },
    {
      valueAccessor: "amt",
      id: "amt",
      caption: "Amount",
      aggregation: "sum"
    }
  ],
  columns: ["tutu"],
  rows: ["toto", "titi"],
  activeMeasures: ["qty", "amt"]
});
class MyPivotGrid extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }
  data = this.buildData(30, 12, 7);
  configuration = this.buildConfiguration();
  render() {
    return (
      <ZebulonGrid
        data={this.data}
        configuration={this.configuration}
        height={600}
        width={1000}
      />
    );
  }
}
```

## React zebulon grid props
| Property | Type | Description |
|:---|:---|:---|
| data | `PropTypes.arrayOf(PropTypes.object)` | Data set as an array of objects. |
| pushedData | `PropTypes.arrayOf(PropTypes.object)` | Data set as an array of objects. |
| menuFunctions | `PropTypes.object` | Items to choose from; can be an array of strings or an array of objects. |
| configuration | `PropTypes.object` | Items to choose from; can be an array of strings or an array of objects. |
| configurationFunctions | `PropTypes.object` | Items to choose from; can be an array of strings or an array of objects. |
| height | `PropTypes.number` | Items to choose from; can be an array of strings or an array of objects. |
| width | `PropTypes.number` | Items to choose from; can be an array of strings or an array of objects. |

## Configuration

## Configuration functions
### Formats
### Accessors
### Aggregations

## Menu functions
Custom functions can be dynamically added to contectual menus on the data cells area:
### Cell functions
### Range functions
### Grid functions

