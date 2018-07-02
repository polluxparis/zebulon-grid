# Zebulon pivot grid
React pivot grid component for large data sets.
Manage measures (aggregated figures) by dimensions (axises) in rows or columns directly on the grid. 
## Available demo at: http://polluxparis.github.io/zebulon-grid/
#### Demo on demand for zebulon-grid connected to big data: thomas.bres@pollux.paris
## Main features
* Pivoting by drag & drop of dimensions.
* Sorting and filtering.
* Grand total and subtotals.
* Expand/collapse of dimensions hierarchy.
* Resizing of columns and rows.
* Drilldown facilities.
* Custom measures, formats, sorts and external functions in the configuration objects.
## Getting started

Install `zebulon-grid` using npm.

```shell
npm install zebulon-grid --save
```

And in your code:
```js
import ZebulonGrid from 'zebulon-grid'
```

## Simple Example

```js
import React, { Component } from "react";
import ReactDOM from "react-dom";
import ZebulonGrid from "zebulon-grid";

const buildData = (n0, n1, n2) => {
  let data = [];
  for (let i0 = 0; i0 < n0; i0++) {
    for (let i1 = 0; i1 < n1; i1++) {
      for (let i2 = 0; i2 < n2; i2++) {
        data.push({
          totoId: i0,
          totoLabel: `toto${i0}`,
          titi: i1,
          tutu: `tutu${i2}`,
          qty: Math.round(50 * Math.random()) + 1,
          amt: Math.round(150 * Math.random()) + 3
        });
      }
    }
  }
  return data;
};
const buildConfiguration = () => ({
  measureHeaders: "columns",
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
  axis:{
      columns: ["tutu"],
      rows: ["toto", "titi"],
      measures: ["qty", "amt"],
      measureHeaders: "columns"
    }
});
class MyPivotGrid extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }
  data = buildData(20, 12, 7);
  configuration = buildConfiguration();
  render() {
    return (
      <ZebulonGrid
        data={this.data}
        configuration={this.configuration}
        sizes={{
          height: 600,
          width: 1000,
          cellWidth: 80,
          cellHeight: 28,
          zoom: 0.9
        }}

      />
    );
  }
}
ReactDOM.render(<MyPivotGrid />, document.getElementById("root"));
```
## React zebulon grid props
| Property | Type | Description |
|:---|:---|:---|
| configuration| `PropTypes.object` | Meta description  of the multidimensional matrix linked to the data set|
| menuFunctions| `PropTypes.object` | Custom functions callable from the data cell area contextual menu. |
| configurationFunctions| `PropTypes.object` | Named custom formats, accessors, sorts and aggregations. |
| data| `PropTypes.arrayOf(PropTypes.object)` | Data set as an array of objects or a promise. |
| pushedData]| `PropTypes.arrayOf(PropTypes.object)` | Data set as an array of objects. |
| sizes | `PropTypes.object)` | sizes of the grid (height, width), zoom, default cell sizes |

## Data set
The data set (data property) can be either an array of similar objects or a promise that will be resolved as an array of objects.
```js
[
    {
      totoId: 25,
      totoLabel: "toto25",
      titi: 3,
      tutu: "tutuA",
      qty: 100,
      amt: 12456
    },
    {
      totoId: 154,
      totoLabel: "toto154",
      titi: 12,
      tutu: "tutuS",
      qty: 22,
      amt: 2539
    },
    ...
  ]
```
Rows can be added dynamically to the data set using the pushedData property.
Changed data cells will be highlighted using the corresponding CSS. 
## Configuration
The configuration property is an object describing the pivot grid objects as measure or dimensions and their links with the data set properties.
### Accessors
Accessors are used to get the appropriate values from the data set.
They can be :
####
* the name of a dataset property.
* A function called with the data row as parameter.
* A name of an accessor specified in the configurationFunctions property. 
####
Accessors can be defined for:
####
* dimension ids.
* dimension labels.
* dimension sorting keys.
* measures values.
### Formats
Formats are functions used to format the displayed values
They can be:
####
* undefined.
* A function called with the value to format.
* A name of a format specified in the configurationFunctions property. 
####
Formats can be defined for
####
* dimension labels.
* measures values. 
####
### Aggregations
Aggregation are functions used the calculate the measures called for each cell with an array of values returned by its accessor for each data rows to be computed (at the intersection of the dimension values in rows and columns).
They can be:
####
* A name of an available precoded aggregation as sum, min, max...
* An aggregation function.
* A name of an aggregation function specified specified in the configurationFunctions property.
####
N.B. Aggregations (as weighted average) may require several data. In this case its accessor must return each needed data.
### Sorts
Sorts are objects thay may contains
####
* A sort key accessor
* A sorting function.
* A direction (ascending or descending).
### Dimensions
Dimensions are the dimensions of the multidimentional matrix managed by the component.
Dimensions have:
####
* An id and a label (caption).
* A keys accessor and a label accessor (by default the key accessor) for each dimension instances.
* A facultative sorting.
* A facultative format.
```js
  dimensions: [
    {
      id: "toto",
      caption: "Toto",
      keyAccessor: "totoId",
      labelAccessor: "totoLabel",
      sort: {
        keyAccessor: "totoId"
      },
      format:x=>x.toUpperCase()
    },
    {
      id: "titi",
      caption: "Titi",
      keyAccessor: "titi",
    },
    ...]
```
### Measures
Measures are the calculations, using aggregation function, of the projections of the matrix on the required dimensions.
Measures have:
####
* An id and a label (caption).
* A value accessor.
* An aggregation function.
* A facultative format.
```js
  measures: [
    {
      valueAccessor: "qty",
      id: "qty",
      caption: "Quantity",
      format: value =>(
          <div style={{ color: "blue", textAlign: "right" }}>
            {Number(value).toFixed(0)}
          </div>
        ),
      aggregation: "sum"
    },
    {
      valueAccessor: "amt",
      id: "amt",
      caption: "Amount",
      aggregation: "sum",
      format: "amount"
    },...]
```
### Axis
Displayed dimensions and measures are assigned (ordered) either in row or in column. All measures must be on the same axis.
```js
{
  axis:{
  columns: ["titi"],
  rows: ["toto"],
  measures: ["qty", "amt"],
  measureHeaders: "columns"}
}
```
### Features
Features provided by the component are available by default on the grid. They can be disabled by changing the values from "enabled" to anything else. 
```js
features = {
  dimensions: "enabled",
  measures: "enabled",
  resize: "enabled",
  expandCollapse: "enabled",
  totals: "enabled",
  filters: "enabled",
  sorting: "enabled",
  configuration: "enabled"
}
```
### Others
Collapses, sizes and filters can be defined in the configuration property 
## Configuration functions
The configurationFunctions property can bu used to define named custom formats, accessors, sorts and aggregations
```js
{
  formats: {amount:...},
  accessors: {...},
  sorts: {...},
  aggregations: {...}
}
```
## Menu functions
Custom functions that can be dynamically added to contectual menus on the data cells area:
### Cell functions
Cell functions are called with a cell description object as parameter:
####
* rows and columns dimensions decription (ids, captions...) corresponding to the cell.
* cell value.
* rows from the dataset to calculate the value of the cell. 
### Range functions
Range functions are called with a range description object as parameter:
####
* Each rows and columns dimensions (ids, captions...) corresponding to the cell* Array of arrays of cell values.
####
Range functions are called with a range description object as parameter:
### Grid functions
Grid functions are called with a configuration object as parameter:
####
* Available dimensions and axis (rows or columns) where they are eventually used.
* Available measures and axis where they are eventually used.
```js
 {
  dataCellFunctions: {
    drilldown: cell => {...},
    otherDrilldown: cell => {...}
  },
  rangeFunctions: { range: range => {...} },
  gridFunctions: {...}
}
```


