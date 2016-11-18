import React, { Component } from 'react';
import {
  ResponsiveContainer,
  XAxis, YAxis,
  Tooltip, Legend, CartesianGrid,
  BarChart, Bar,
  LineChart, Line,
  AreaChart, Area,
  PieChart, Pie,
} from 'recharts';
import chroma from 'chroma-js';

export default class Chart extends Component {
  render() {
    const { store } = this.props;
    const type = this.props.type || 'bar';
    const dimension = store.getChartAxis().root;
    const measures = store.config.dataFields;
    const colors = chroma.scale('Spectral').colors(store.config.dataFields.length);
    let data = [];
    if (['bar', 'line', 'area'].indexOf(type) > -1) {
      data = Object.keys(dimension.subdimvals)
      .map(key => dimension.subdimvals[key]).map(dimension =>
        measures.reduce((current, measure) =>
        Object.assign(current,
          { [measure.caption]: store.getData(measure.name, dimension, { isRoot: true }) }),
        { name: dimension.value }),
      );
    } else {
      data = measures.reduce((current, measure) => ({ ...current, [measure.caption]: [] }), {});
      Object.keys(dimension.subdimvals).map(key => dimension.subdimvals[key]).forEach(dimension =>
        measures.forEach(measure =>
          data[measure.caption].push({
            name: dimension.value,
            value: store.getData(measure.name, dimension, { isRoot: true }) }),
        ),
      );
    }
    return (
      <ResponsiveContainer>
        <ChartHOC
          type={type}
          data={data}
          margin={{ top: 5, right: 30, left: 30, bottom: 5 }}
        >
          {measures.map((mea, index) => {
            // It's necessary to filter afterwards in order to keep
            // the same couple field-color when changing the number of activated data fields
            if (store.config.activatedDataFields.map(field => field.name).indexOf(mea.name) > -1) {
              switch (type) {
                case 'bar':
                default:
                  return (<Bar
                    key={index}
                    type="monotone"
                    dataKey={mea.caption}
                    fill={colors[index]}
                    stroke={colors[index]}
                    isAnimationActive={false}
                  />);
                case 'area':
                  return (<Area
                    key={index}
                    type="monotone"
                    dataKey={mea.caption}
                    fill={colors[index]}
                    stroke={colors[index]}
                    isAnimationActive={false}
                  />);
                case 'line':
                  return (<Line
                    key={index}
                    type="monotone"
                    dataKey={mea.caption}
                    stroke={colors[index]}
                    isAnimationActive={false}
                  />);
                case 'pie': {
                  const step = Math.floor(100 / measures.length);
                  const margin = 5;
                  const outerRadius = step * (index + 1);
                  const innerRadius = Math.min((step * index) + margin, outerRadius - 1);
                  return (<Pie
                    key={index}
                    data={data[mea.caption]}
                    innerRadius={`${innerRadius}%`}
                    outerRadius={`${outerRadius}%`}
                    fill={colors[index]}
                    isAnimationActive={false}
                  />);
                }

              }
            } else {
              return null;
            }
          })}
        </ChartHOC>
      </ResponsiveContainer>
    );
  }
}

const ChartHOC = (props) => {
  const { type } = props;
  switch (type) {
    case 'pie':
      return (<PieChart {...props}>
        <Tooltip />
        {props.children}
      </PieChart>);
    case 'line':
      return (<LineChart {...props}>
        <XAxis dataKey="name" />
        <YAxis />
        <CartesianGrid strokeDasharray="3 3" />
        <Tooltip />
        <Legend />
        {props.children}
      </LineChart>);
    case 'area':
      return (<AreaChart {...props}>
        <XAxis dataKey="name" />
        <YAxis />
        <CartesianGrid strokeDasharray="3 3" />
        <Tooltip />
        <Legend />
        {props.children}
      </AreaChart>);
    case 'bar':
    default:
      return (<BarChart {...props}>
        <XAxis dataKey="name" />
        <YAxis />
        <CartesianGrid strokeDasharray="3 3" />
        <Tooltip />
        <Legend />
        {props.children}
      </BarChart>);
  }
};
