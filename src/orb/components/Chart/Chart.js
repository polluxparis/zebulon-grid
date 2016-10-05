import React, {Component} from 'react'
import {
  ResponsiveContainer,
  XAxis, YAxis,
  Tooltip, Legend, CartesianGrid,
  BarChart, Bar,
  LineChart, Line,
  AreaChart, Area
} from 'recharts'
import chroma from 'chroma-js'

export default class Chart extends Component {
  render () {
    let {store, type} = this.props
    type = type || 'bar'
    const dimension = store.getChartAxe().root
    const measures = store.config.dataFields
    const colors = chroma.scale('Spectral').colors(store.config.dataFields.length)
    const data = Object.keys(dimension.subdimvals).map(key => dimension.subdimvals[key]).map(dimension =>
      measures.reduce((current, measure) =>
        Object.assign(current, {[measure.caption]: store.getData(measure.name, dimension, {isRoot: true})}),
      {name: dimension.value})
    )
    return (
      <ResponsiveContainer>
        <ChartHOC
          type={type}
          data={data}
          margin={{top: 5, right: 30, left: 30, bottom: 5}}>
          <XAxis dataKey='name' />
          <YAxis />
          <CartesianGrid strokeDasharray='3 3' />
          <Tooltip />
          <Legend />
          {measures.map((mea, index) => {
            // It's necessary to filter afterwards in order to keep the same couple field-color when changing the number of activated data fields
            if (store.config.activatedDataFields.map(field => field.name).indexOf(mea.name) > -1) {
              switch (type) {
                case 'bar':
                default:
                  return <Bar key={index} type='monotone' dataKey={mea.caption} fill={colors[index]} stroke={colors[index]} />
                case 'area':
                  return <Area key={index} type='monotone' dataKey={mea.caption} fill={colors[index]} stroke={colors[index]} />
                case 'line':
                  return <Line key={index} type='monotone' dataKey={mea.caption} stroke={colors[index]} />
              }
            } else {
              return null
            }
          })}
        </ChartHOC>
      </ResponsiveContainer>
    )
  }
}

class ChartHOC extends Component {
  render () {
    const {type} = this.props
    switch (type) {
      case 'line':
        return <LineChart {...this.props} />
      case 'area':
        return <AreaChart {...this.props} />
      case 'bar':
      default:
        return <BarChart {...this.props} />
    }
  }
}
