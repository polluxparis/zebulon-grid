import React, { Component } from 'react'
import { ResponsiveContainer, BarChart, XAxis, YAxis, Tooltip, Legend, CartesianGrid, Bar } from 'recharts'
import chroma from 'chroma-js'

export default class Chart extends Component {
  render () {
    let {store} = this.props
    const dimension = store.getChartAxe().root
    const measures = store.config.dataFields
    const colors = chroma.scale('Spectral').colors(store.config.dataFields.length)
    const data = Object.values(dimension.subdimvals).map(dimension =>
      measures.reduce((current, measure) =>
        Object.assign(current, {[measure.caption]: store.getData(measure.name, dimension, {isRoot: true})}),
      {name: dimension.value})
    )
    return (
      <ResponsiveContainer>
        <BarChart data={data}
          margin={{top: 5, right: 30, left: 30, bottom: 5}}>
          <XAxis dataKey='name' />
          <YAxis />
          <CartesianGrid strokeDasharray='3 3' />
          <Tooltip />
          <Legend />
          {measures.map((mea, index) => {
            // It's necessary to filter afterwards in order to keep the same couple field-color when changing the number of activated data fields
            if (store.config.activatedDataFields.map(field => field.name).indexOf(mea.name) > -1) {
              return <Bar key={index} type='monotone' dataKey={mea.caption} fill={colors[index]} stroke={colors[index]} />
            }
          })}
        </BarChart>
      </ResponsiveContainer>
    )
  }
}
