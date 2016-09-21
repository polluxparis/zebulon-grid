import React, { Component } from 'react'
import { ResponsiveContainer, BarChart, XAxis, YAxis, Tooltip, Legend, CartesianGrid, Bar } from 'recharts'

export default class Chart extends Component {
  render () {
    let {store, rootDimension, measures} = this.props
    rootDimension = store.rows.root
    measures = store.config.activatedDataFields
    const data = Object.values(rootDimension.subdimvals).map(dimension =>
      measures.reduce((current, measure) =>
        Object.assign(current, {[measure.caption]: store.getData(measure.name, dimension, {isRoot: true})}),
      {name: dimension.value})
    )
    return (
      <div className='container' style={{height: 600}}>
        <ResponsiveContainer>
          <BarChart data={data}
            margin={{top: 5, right: 30, left: 30, bottom: 5}}>
            <XAxis dataKey='name' />
            <YAxis />
            <CartesianGrid strokeDasharray='3 3' />
            <Tooltip />
            <Legend />
            {measures.map(mea => mea.caption).indexOf('Quantity') > -1
              ? <Bar type='monotone' dataKey='Quantity' fill='#8884d8' stroke='#8884d8' isAnimationActive={false} />
              : null}
            {measures.map(mea => mea.caption).indexOf('Amount') > -1
              ? <Bar type='monotone' dataKey='Amount' fill='#82ca9d' stroke='#82ca9d' isAnimationActive={false} />
              : null}
          </BarChart>
        </ResponsiveContainer>
      </div>
    )
  }
}
