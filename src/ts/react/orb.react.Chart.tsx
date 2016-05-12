import * as React from 'react';
import * as ReactDOM from 'react-dom';

export default class Chart extends React.Component<any,any>{
  constructor() {
    super();
    this.state = {
      canRender: false
    };
  }
  canRender() {
    return this.state.canRender &&
      typeof this.props.chartMode.type === 'string' &&
      typeof google.visualization[this.props.chartMode.type] === 'function';
  }
  drawChart() {
    if(this.canRender()) {
      const chartData = this.props.pivotTableComp.pgridwidgetstore.pgrid.getChartData();
      const data = new google.visualization.DataTable();

      data.addColumn('string', chartData.hAxisLabel);
      for(let ri=0; ri < chartData.colNames.length; ri++) {
        data.addColumn('number', chartData.colNames[ri]);
      }

      data.addRows(chartData.dataTable);

      const options = {
        title: chartData.title,
        //isStacked: true,
        fontName: this.state.chartStyle.fontFamily,
        fontSize: parseFloat(this.state.chartStyle.fontSize),
        hAxis: {
          title: chartData.hAxisLabel
        },
        vAxis: {
          title: chartData.vAxisLabel
        }
      };

      if(typeof google.visualization[this.props.chartMode.type] === 'function') {
          const chart = new google.visualization[this.props.chartMode.type](ReactDOM.findDOMNode(this));
        chart.draw(data, options);
      }
    }
  }
  componentDidMount() {
    this.drawChart();
  }
  componentDidUpdate() {
    this.drawChart();
  }
  render() {
    if(this.canRender()) {
      return <div className="chart" style={this.state.chartStyle}></div>;
    }
    return null;
  }
}
