import React, {Component} from 'react'
import ReactDOM from 'react-dom'
import PGridWidgetComponent from '../PGridWidget/components/Main'
import styles from '../../styles.css'

function getMockDataSource (dataRepetition, nToto) {
  const nTiti = 100
  const nTutu = 2
  var arr = []
  var res = []
  for (var k = 0; k < dataRepetition; k++) {
    for (var ll = 0; ll < nToto; ll++) {
      for (var l = 0; l < nTiti; l++) {
        for (var j = 0; j < nTutu; j++) {
          arr = []
          arr[0] = 'toto' + String(ll)
          arr[3] = 'titi' + String(l)
          arr[4] = 'tutu' + String(j)
          arr[1] = k + 10 * j + 100 * l * 1000 * ll + 1 // +9999999999.1234567890123456
          arr[2] = k + 10 * j + 100 * l * 1000 * ll + 1 // +9999999999.1234567890123456
          res.push(arr)
        }
      }
    }
  }
  return res
}

class Application extends Component {

  render () {
    const data = getMockDataSource(1, 500)

    const config = {
      dataSource: data,
      canMoveFields: true,
      dataHeadersLocation: 'columns',
      width: 1099,
      height: 601,
      theme: 'green',
      toolbar: {
        visible: true
      },
      grandTotal: {
        rowsvisible: false,
        columnsvisible: false
      },
      subTotal: {
        visible: false,
        collapsed: false,
        collapsible: false
      },
      rowSettings: {
        subTotal: {
          visible: false,
          collapsed: false,
          collapsible: false
        }
      },
      columnSettings: {
        subTotal: {
          visible: false,
          collapsed: false,
          collapsible: false
        }
      },
      fields: [
        {
          name: '1',
          caption: 'Amount',
          aggregateFunc: 'sum'

        // dataSettings: {
        //     aggregateFunc: 'sum',
        //     aggregateFuncName: 'whatever',
        //     formatFunc: function(value) {
        //         return value ? Number(value).toFixed(0) + ' $' : ''
        //     }
        // }
        },
        {
          name: '0',
          caption: 'Toto'
        },
        // {
        //     name: '1',
        //     caption: 'Product',
        // },
        // {
        //     name: '2',
        //     caption: 'Manufacturer',
        //     sort: {
        //         order: 'asc'
        //     },
        //     rowSettings: {
        //         subTotal: {
        //             visible: false,
        //             collapsed: true,
        //             collapsible: true
        //         }
        //     },
        // },
        {
          name: '3',
          caption: 'Titi'
        },
        {
          name: '4',
          caption: 'Tutu'
        },
        // {
        //     name: '4',
        //     caption: 'Category',
        //     sort: {
        //         customfunc: function(a, b) {
        //             if(a.trim() == 'Touch Screen Phones'){
        //              return -1
        //             }
        //             if(a < b) return -1
        //             if(a > b) return 1
        //             return 0
        //         }
        //     }
        // },
        {
          name: '2',
          caption: 'Quantity',
          aggregateFunc: 'sum'
        }
      ],
      columns: ['Titi', 'Tutu'], // , 'Category' ],
      rows: ['Toto'],
      data: ['Quantity', 'Amount']
    /*
    preFilters : {
        'Class': { 'Matches': 'Regular' },
        'Manufacturer': { 'Matches': /^a|^c/ },
        'Category'    : { 'Does Not Match': 'D' },
       // 'Amount'      : { '>':  40 },
     //   'Quantity'    : [4, 8, 12]
    }*/
    }

    return (
      <div>
        <header className={styles.header}>
          <h1 className={styles.container}>Orb Pivot Table</h1>
          <div className={styles.container}>
            Pivot table component on React
          </div>
          <div className={styles.container}>
            Docs and code on <a className={styles.headerLink} href='https://github.com/emilebres/orb/'>GitHub</a>.
          </div>
          <br />
        </header>
        <section className={styles.container}>
          <PGridWidgetComponent config={config} />
        </section>
      </div>
    )
  }
}

ReactDOM.render(
  <Application />,
  document.getElementById('root')
)
