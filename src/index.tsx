import * as React from 'react';
import * as ReactDOM from 'react-dom';
require('expose?Perf!react-addons-perf');
import {PGridWidgetStore} from './js/orb.ui.pgridwidgetstore';
import PivotTableComponent from './js/react/orb.react.PivotTable';


function getData(data_repetition,n_toto) {
	const n_titi = 3;
	const n_tutu = 2;
	var arr = [];
	var res = [];
	for(var k=0; k<data_repetition;k++){
		for (var ll=0; ll<n_toto; ll++){
			for (var l=0; l<n_titi; l++){
				for (var j=0; j<n_tutu; j++){
					arr = []
					arr[0] = 'toto'+String(ll);
					arr[3] = 'titi'+String(l);
					arr[4] = 'tutu'+String(j);
					arr[1] = k+10*j+100*l*1000*ll;
					arr[2] = k+10*j+100*l*1000*ll;
					res.push(arr);
				}
		}
	}
	}
	return res;
    }
const data = getData(10,3);
 var config = {
        dataSource: data,
        canMoveFields: true,
        dataHeadersLocation: 'columns',
        width: 1099,
        height: 611,
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
                visible: true,
                collapsed: false,
                collapsible: true
            }
        },
        columnSettings: {
            subTotal: {
                visible: true,
                collapsed: false,
                collapsible: true
            }
        },
        fields: [
            {
                name: '1',
                caption: 'Amount',
                dataSettings: {
                    aggregateFunc: 'sum',
                    aggregateFuncName: 'whatever',
                    formatFunc: function(value) {
                        return value ? Number(value).toFixed(0) + ' $' : '';
                    }
                }
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
            //              return -1;
            //             }
            //             if(a < b) return -1;
            //             if(a > b) return 1;
            //             return 0;
            //         }
            //     }
            // },
            {
                name: '2',
                caption: 'Quantity',
                aggregateFunc: 'sum'
            }
        ],
        rows    : [ 'Tutu'],//, 'Category' ],
        columns : [ 'Toto', 'Titi' ],
        data    : [ 'Quantity', 'Amount' ],
        /*preFilters : {
            'Class': { 'Matches': 'Regular' },
            'Manufacturer': { 'Matches': /^a|^c/ },
            'Category'    : { 'Does Not Match': 'D' },
           // 'Amount'      : { '>':  40 },
         //   'Quantity'    : [4, 8, 12]
        }*/
    };
const appStore = new PGridWidgetStore(config);
ReactDOM.render(<PivotTableComponent pgridwidgetstore={appStore}/>, document.getElementById('grid'))
