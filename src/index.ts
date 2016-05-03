import * as React from 'react';
import * as ReactDOM from 'react-dom';
require('expose?Perf!react-addons-perf');
import {PGridWidget} from './js/orb';


function getData(i,n) {
	var res = [];
	for(var k=0; k<10*i;k++){
		res[k]=[];
	}
	for(var k=0; k<i;k++){
		for (var j=0; j<10; j++){
			res[10*k+j][0] = 'toto'+String(k%n);
			res[10*k+j][3] = 'titi'+String(j);
			res[10*k+j][1] = k+j;
			res[10*k+j][2] = 100*k+3*j;
		}
	}
	return res;
    }
console.log(`starting 1`);
const data = getData(1000,300);
console.log(`starting 1`);
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
            visible: true,
            collapsed: true,
            collapsible: true
        },
        rowSettings: {
            subTotal: {
                visible: true,
                collapsed: true,
                collapsible: true
            }
        },
        columnSettings: {
            subTotal: {
                visible: false,
                collapsed: true,
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
        // rows    : [ 'Toto'],//, 'Category' ],
        rows : [ 'Toto','Titi' ],
        data    : [ 'Quantity', 'Amount' ],
        /*preFilters : {
            'Class': { 'Matches': 'Regular' },
            'Manufacturer': { 'Matches': /^a|^c/ },
            'Category'    : { 'Does Not Match': 'D' },
           // 'Amount'      : { '>':  40 },
         //   'Quantity'    : [4, 8, 12]
        }*/
    };
console.log(`starting 3`);
 const elem = document.getElementById('grid');
console.log(`starting 4`);
const myWidget = new PGridWidget(config);
console.log(`starting 5`);
myWidget.render(elem);
console.log(`starting 6`);
