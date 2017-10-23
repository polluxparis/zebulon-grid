import React from 'react';

export const menuFunctions = {
  dataCellFunctions: {
    d1: {
      code: 'd1',
      caption: 'drilldown1',
      function: cell => {
        console.log('drilldown1', cell);
      }
    },
    d2: {
      code: 'd2',
      caption: 'drilldown2',
      function: cell => {
        console.log('drilldown2', cell);
      }
    }
  },
  rangeFunctions: {
    r1: {
      code: 'r1',
      caption: 'range1',
      function: range => {
        console.log('range1', range);
      }
    }
  },
  gridFunctions: {
    qi: {
      code: 'qi',
      type: 'subMenu',
      caption: 'Query Infos',
      function: () => <div>toto</div>
    }
  }
};
