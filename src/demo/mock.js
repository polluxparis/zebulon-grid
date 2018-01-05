/* eslint-disable import/no-extraneous-dependencies*/
import { Observable } from "rx-lite";
/* eslint-enable */

export function getMockDatasource(
  dataRepetition = 1,
  nToto = 10,
  nTiti = 10,
  nTutu = 2
) {
  // const nTutu = 2;
  let obj = [];
  const res = [];
  for (let k = 0; k < dataRepetition; k += 1) {
    for (let o = 0; o < nToto; o += 1) {
      for (let i = 0; i < nTiti; i += 1) {
        for (let u = 0; u < nTutu; u += 1) {
          obj = {};
          obj.toto = o;
          obj.toto_lb = `toto ${String(o)}`;
          obj.toto_0 = `att0 ${String(o)}`;
          obj.toto_1 = `att1 ${String(nToto - o)}`;
          obj.titi = i;
          obj.titi_lb = `titi ${String(i)}`;
          obj.tutu = String(Math.round((nTutu - 1) * Math.random()));
          obj.qty = Math.round(400 * Math.random()) + 125; // +9999999999.1234567890123456
          obj.amt = Math.round(5000 * Math.random()) + 310; // +9999999999.1234567890123456
          obj.d = new Date(
            2015 + Math.round(4 * Math.random()),
            Math.round(12 * Math.random()),
            Math.round(31 * Math.random())
          );
          res.push(obj);
        }
      }
    }
  }
  //obj.d = new Date(new Date().setDate(20)).toISOString();
  // const res2 = getRandomMockDatasource();
  // const res_2 = join(
  //   res,
  //   ["toto", "titi"],
  //   res2,
  //   ["toto", "titi"],
  //   row => ({ x: row.qty, y: row.amt }),
  //   false
  // );
  return res;
}
// const getObj = (o, i, u) => {
//   let obj = {};
//   obj.toto = o;
//   obj.toto_lb = `toto ${String(o)}`;
//   obj.toto_0 = `att0 ${String(o)}`;
//   obj.toto_1 = `att1 ${String(100 - o)}`;
//   obj.titi = `titi ${String(i)}`;
//   obj.tutu = String(u);
//   obj.qty = 10; // +9999999999.1234567890123456
//   obj.amt = 100; // +9999999999.1234567890123456
//   return obj;
// };
export function getRandomMockDatasource(
  dataPercentage = 10,
  nToto = 10,
  nTiti = 10,
  nTutu = 2
) {
  // return [getObj(1, 1, 0), getObj(1, 100, 1)];
  const ratio = dataPercentage / 100;
  // const nTutu = 2;
  let obj = [];
  const res = [];
  for (let k = 0; k < 1; k += 1) {
    for (let o = 0; o < nToto * ratio; o += 1) {
      for (let i = 0; i < nTiti * ratio; i += 1) {
        for (let u = 0; u < nTutu * ratio; u += 1) {
          const oo = Math.round(Math.random() * nToto * 1.1);
          const ii = Math.round(Math.random() * nTiti * 1);
          obj = {};
          obj.toto = oo;
          obj.toto_lb = `toto ${String(oo)}`;
          obj.toto_0 = `att0 ${String(oo)}`;
          obj.toto_1 = `att1 ${String(nToto - oo)}`;
          obj.titi = ii;
          obj.titi_lb = `titi ${String(ii)}`;
          obj.tutu = String(Math.round((nTutu - 1) * Math.random()));
          obj.qty = 100; // Math.round(400 * Math.random()) + 125; // +9999999999.1234567890123456
          obj.amt = Math.round(5000 * Math.random()) + 310; // +9999999999.1234567890123456
          res.push(obj);
        }
      }
    }
  }
  return res;
}

export const overwritedData = () => {
  const obj = {};
  obj.toto = 1;
  obj.toto_lb = `toto ${String(1)}`;
  obj.toto_0 = `att0 ${String(1)}`;
  obj.toto_1 = `att1 ${String(199)}`;
  obj.titi = 1;
  obj.titi_lb = `titi ${String(1)}`;
  // obj.tutu = "2";
  obj.qty = 100; // Math.round(400 * Math.random()) + 125; // +9999999999.1234567890123456
  // obj.amt = Math.round(5000 * Math.random()) + 310; // +9999999999.1234567890123456
  return [obj];
};
export function getMockDatasource2(
  dataRepetition = 1,
  nToto = 10,
  nTiti = 10,
  nTutu = 2
) {
  // const nTutu = 2;
  let obj = [];
  const res = [];
  for (let k = 0; k < dataRepetition; k += 1) {
    for (let o = 0; o < nToto; o += 1) {
      for (let i = 0; i < nTiti; i += 1) {
        for (let u = 0; u < nTutu; u += 1) {
          obj = {};
          obj.toto2 = o;
          obj.toto_lb = `toto2 ${String(nToto - o)}`;
          obj.toto_0 = `att0 ${String(o)}`;
          obj.toto_1 = `att1 ${String(nToto - o)}`;
          obj.titi = `titi2 ${String(i)}`;
          obj.tutu = String(u);
          obj.qty = u + 10 * i + 100 * o + 1; // +9999999999.1234567890123456
          obj.amt = u + 10 * i + 100 * o + 1000; // +9999999999.1234567890123456
          res.push(obj);
        }
      }
    }
  }
  return res;
}
export function getObservableMockDatasource(interval) {
  const data = [
    getMockDatasource(),
    [
      {
        toto: "0",
        toto_lb: "toto 0",
        qty: 100,
        amt: 100,
        titi: "titi 0",
        tutu: "1"
      },
      {
        toto: "0",
        toto_lb: "toto 0",
        qty: 100,
        amt: 100,
        titi: "titi 0",
        tutu: "0"
      }
    ],
    { toto: "0", toto_lb: "toto 0", qty: 1, amt: 2, titi: "titi 0", tutu: "1" }
  ];
  return Observable.interval(interval || 100)
    .take(3)
    .map(i => data[i]);
}
export function getObservableError() {
  return Observable.throw(new Error("titi"));
}
export const getPromiseMockDatasource = (
  dataRepetition = 1,
  nToto = 10,
  nTiti = 10,
  nTutu = 2
) => {
  const p = new Promise(resolve => setTimeout(resolve, 20)).then(() =>
    getMockDatasource(dataRepetition, nToto, nTiti, nTutu)
  );
  return p;
};
export const basicConfig = callbacks => ({
  measureHeadersAxis: "columns",
  width: 1099,
  height: 601,
  cellHeight: 30,
  cellWidth: 100,
  totalsFirst: true,
  edition: { editable: true },

  dimensions: [
    {
      id: "toto",
      caption: "Toto",
      keyAccessor: "toto",
      labelAccessor: "toto_lb",
      sort: {
        keyAccessor: "toto"
      }
    },
    {
      id: "titi",
      caption: "Titi",
      keyAccessor: "titi",
      labelAccessor: "titi_lb"
    },
    {
      id: "tutu",
      caption: "Tutu",
      keyAccessor: "tutu",
      format: "id"
    },
    {
      id: "toto att 0",
      caption: "toto 0",
      keyAccessor: "toto_0",
      attributeParents: ["toto"]
    },
    {
      id: "toto att 1",
      caption: "toto 1",
      keyAccessor: "toto_1",
      attributeParents: ["toto"],
      format: "id"
    }
  ],
  measures: [
    {
      valueAccessor: "qty",
      id: "qty",
      caption: "Quantity",
      format: "quantity",
      aggregation: "sum"
    },
    {
      valueAccessor: "amt",
      id: "amt",
      caption: "Amount",
      aggregation: "sum",
      format: "amount"
    }
    // ,
    // {
    //   id: "price",
    //   caption: "Price",
    //   aggregation: "weighted_avg",
    //   valueAccessor: "price",
    //   format: "price"
    // }
  ],
  columns: ["titi"],
  rows: ["toto", "toto att 0", "toto att 1", "tutu"],
  activeMeasures: ["qty", "amt"],
  collapses: { rows: { 99: true, 98: true }, columns: {} },
  subtotals: { toto: true, rows__total__: true },
  callbacks
  // features: {}
});
export const basicConfig2 = callbacks => ({
  measureHeadersAxis: "columns",
  width: 1099,
  height: 601,
  cellHeight: 30,
  cellWidth: 100,

  dimensions: [
    {
      id: "toto2",
      caption: "Toto",
      keyAccessor: "toto2",
      labelAccessor: "toto_lb",
      sort: {
        keyAccessor: "toto_lb"
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
      keyAccessor: "tutu",
      format: "id"
    },
    {
      id: "toto att 0",
      caption: "toto 0",
      keyAccessor: "toto_0",
      attributeParents: ["toto2"]
    },
    {
      id: "toto att 1",
      caption: "toto 1",
      keyAccessor: "toto_1",
      attributeParents: ["toto2"],
      format: "id"
    }
  ],
  measures: [
    {
      valueAccessor: "qty",
      id: "qty",
      caption: "Quantity",
      format: "quantity",
      aggregation: "sum"
    },
    {
      valueAccessor: "amt",
      id: "amt",
      caption: "Amount",
      aggregation: "sum",
      format: "amount"
    },
    {
      id: "price",
      caption: "Price",
      aggregation: "weighted_avg",
      valueAccessor: "price",
      format: "price"
    }
  ],
  columns: ["titi"],
  rows: ["toto2", "toto att 0", "toto att 1", "tutu"],
  activeMeasures: ["qty", "amt", "price"],
  callbacks
});
export const getMenu = (id, data) => ({
  id: 1,
  type: "menu",
  caption: "Menu",
  position: "bottom",
  children: [
    {
      id: 2,
      type: "sub-menu",
      caption: "SubMenu1",
      // accelerator: "Ctrl+A",
      opened: false,
      selected: true,
      children: [
        {
          id: 3,
          type: "menu-item",
          caption: "Item1",
          onClick: () => {
            console.log("tutu");
          },
          // accelerator: "Ctrl+C",
          checked: true
        },
        {
          id: 4,
          type: "menu-item",
          caption: "Item2",
          onClick: () => {
            console.log("tata");
          },
          disable: true
        },
        {
          id: 6,
          type: "sub-menu",
          caption: "SubMenu6",
          separation: true,
          children: [
            {
              id: 7,
              type: "menu-item",
              caption: "Item7",
              onClick: () => {
                console.log("7");
              }
            },
            {
              id: 8,
              type: "menu-item",
              caption: "Item8",
              onClick: () => {
                console.log("8");
              }
            }
          ]
        }
      ]
    },
    {
      id: 9,
      type: "sub-menu",
      caption: "SubMenu2",
      checked: true,
      children: [
        {
          id: 10,
          type: "menu-item",
          caption: "Item10",
          onClick: () => {
            console.log("10");
          }
        },
        {
          id: 11,
          type: "menu-item",
          caption: "Item11",
          onClick: () => {
            console.log("11");
          }
        }
      ]
    },
    {
      id: 12,
      type: "sub-menu",
      caption: "SubMenu3",
      checked: true,
      onClick: () => {
        console.log("toto");
      },
      disabled: false
    }
  ]
});
// const getKeys1 = (table, keys) =>
//   table.map(row => JSON.stringify(keys.map(key => row[key])));

// const getKeys2 = (table, keys) =>
//   table.reduce((acc, row, index) => {
//     const key = JSON.stringify(keys.map(key => row[key]));
//     const indexes = acc[key];
//     if (!indexes) {
//       acc[key] = [index];
//     } else {
//       indexes.push(index);
//     }
//     return acc;
//   }, {});
// const join = (table1, keys1, table2, keys2, columns2, external) => {
//   const k1 = getKeys1(table1, keys1);
//   const k2 = getKeys2(table2, keys2);
//   const resultTable = [];
//   k1.forEach((key, index1) => {
//     const row1 = table1[index1];
//     const indexes = k2[key];
//     if (!indexes) {
//       if (external) {
//         resultTable.push({ ...row1 });
//       }
//     } else {
//       indexes.forEach(index2 => {
//         const row2 = table2[index2];
//         columns2.forEach(column => (row1[column] = row2[column]));
//         //  resultTable.push({ ...row1, ...columns2(row2) });
//         resultTable.push(row1);
//       });
//     }
//   });
//   return resultTable;
// };
/*

// const keys2 = getKeys2(table2, keys2);
config:"{\"dimensions\": [{\"id\": \"ptf\",\"name\": \"Portfolio\",\"primary_key\": \"ptf\",\"label_key\": \"ptf_lb\",\"order_key\":\"ptf_lb\"},"
config,:"{\"id\": \"thp\",\"name\": \"Thirdparty\",\"primary_key\": \"thp\",\"label_key\": \"thp_lb\",\"order_key\": \"thp_lb\"}],";
config,:"\"measures\": [{\"id\": \"x\",\"code\": \"x\",\"name\": \"X\",\"format\": \"amount\",\"operation\":\"sum\"}],";
config,:"  \"columns\": [\"thp\"],\"rows\": [\"ptf\"], \"activeMeasures\": [\"x\"]}";
 */
/*
const a = {
  dimensions: [
    {
      id: "ptf",
      name: "Portfolio",
      primary_key: "ptf",
      label_key: "ptf_lb",
      order_key: "ptf_lb"
    },
    {
      id: "thp",
      name: "Thirdparty",
      primary_key: "thp",
      label_key: "thp_lb",
      order_key: "thp_lb"
    }
  ],
  measures: [
    { id: "x", code: "x", name: "X", format: "amount", operation: "sum" }
  ],
  columns: ["thp"],
  rows: ["ptf"],
  activeMeasures: ["x"]
};
*/
