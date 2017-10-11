/* eslint-disable import/no-extraneous-dependencies*/
import { Observable } from "rx-lite";
/* eslint-enable */
// import * as 'from' './Format';

export function getMockDatasource(dataRepetition = 1, nToto = 10, nTiti = 10) {
  const nTutu = 2;
  let obj = [];
  const res = [];
  for (let k = 0; k < dataRepetition; k += 1) {
    for (let o = 0; o < nToto; o += 1) {
      for (let i = 0; i < nTiti; i += 1) {
        for (let u = 0; u < nTutu; u += 1) {
          obj = {};
          obj.toto = o;
          obj.toto_lb = `toto ${String(nToto - o)}`;
          obj.toto_0 = `att0 ${String(o)}`;
          obj.toto_1 = `att1 ${String(nToto - o)}`;
          obj.titi = `titi ${String(i)}`;
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
export function getMockDatasource2(dataRepetition = 1, nToto = 10, nTiti = 10) {
  const nTutu = 2;
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
  nTiti = 10
) => {
  const p = new Promise(resolve => setTimeout(resolve, 20)).then(
    () => getMockDatasource(dataRepetition, nToto, nTiti)
    // {
    //   throw new Error('toto');
    // }
  );
  return p;
};
export const basicConfig = {
  measureHeadersAxis: "columns",
  width: 1099,
  height: 601,
  cellHeight: 30,
  cellWidth: 100,

  dimensions: [
    {
      id: "toto",
      caption: "Toto",
      keyAccessor: "toto",
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
      aggregationCaption: "whatever",
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
  columns: [],
  rows: ["toto", "toto att 0", "toto att 1", "tutu", "titi"],
  activeMeasures: ["qty", "amt", "price"],
  collapses: { rows: { 99: true, 98: true }, columns: {} }
};
export const basicConfig2 = {
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
      aggregationCaption: "whatever",
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
  activeMeasures: ["qty", "amt", "price"]
};
