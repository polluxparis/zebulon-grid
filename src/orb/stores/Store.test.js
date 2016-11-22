import Store from './Store';
import { AxisType } from '../Axis';
import { getMockDatasource, basicConfig } from '../../utils/mock';

const store = new Store(basicConfig, null, getMockDatasource());

it('filter correctly', () => {
  store.addFilter('toto_lb', AxisType.ROWS, false, '', '', ['toto 1'], false);
  const filteredData = [
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 9', tutu: '1', qty: 191, amt: 191 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 9', tutu: '0', qty: 190, amt: 190 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 8', tutu: '1', qty: 181, amt: 181 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 8', tutu: '0', qty: 180, amt: 180 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 7', tutu: '1', qty: 171, amt: 171 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 7', tutu: '0', qty: 170, amt: 170 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 6', tutu: '1', qty: 161, amt: 161 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 6', tutu: '0', qty: 160, amt: 160 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 5', tutu: '1', qty: 151, amt: 151 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 5', tutu: '0', qty: 150, amt: 150 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 4', tutu: '1', qty: 141, amt: 141 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 4', tutu: '0', qty: 140, amt: 140 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 3', tutu: '1', qty: 131, amt: 131 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 3', tutu: '0', qty: 130, amt: 130 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 2', tutu: '1', qty: 121, amt: 121 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 2', tutu: '0', qty: 120, amt: 120 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 1', tutu: '1', qty: 111, amt: 111 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 1', tutu: '0', qty: 110, amt: 110 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 0', tutu: '1', qty: 101, amt: 101 },
    { toto: '1', toto_lb: 'toto 1', titi: 'titi 0', tutu: '0', qty: 100, amt: 100 },
  ];
  expect(store.filter(store.data)).toEqual(filteredData);
});
