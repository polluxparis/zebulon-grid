import PivotGrid, { PivotGridWithoutDndContext } from './containers/PivotGrid';
import ZebulonGrid from './ZebulonGrid';
import reducer from './reducers';
import * as actions from './actions';
import { setConfig } from './utils/setConfig';

// Import polyfills
import './utils/polyfill';

export default PivotGrid;
export { PivotGridWithoutDndContext, ZebulonGrid, reducer, actions, setConfig };
