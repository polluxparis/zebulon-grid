import ZebulonGrid from "./ZebulonGrid";
import GridWithoutStore, {
	GridWithoutStoreAndDndContext
} from "./containers/PivotGrid";
import reducer from "./reducers";
import * as actions from "./actions";
import { applyConfigToStore } from "./utils/configuration";

// Import polyfills for IE11 compatibility
import "./utils/polyfill";

export default ZebulonGrid;
export {
	GridWithoutStoreAndDndContext,
	GridWithoutStore,
	reducer,
	actions,
	applyConfigToStore
};
