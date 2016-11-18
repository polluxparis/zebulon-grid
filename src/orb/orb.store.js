export class Store {
  constructor() {
    this._states = {};
  }

  set(key, state) {
    this._states[key] = state;
  }

  get(key) {
    return this._states[key];
  }
}
