/**
 * @fileOverview Pivot Grid columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

 /* global module */

'use strict';

export class Store{
	private states = {};

	set(key, state) {
		this.states[key] = state;
	};

	get(key) {
		return this.states[key];
	};
};