import { getFilteredData } from "./data.selector";
import { getMockDatasource } from "../../utils/mock";

describe("filtering data works", () => {
	test("with one static filter", () => {
		const actual = getFilteredData({
			data: getMockDatasource(1, 10, 10),
			filters: { toto: { dimensionId: "toto", staticValue: [1] } }
		});
		expect(actual.length).toEqual(20);
		expect(actual.map(data => data.toto).includes(1)).toBe(true);
		expect(actual.map(data => data.toto).includes(0)).toBe(false);
	});
});
