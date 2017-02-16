for comp in $(ls); do
echo "Creating test for $comp"
test="import React from 'react';
import { shallow } from 'enzyme';
import $comp from './$comp';

describe('$comp', () => {
it('renders without crashing', () => {
  shallow(<$comp />);
});
})
"
echo $test > "$comp/$comp.test.js"
done