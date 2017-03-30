import ReactDOM from 'react-dom';

/**
 * Helper method for testing components that may use Portal and thus require cleanup.
 * This helper method renders components to a transient node that is destroyed after the test completes.
 * Note that rendering twice within the same test method will update the same element (rather than recreate it).
 */
/* eslint-disable import/prefer-default-export, react/no-render-return-value */
export function render(markup) {
  if (!render.mountNode) {
    render.mountNode = document.createElement('div');

    // Unless we attach the mount-node to body, getBoundingClientRect() won't work
    document.body.appendChild(render.mountNode);

    afterEach(render.unmount);
  }

  return ReactDOM.render(markup, render.mountNode);
}
/* eslint-enable */

/**
 * The render() method auto-unmounts components after each test has completed.
 * Use this method manually to test the componentWillUnmount() lifecycle method.
 */
render.unmount = function() {
  if (render.mountNode) {
    ReactDOM.unmountComponentAtNode(render.mountNode);

    document.body.removeChild(render.mountNode);

    render.mountNode = null;
  }
};
