// /// <reference path="../../../typings/browser/ambient/react-dom/index.d.ts" />
// /// <reference path="../../../typings/browser/ambient/react/index.d.ts" />

import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as utils from '../orb.utils';

function createOverlay() {
  const overlayElement = document.createElement('div');
  overlayElement.className = 'orb-overlay orb-overlay-hidden';
  document.body.appendChild(overlayElement);
  return overlayElement;
}

export class Dialog extends React.Component<any, any>{

    static create() {
        const dialogFactory = React.createFactory(Dialog);
        const overlay = createOverlay();

        return {
          show(props) {
              ReactDOM.render(dialogFactory(props), overlay);
          }
        };
    }
    overlayElement= null;

  constructor(props){
    super(props);
  }
  setOverlayClass(visible) {
    this.overlayElement.className = this.props.theme.getDialogClasses(visible).overlay;
  }

  componentDidMount() {
      this.overlayElement = ReactDOM.findDOMNode(this).parentNode;
    this.setOverlayClass(true);
    utils.addEventListener(this.overlayElement, 'click', this.close);

    const dialogElement = this.overlayElement.children[0];
    const dialogBodyElement = dialogElement.children[0].children[1];

    const screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
    const screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
    let maxHeight = 2*screenHeight/3;
    maxHeight = maxHeight < 301 ? 301 : maxHeight;
    const dWidth = dialogElement.offsetWidth + (dialogElement.offsetHeight > maxHeight ?  11 : 0);
    const dHeight = dialogElement.offsetHeight > maxHeight ? maxHeight : dialogElement.offsetHeight;

    dialogElement.style.top = `${screenHeight > dHeight ? (screenHeight - dHeight) / 2 : 0}px`;
    dialogElement.style.left = `${screenWidth > dWidth ? (screenWidth - dWidth) / 2 : 0}px`;
    dialogElement.style.height = `${dHeight}px`;
    dialogBodyElement.style.width = `${dWidth}px`;
    dialogBodyElement.style.height = `${dHeight - 45}px`;
  }

  close(e) {
    const target = e.target || e.srcElement;
    if(target == this.overlayElement || target.className === 'button-close') {
      utils.removeEventListener(this.overlayElement, 'click', this.close);
      ReactDOM.unmountComponentAtNode(this.overlayElement);
      this.setOverlayClass(false);
    }
  }

  render() {
    if(this.props.comp) {
      const comp = React.createElement(this.props.comp.type, this.props.comp.props);
      const classes = this.props.theme.getDialogClasses();

      return <div className={classes.dialog} style={ this.props.style || {} }>
      <div className={classes.content}>
          <div className={classes.header}><div className="button-close" onClick={ this.close }></div><div className={classes.title}>{ this.props.title }</div></div>
          <div className={classes.body}>
          { comp }
          </div>
          </div>
        </div>;
    }
  }
}
