import * as React from 'react';
import {Axe, AxeType} from '../orb.axe';
import * as defaultToolbarConfig from './defaultToolbarConfig';

export default class ToolbarComponent extends React.Component<any,any>{
  _toInit = [];
  componentDidMount() {
    for(let i = 0; i < this._toInit.length; i++){
      const btn = this._toInit[i];
      btn.init(this.props.pivotTableComp, this.refs[btn.ref]);
    }
  }
  componentDidUpdate() {
    for(let i = 0; i < this._toInit.length; i++){
      const btn = this._toInit[i];
      btn.init(this.props.pivotTableComp, this.refs[btn.ref]);
    }
  }
  createCallback(action) {
    if(action != null) {
      const pgridComponent = this.props.pivotTableComp;
      return e => {
        action(pgridComponent, e.target || e.srcElement);
      };
    }
    return null;
  }
  render() {

    const config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    if(config.toolbar && config.toolbar.visible) {

      const configButtons = config.toolbar.buttons ?
        defaultToolbarConfig.buttons.concat(config.toolbar.buttons) :
        defaultToolbarConfig.buttons;

      const buttons = [];
      for(let i = 0; i < configButtons.length; i++) {
        const btnConfig = configButtons[i];
        const refName = `btn${i}`;

        if(btnConfig.type == 'separator') {
          buttons.push(<div key={i} className="orb-tlbr-sep"></div>);
        } else if(btnConfig.type == 'label') {
          buttons.push(<div key={i} className="orb-tlbr-lbl">{btnConfig.text}</div>);
        } else {
          buttons.push(<div key={i} className={'orb-tlbr-btn ' + btnConfig.cssClass} title={btnConfig.tooltip} ref={refName} onClick={ this.createCallback(btnConfig.action) }></div>);
        }
        if(btnConfig.init) {
          this._toInit.push({
            ref: refName,
            init: btnConfig.init
          });
        }
      }

      return <div>
        { buttons }
        </div>;
    }

    return <div></div>;
  }
}
