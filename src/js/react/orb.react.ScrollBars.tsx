import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as utils from '../orb.utils';
import * as domUtils from '../orb.utils.dom';

const scrollBarMixin = (posProp, mousePosProp, sizeProp, offsetCssProp, cssClass) => class extends React.Component<any,any>{

  scrollEvent;
  scrollClient;
  mousePosProp;
  posProp;
  sizeProp;
  offsetCssProp;
  cssClass;

  constructor() {
    super();
    this.scrollEvent = null;
    this.scrollClient = null;
    // initial state, all zero.
    this.state = {
      size: 16,
      mousedown: false,
      thumbOffset: 0
    };
  }
  componentDidMount() {
    this.scrollEvent = new ScrollEvent(this);
  }
  componentDidUpdate() {
    if (!this.state.mousedown) {
      // mouse not down, don't care about mouse up/move events.
      utils.removeEventListener(document, 'mousemove', this.onMouseMove);
      utils.removeEventListener(document, 'mouseup', this.onMouseUp);
    } else if (this.state.mousedown) {
      // mouse down, interested by mouse up/move events.
      utils.addEventListener(document, 'mousemove', this.onMouseMove);
      utils.addEventListener(document, 'mouseup', this.onMouseUp);
    }
  }
  componentWillUnmount() {
    utils.removeEventListener(document, 'mousemove', this.onMouseMove);
    utils.removeEventListener(document, 'mouseup', this.onMouseUp);
  }
  onMouseDown(e) {
    // drag with left mouse button
    if (e.button !== 0) return;

    const thumbElem  = this.refs['scrollThumb'];
    const thumbposInParent = domUtils.getParentOffset(thumbElem);
    const mousePageXY = utils.getMousePageXY(e);

    domUtils.addClass(thumbElem, 'orb-scrollthumb-hover');

    // inform mousedown, save start pos
    this.setState({
      mousedown: true,
      mouseoffset: mousePageXY[this.mousePosProp],
      thumbOffset: thumbposInParent[this.posProp]
    });

    // prevent event bubbling (to prevent text selection while dragging for example)
    utils.stopPropagation(e);
    utils.preventDefault(e);
  }
  onMouseUp() {

    if(this.state.mousedown) {
      const thumbElem  = this.refs['scrollThumb'];
      domUtils.removeClass(thumbElem, 'orb-scrollthumb-hover');
    }

    this.setState({
      mousedown: false
    });
  }
  onMouseMove(e) {

    // if the mouse is not down while moving, return (no drag)
    if (!this.state.mousedown) return;

    utils.stopPropagation(e);
    utils.preventDefault(e);

    const mousePageXY = utils.getMousePageXY(e);
    const amount = mousePageXY[this.mousePosProp] - this.state.mouseoffset;
    this.state.mouseoffset = mousePageXY[this.mousePosProp];

    this.scroll(amount);
  }
  getScrollSize() {
    if(this.scrollClient != null) {
      return domUtils.getSize(this.scrollClient)[this.sizeProp];
    } else {
        return domUtils.getSize(ReactDOM.findDOMNode(this))[this.sizeProp];
    }
  }
  setScrollClient(scrollClient, scrollCallback) {
    this.scrollClient = scrollClient;
    this.scrollEvent.callback = scrollCallback;
  }
  getScrollPercent() {
    const maxOffset = this.getScrollSize() - this.state.size;
    return maxOffset <= 0 ? 0 : this.state.thumbOffset/maxOffset;
  }
  refresh() {
    if(this.scrollClient) {
      const scrolledElement = this.scrollClient.children[0];

      const clientSize = domUtils.getSize(this.scrollClient);
      const elementSize = domUtils.getSize(scrolledElement);

      const scrollBarContainerSize = this.getScrollSize();
      const newSize = clientSize[this.sizeProp] >= elementSize[this.sizeProp] ? 0 : (clientSize[this.sizeProp]/elementSize[this.sizeProp]) * scrollBarContainerSize;

      this.setState(
        {
          containerSize: scrollBarContainerSize,
          size: newSize,
          thumbOffset: Math.min(this.state.thumbOffset, scrollBarContainerSize - newSize)
        },
        this.scrollEvent.raise
      );

    }
  }
  scroll(amount, mode?) {
    if(this.state.size > 0) {
      if(mode == 1) amount *= 8;

      const maxOffset = this.getScrollSize() - this.state.size;
      let newOffset = this.state.thumbOffset + amount;
      if(newOffset < 0) newOffset = 0;
      if(newOffset > maxOffset) newOffset = maxOffset;

      if(this.state.thumbOffset != newOffset) {
        this.setState(
          { thumbOffset: newOffset },
          this.scrollEvent.raise
        );
        return true;
      }
    }
    return false;
  }
  onWheel(e) {
    this.scroll(e.deltaY, e.deltaMode);
    utils.stopPropagation(e);
    utils.preventDefault(e);
  }
  render() {
    const self = this;

    const thumbStyle = {padding: 0};
    thumbStyle[this.sizeProp] = this.state.size;
    thumbStyle[this.offsetCssProp] = this.state.thumbOffset;

    const thisStyle = {};
    thisStyle[this.sizeProp] = this.state.containerSize;

    const thumbClass = `orb-scrollthumb ${this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().scrollBar}`;

    var scrollThumb = this.state.size <= 0 ?
      null :
      <div className={thumbClass} style={thumbStyle}
           ref="scrollThumb"
           onMouseDown={this.onMouseDown}>
      </div>;

    return  <div className={this.cssClass} style={thisStyle} onWheel={this.onWheel}>
        { scrollThumb }
      </div>;
  }
};

class ScrollEvent {
  scrollBarComp;
  callback;
  constructor(scrollBarComp){
    this.scrollBarComp = scrollBarComp;
    this.callback = null;
  }

  raise(){
      if(SVGPathSegCurvetoCubicAbs['callback']) {
        SVGPathSegCurvetoCubicAbs['callback'](this.scrollBarComp.getScrollPercent());
      }
  };


}
//   const oldMixin = {
// }

export const HorizontalScrollBar = scrollBarMixin(
   'x',
   'pageX',
   'width',
   'left',
   'orb-h-scrollbar'
);

export const VerticalScrollBar = scrollBarMixin(
  'y',
  'pageY',
  'height',
  'top',
  'orb-v-scrollbar'
);
