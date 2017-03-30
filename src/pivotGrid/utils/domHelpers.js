let calculatedScrollBarSize;
let lastDevicePixelRatio;

const inDOM = !!(typeof window !== 'undefined' &&
  window.document &&
  window.document.createElement);
export function scrollbarSize(recalc) {
  // scrollbar size changes when zoom (devicePixelRatio) is modified
  if (
    !calculatedScrollBarSize ||
    recalc ||
    window.devicePixelRatio !== lastDevicePixelRatio
  ) {
    if (inDOM) {
      const inner = document.createElement('p');
      inner.style.width = '100% !important';
      inner.style.height = '200px !important';

      const outer = document.createElement('div');
      outer.style.position = 'absolute !important';
      outer.style.top = '0px !important';
      outer.style.left = '0px !important';
      outer.style.visibility = 'hidden !important';
      outer.style.width = '200px !important';
      outer.style.height = '150px !important';
      outer.style.overflow = 'hidden !important';
      outer.appendChild(inner);

      document.body.appendChild(outer);
      const w1 = inner.offsetWidth;
      outer.style.overflow = 'scroll';
      let w2 = inner.offsetWidth;
      if (w1 === w2) w2 = outer.clientWidth;

      document.body.removeChild(outer);

      calculatedScrollBarSize = w1 - w2;
      lastDevicePixelRatio = window.devicePixelRatio;
    }
  }
  return calculatedScrollBarSize || 0;
}
