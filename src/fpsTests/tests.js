/* eslint-disable */

export default function createScrollingTestCase(component) {
  const scrollDown = true;
  const scrollRight = true;

  return function testCase(completedCallback) {
    if (scrollDown) {
      component.scrollTop = 0;
    } else {
      component.scrollTop = component.scrollHeight;
    }

    if (scrollRight) {
      component.scrollLeft = 0;
    } else {
      component.scrollLeft = component.scrollWidth;
    }

    const maxScrollTop = component.scrollHeight;
    const maxScrollLeft = component.scrollWidth;

    let intervalTop = component.scrollHeight / 100;
    let intervalLeft = component.scrollWidth / 100;
    let scrollTop = component.scrollTop;
    let scrollLeft = component.scrollLeft;

    if (scrollDown && scrollRight) {
      incrementScrollDownRight();
    } else {
      incrementScrollUp();
    }

    function incrementScrollDown() {
      interval *= 1.05;
      scrollTop = Math.min(scrollTop + interval, maxScrollTop);

      component.scrollTop = scrollTop;

      if (scrollTop < maxScrollTop) {
        requestAnimationFrame(incrementScrollDown);
      } else {
        completedCallback();
      }
    }

    function incrementScrollUp() {
      interval *= 1.05;
      scrollTop = Math.max(scrollTop - interval, 0);

      component.scrollTop = scrollTop;

      if (scrollTop > 0) {
        requestAnimationFrame(incrementScrollUp);
      } else {
        completedCallback();
      }
    }

    function incrementScrollRight() {
      interval *= 1.05;
      scrollLeft = Math.min(scrollLeft + interval, maxScrollLeft);

      component.scrollLeft = scrollLeft;

      if (scrollLeft < maxScrollLeft) {
        requestAnimationFrame(incrementScrollRight);
      } else {
        completedCallback();
      }
    }

    function incrementScrollDownRight() {
      intervalTop *= 1.05;
      intervalLeft *= 1.05;
      scrollTop = Math.min(scrollTop + intervalTop, maxScrollTop);
      component.scrollTop = scrollTop;
      scrollLeft = Math.min(scrollLeft + intervalLeft, maxScrollLeft);
      component.scrollLeft = scrollLeft;
      if (scrollTop < maxScrollTop && scrollLeft < maxScrollLeft) {
        requestAnimationFrame(incrementScrollDownRight);
      } else {
        completedCallback();
      }
    }
  };
}
