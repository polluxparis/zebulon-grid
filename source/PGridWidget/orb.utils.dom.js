'use strict'

var _scrollBarSize

const inDOM = !!(typeof window !== 'undefined' && window.document && window.document.createElement)
export function scrollbarSize (recalc) {
  console.log(`in scrollbarSize ${_scrollBarSize}`)
  if (!_scrollBarSize || recalc) {
    console.log(`in scrollbarSize ${inDOM}`)
    if (inDOM) {
      console.log(`in scrollbarSize ${inDOM}`)
      var inner = document.createElement('p')
      inner.style.width = '100% !important'
      inner.style.height = '200px !important'

      var outer = document.createElement('div')
      outer.style.position = 'absolute !important'
      outer.style.top = '0px !important'
      outer.style.left = '0px !important'
      outer.style.visibility = 'hidden !important'
      outer.style.width = '200px !important'
      outer.style.height = '150px !important'
      outer.style.overflow = 'hidden !important'
      outer.appendChild(inner)

      document.body.appendChild(outer)
      var w1 = inner.offsetWidth
      outer.style.overflow = 'scroll'
      var w2 = inner.offsetWidth
      if (w1 === w2) w2 = outer.clientWidth

      document.body.removeChild(outer)

      _scrollBarSize = (w1 - w2)
    }
    console.log(`return scrollbarSize: ${_scrollBarSize}`)
    return _scrollBarSize
  }
}

export function removeClass (element, classname) {
  if (element && classname) {
    while (element.className.indexOf(classname) >= 0) {
      element.className = element.className.replace(classname, '')
    }
  }
}

export function addClass (element, classname) {
  if (element && classname) {
    if (element.className.indexOf(classname) < 0) {
      element.className += ' ' + classname
    }
  }
}

export function getOffset (element) {
  if (element) {
    var rect = element.getBoundingClientRect()
    return { x: rect.left, y: rect.top }
  }
  return { x: 0, y: 0 }
}

export function getParentOffset (element) {
  if (element) {
    var rect = element.getBoundingClientRect()
    var rectParent = element.parentNode != null ? element.parentNode.getBoundingClientRect() : { top: 0, left: 0 }
    return { x: rect.left - rectParent.left, y: rect.top - rectParent.top }
  }
  return { x: 0, y: 0 }
}

export function getSize (element) {
  if (element) {
    var rect = element.getBoundingClientRect()
    return { width: rect.right - rect.left, height: rect.bottom - rect.top }
  }
  return { width: 0, height: 0 }
}

var reHyphenToUcase = /\-(\w)/g
function replaceHyphenByUcase (val) {
  return val.replace(reHyphenToUcase, function (m, m1) {
    return m1.toUpperCase()
  })
}

export function getStyle (element, styleProps, keepString) {
  var values = []
  if (element && styleProps) {
    var currStyle, f, fixProp
    if (element.currentStyle) {
      currStyle = element.currentStyle
      f = function (prop) { return currStyle[prop] }
      fixProp = true
    } else if (window && window.getComputedStyle) {
      currStyle = window.getComputedStyle(element, null)
      f = function (prop) { return currStyle.getPropertyValue(prop) }
    }

    for (var i = 0; i < styleProps.length; i++) {
      var val = f(fixProp ? replaceHyphenByUcase(styleProps[i]) : styleProps[i])
      values.push(val && keepString !== true ? Math.ceil(parseFloat(val)) : val)
    }
  }
  return values
}

export function isVisible (element) {
  if (element) {
    return element.style.display !== 'none' && (element.offsetWidth !== 0 || element.offsetHeight !== 0)
  }
  return false
}

export function updateTableColGroup (tableNode, widths) {
  if (tableNode) {
    var colGroupNode = tableNode.firstChild
    if (colGroupNode && colGroupNode.nodeName === 'COLGROUP') {
      tableNode.style.tableLayout = 'auto'
      tableNode.style.width = ''

      while (colGroupNode.firstChild) {
        colGroupNode.removeChild(colGroupNode.firstChild)
      }
      for (var i = 0; i < widths.length; i++) {
        var col = document.createElement('col')
        col.style.width = widths[i] + 'px'
        colGroupNode.appendChild(col)
      }
      tableNode.style.tableLayout = 'fixed'
    }
  }
}
