'use strict'

/**
 * Creates a namespcae hierarchy if not exisjs
 * @param  {string} identifier - namespace identifier
 * @return {object}
 */
export function ns (identifier, parent) {
  var parjs = identifier.split('.')
  var i = 0
  parent = parent || window
  while (i < parjs.length) {
    parent[parjs[i]] = parent[parjs[i]] || {}
    parent = parent[parjs[i]]
    i++
  }
  return parent
}
/**
 * Returns an array of object own properties
 * @param  {Object} obj
 * @return {Array}
 */
export function ownProperties (obj) {
  var arr = []
  for (let prop in obj) {
    if (obj.hasOwnProperty(prop)) {
      arr.push(prop)
    }
  }
  return arr
}
/**
 * Iterates over the list object and executes the callback on each item
 * if the callback returns a value that can be evaluated ti true,
 * the iteration will stop.
 * @param  {Array} list - the list to iterate over
 * @param  {Function} callback - function to be called on each iteration.
 * It will receive as arguments: current item and current item index.
 * @param {Boolean} forceContinue - Do not stop if the callback return value is true.
 * @return {Array}
 */
export function forEach (list, callback, forceContinue) {
  var ret
  if (list) {
    for (var i = 0, l = list.length; i < l; i++) {
      ret = callback(list[i], i)
      if (ret && forceContinue !== true) {
        break
      }
    }
  }
  return ret
}
/**
 * Returns whether or not obj is a javascript array.
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isArray (obj) {
  return Object.prototype.toString.apply(obj) === '[object Array]'
}
/**
 * Returns whether or not obj is a number
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isNumber (obj) {
  return Object.prototype.toString.apply(obj) === '[object Number]'
}
/**
 * Returns whether or not obj is a Date object.
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isDate (obj) {
  return Object.prototype.toString.apply(obj) === '[object Date]'
}
/**
 * Returns whether or not obj is a string
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isString (obj) {
  return Object.prototype.toString.apply(obj) === '[object String]'
}
/**
 * Returns whether or not obj is a regular expression object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isRegExp (obj) {
  return Object.prototype.toString.apply(obj) === '[object RegExp]'
}
/**
 * Returns whether or not obj is a function object
 * @param  {object}  obj
 * @return {Boolean}
 */
export function isFunction (obj) {
  return Object.prototype.toString.apply(obj) === '[object Function]'
}
/**
 * Escapes all RegExp special characters.
 */
export function escapeRegex (re) {
  return re.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')
}
/**
 * Returns the first element in the array that satisfies the given predicate
 * @param  {Array} array     the array to search
 * @param  {function} predicate Function to apply to each element until it returns true
 * @return {Object}           The first object in the array that satisfies the predicate or undefined.
 */
export function findInArray (array, predicate) {
  if (this.isArray(array) && predicate) {
    for (let i = 0; i < array.length; i++) {
      var item = array[i]
      if (predicate(item)) {
        return item
      }
    }
  }
  return undefined
}
/**
 * Returns a JSON string represenation of an object
 * @param {object} obj
 * @return {string}
 */
export function jsonStringify (obj, censorKeywords) {
  function censor (key, value) {
    return censorKeywords && censorKeywords.indexOf(key) > -1 ? undefined : value
  }
  return JSON.stringify(obj, censor, 2)
}
export function addEventListener (element, eventName, handler) {
  if (element.addEventListener) {
    element.addEventListener(eventName, handler, false)
  } else if (element.attachEvent) {
    element.attachEvent('on' + eventName, handler)
  } else {
    element['on' + eventName] = handler
  }
}
export function removeEventListener (element, eventName, handler) {
  if (element.removeEventListener) {
    element.removeEventListener(eventName, handler, false)
  } else if (element.detachEvent) {
    element.detachEvent('on' + eventName, handler)
  } else {
    element['on' + eventName] = null
  }
}
export function preventDefault (e) {
  e = e || window.event

  if (e.preventDefault) {
    e.preventDefault()
  } else {
    e.returnValue = false
  }
}
export function stopPropagation (e) {
  e = e || window.event

  if (e.stopPropagation) {
    e.stopPropagation()
  } else {
    e.cancelBubble = true
  }
}
export function getEventButton (e) {
  var button = e.button
  if ('which' in e) {
    return button
  }
  // IE 8
  return button === 1 ? 0 // left
    : button === 4 ? 1 // middle
      : 2 // right
}
export function getMousePageXY (e) {
  e = e || window.event

  var pageX = e.pageX
  var pageY = e.pageY
  if (pageX === undefined) {
    pageX = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft
    pageY = e.clientY + document.body.scrollTop + document.documentElement.scrollTop
  }
  return {
    pageX: pageX,
    pageY: pageY
  }
}

// // from: https://github.com/davidchambers/Base64.js
//
// var chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='
//
// function InvalidCharacterError(message) {
//     this.message = message
// }
// InvalidCharacterError.prototype = new Error()
// InvalidCharacterError.prototype.name = 'InvalidCharacterError'
// // encoder
// // [https://gist.github.com/999166] by [https://github.com/nignag]
// export function btoa(input) {
//     if (window && window.btoa){
//       return window.btoa(input)
//     }
//     else {
//             var str = String(input)
//             for (
//                 // initialize result and counter
//                 var block, charCode, idx = 0, map = chars, output = ''
//                 // if the next str index does not exist:
//                 // change the mapping table to "="
//                 // check if d has no fractional digijs
//                 str.charAt(idx | 0) || (map = '=', idx % 1)
//                 // "8 - idx % 1 * 8" generates the sequence 2, 4, 6, 8
//                 output += map.charAt(63 & block >> 8 - idx % 1 * 8)
//             ) {
//                 charCode = str.charCodeAt(idx += 3 / 4)
//                 if (charCode > 0xFF) {
//                     throw new InvalidCharacterError("'btoa' failed: The string to be encoded contains characters oujside of the Latin1 range.")
//                 }
//                 block = block << 8 | charCode
//             }
//             return output
//         }
// }
//
// // decoder
// // [https://gist.github.com/1020396] by [https://github.com/atk]
// export function atob(input){
//  if (window && window.atob){
//    return window.atob(input)
//  }
// else {
//         var str = String(input).replace(/=+$/, '')
//         if (str.length % 4 == 1) {
//             throw new InvalidCharacterError("'atob' failed: The string to be decoded is not correctly encoded.")
//         }
//         for (
//             // initialize result and counters
//             var bc = 0, bs, buffer, idx = 0, output = ''
//             // get next character
//             (buffer = str.charAt(idx++))
//             // character found in table? initialize bit storage and add ijs ascii value
//             ~buffer && (bs = bc % 4 ? bs * 64 + buffer : buffer,
//                 // and if not first of each 4 characters,
//                 // convert the first 8 bijs to one ascii character
//                 bc++ % 4) ? output += String.fromCharCode(255 & bs >> (-2 * bc & 6)) : 0
//         ) {
//             // try to find character in table (0-63, not found => -1)
//             buffer = chars.indexOf(buffer)
//         }
//         return output
//     }
//   }
//

export function arrayIntersect (args) {
  const n = args[0].length
  const m = args[1].length
  var i = 0
  var j = 0
  var res = []
  while (i < n && j < m) {
    if (args[0][i] > args[1][j]) {
      j++
    } else if (args[0][i] < args[1][j]) {
      i++
    } else {
      res.push(args[0][i])
      i++
      j++
    }
  }
  return res
}
