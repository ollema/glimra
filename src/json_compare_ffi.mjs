// FFI for JSON operations to avoid stack overflow in Gleam
// Uses iterative approach with explicit stack for deep structures

/**
 * Convert a value to a canonical form for comparison
 * - Objects have keys sorted alphabetically
 * - Uses iterative approach to avoid stack overflow
 */
function toCanonical(value) {
  // Use iterative approach with explicit stack
  const stack = [{ value, path: [], result: null }];
  const results = new Map();

  while (stack.length > 0) {
    const item = stack[stack.length - 1];
    const { value: val, path } = item;
    const pathKey = path.join('.');

    if (val === null || val === undefined) {
      item.result = null;
      results.set(pathKey, null);
      stack.pop();
    } else if (Array.isArray(val)) {
      // Check if all children are processed
      const childResults = [];
      let allProcessed = true;
      for (let i = 0; i < val.length; i++) {
        const childPath = [...path, `[${i}]`].join('.');
        if (results.has(childPath)) {
          childResults.push(results.get(childPath));
        } else {
          allProcessed = false;
          stack.push({ value: val[i], path: [...path, `[${i}]`], result: null });
        }
      }
      if (allProcessed) {
        item.result = childResults;
        results.set(pathKey, childResults);
        stack.pop();
      }
    } else if (typeof val === 'object') {
      const keys = Object.keys(val).sort();
      const childResults = {};
      let allProcessed = true;
      for (const key of keys) {
        const childPath = [...path, key].join('.');
        if (results.has(childPath)) {
          childResults[key] = results.get(childPath);
        } else {
          allProcessed = false;
          stack.push({ value: val[key], path: [...path, key], result: null });
        }
      }
      if (allProcessed) {
        item.result = childResults;
        results.set(pathKey, childResults);
        stack.pop();
      }
    } else {
      item.result = val;
      results.set(pathKey, val);
      stack.pop();
    }
  }

  return results.get('') ?? value;
}

/**
 * Compare two JSON strings semantically
 * @param {string} a - First JSON string
 * @param {string} b - Second JSON string
 * @returns {boolean} - True if semantically equal
 */
export function compare_json(a, b) {
  try {
    const objA = JSON.parse(a);
    const objB = JSON.parse(b);
    const canonicalA = JSON.stringify(toCanonical(objA));
    const canonicalB = JSON.stringify(toCanonical(objB));
    return canonicalA === canonicalB;
  } catch (e) {
    return false;
  }
}

/**
 * Convert a JavaScript value to a JSON string
 * Used to convert decoded dynamic values back to JSON
 * @param {any} value - JavaScript value (from Gleam dynamic)
 * @returns {string} - JSON string
 */
export function stringify_dynamic(value) {
  try {
    return JSON.stringify(value);
  } catch (e) {
    return '{}';
  }
}
