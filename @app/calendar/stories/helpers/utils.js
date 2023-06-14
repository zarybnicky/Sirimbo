export function displayObj(obj) {
  return JSON.stringify(obj, null, 2).replace(/"|'/g, '')
}

export const capitalize = (str) => str[0].toUpperCase() + str.substr(1)

export const cleanDocletValue = (str) =>
  str.trim().replace(/^\{/, '').replace(/\}$/, '')

export function getDisplayTypeName(typeName) {
  if (typeName === 'func') {
    return 'function'
  } else if (typeName === 'bool') {
    return 'boolean'
  } else if (typeName === 'object') {
    return 'Object'
  }

  return typeName
}
