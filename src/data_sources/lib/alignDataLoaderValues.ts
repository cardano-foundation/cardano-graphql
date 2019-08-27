// https://github.com/evancorl/dataloader-values/blob/master/src/index.ts
export interface DataLoaderOptions<TKey = any, TValue = any> {
  keys: TKey[];
  values: TValue[];
  getKey: (value: any) => string;
  hasMany?: boolean;
}

export function alignDataLoaderValues<TKey = any, TValue = any> (
  options: DataLoaderOptions<TKey, TValue>
) {
  const {
    keys,
    values,
    getKey,
    hasMany = false
  } = options

  // Build indexed object that maps keys to values
  const indexedValues = values.reduce((acc: any, nextValue: TValue) => {
    const key = getKey(nextValue)

    // For "hasMany" relationships, return an array of values instead of a single value
    const value = hasMany
      ? [
        // Flattened values are grouped into an array by key
        ...acc[key] || [],
        nextValue
      ]
      : nextValue

    return {
      ...acc,
      [key]: value
    }
  }, {})

  // "We must return an Array of values the same length as the Array of keys,
  // and re-order them to ensure each index aligns with the original keys"
  const alignedValues = keys.map((key): TValue | TValue[] => {
    const value = indexedValues[String(key)]

    if (value === undefined) {
      return hasMany ? [] : null // Fill in missing values
    }

    return value
  })

  return alignedValues as TValue[]
}
