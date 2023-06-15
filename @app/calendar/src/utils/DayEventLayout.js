/*eslint no-unused-vars: "off"*/

import overlap from './layout-algorithms/overlap'
import noOverlap from './layout-algorithms/no-overlap'

const DefaultAlgorithms = {
  overlap: overlap,
  'no-overlap': noOverlap,
}

export function getStyledEvents({
  events,
  minimumStartDifference,
  slotMetrics,
  dayLayoutAlgorithm,
}) {
  let algorithm = dayLayoutAlgorithm

  if (dayLayoutAlgorithm in DefaultAlgorithms)
    algorithm = DefaultAlgorithms[dayLayoutAlgorithm]

  return algorithm.apply(this, arguments)
}
