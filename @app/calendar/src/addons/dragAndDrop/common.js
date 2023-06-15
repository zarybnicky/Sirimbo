export function pointInColumn(bounds, point) {
  const { left, right, top } = bounds
  const { x, y } = point
  return x < right + 10 && x > left && y > top
}

export function eventTimes(event, localizer) {
  let start = event.start
  let end = event.end

  const isZeroDuration =
    localizer.eq(start, end, 'minutes') &&
    localizer.diff(start, end, 'minutes') === 0
  // make zero duration midnight events at least one day long
  if (isZeroDuration) end = localizer.add(end, 1, 'day')
  const duration = localizer.diff(start, end, 'milliseconds')
  return { start, end, duration }
}
