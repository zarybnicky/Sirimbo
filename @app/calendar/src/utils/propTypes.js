import PropTypes from 'prop-types'

export const DayLayoutAlgorithmPropType = PropTypes.oneOfType([
  PropTypes.oneOf(['overlap', 'no-overlap']),
  PropTypes.func,
])
