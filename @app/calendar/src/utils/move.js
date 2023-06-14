import { navigate } from './constants'
import VIEWS from '../Views'

export default function moveDate(View, { action, date, today, ...props }) {
  View = typeof View === 'string' ? VIEWS[View] : View

  switch (action) {
    case navigate.TODAY:
      date = today || new Date()
      break
    case navigate.DATE:
      break
    default:
      date = View.navigate(date, action, props)
  }
  return date
}
