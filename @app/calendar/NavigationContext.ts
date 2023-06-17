import { endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { Navigate, View } from 'types';

export type NavigationContext = {
  timeslots: number;
  step: number;
  min: Date;
  max: Date;
  focusedTime: Date;
  onDrillDown: (date: Date, view: View) => void;
  onNavigate: (action: Navigate, newDate?: Date) => void;
};

export const NavigationContext = React.createContext<NavigationContext>({
  timeslots: 4,
  step: 15,
  min: startOf(new Date(), 'day'),
  max: endOf(new Date(), 'day'),
  focusedTime: new Date(1972, 0, 1, 16, 0, 0),
  onDrillDown: () => {},
  onNavigate: () => {},
});
