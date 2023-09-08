import { endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { View } from './types';

export type NavigationContext = {
  timeslots: number;
  step: number;
  minTime: Date;
  maxTime: Date;
  focusedTime: Date;
  onDrillDown: (date: Date, view: View) => void;
};

export const NavigationContext = React.createContext<NavigationContext>({
  timeslots: 4,
  step: 15,
  minTime: startOf(new Date(), 'day'),
  maxTime: endOf(new Date(), 'day'),
  focusedTime: new Date(1972, 0, 1, 16, 0, 0),
  onDrillDown: () => {/* empty */},
});

export const NavigationProvider = ({ children, setDate, setView }: {
  children: React.ReactNode;
  setDate: React.Dispatch<React.SetStateAction<Date>>;
  setView: (view: View) => void;
}) => {
  const navigationContext: NavigationContext = React.useMemo<NavigationContext>(() => ({
    timeslots: 4,
    step: 15,
    minTime: new Date(1972, 0, 1, 7, 0, 0),
    maxTime: endOf(new Date(), 'day'),
    focusedTime: new Date(1972, 0, 1, 16, 0, 0),
    onDrillDown(date, view) {
      setView(view);
      setDate(date);
    },
  }), [setDate, setView]);

  return React.createElement(NavigationContext.Provider, { value: navigationContext }, children);
};
