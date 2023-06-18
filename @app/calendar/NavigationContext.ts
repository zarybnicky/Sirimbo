import { endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { View } from './types';

export type NavigationContext = {
  timeslots: number;
  step: number;
  min: Date;
  max: Date;
  focusedTime: Date;
  onDrillDown: (date: Date, view: View) => void;
};

export const NavigationContext = React.createContext<NavigationContext>({
  timeslots: 4,
  step: 15,
  min: startOf(new Date(), 'day'),
  max: endOf(new Date(), 'day'),
  focusedTime: new Date(1972, 0, 1, 16, 0, 0),
  onDrillDown: () => {},
});

export const NavigationProvider = ({ children, setDate, setView }: {
  children: React.ReactNode;
  setDate: React.Dispatch<React.SetStateAction<Date>>;
  setView: React.Dispatch<React.SetStateAction<View>>;
}) => {
  const navigationContext: NavigationContext = React.useMemo<NavigationContext>(() => ({
    timeslots: 4,
    step: 15,
    min: startOf(new Date(), 'day'),
    max: endOf(new Date(), 'day'),
    focusedTime: new Date(1972, 0, 1, 16, 0, 0),
    onDrillDown(date, view) {
      setView(view);
      setDate(date);
    },
  }), []);

  return React.createElement(NavigationContext.Provider, { children, value: navigationContext });
};
