import React from 'react';
import { Navigate, View } from 'types';

export type NavigationContext = {
  onDrillDown: (date: Date, view: View) => void;
  onNavigate: (action: Navigate, newDate?: Date) => void;
};

export const NavigationContext = React.createContext<NavigationContext>({
  onDrillDown: () => {},
  onNavigate: () => {},
});
