import React from 'react';
import { Event, SlotInfo } from 'types';

export type SelectionContext = {
  selectedIds: number[];
  onSelectEvent: (event?: Event) => void;
  onSelectSlot: (slotInfo?: SlotInfo) => void;
};

export const SelectionContext = React.createContext<SelectionContext>({
  selectedIds: [],
  onSelectEvent: () => {},
  onSelectSlot: () => {},
});
