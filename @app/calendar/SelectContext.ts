import React from 'react';
import { Event } from './types';

export interface SlotInfo {
  start: Date;
  end: Date;
  slots: Date[];
  action: 'select' | 'click';
  /** For "TimeGrid" views */
  resourceId?: number | string;
  /** For "select" action */
  bounds?: {
    x: number;
    y: number;
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  /** For "click" actions */
  box?: {
    x: number;
    y: number;
    clientX?: number;
    clientY?: number;
  };
}

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
