import { useAtom } from 'jotai';
import { groupByAtom } from '@/calendar/state';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { ChevronDown } from 'lucide-react';
import React from 'react';

export function GroupByPicker() {
  const [groupBy, setGroupBy] = useAtom(groupByAtom);
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        {groupBy === 'room'
          ? 'Seskupit podle místa'
          : groupBy === 'trainer'
            ? 'Seskupit podle trenéra'
            : 'Neseskupovat'}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onSelect={() => setGroupBy('none')}>
          Neseskupovat
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setGroupBy('trainer')}>
          Seskupit podle trenérů
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setGroupBy('room')}>
          Seskupit podle místa
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
