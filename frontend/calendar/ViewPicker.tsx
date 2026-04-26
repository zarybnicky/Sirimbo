import { CalendarViewKey } from '@/calendar/CalendarViews';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { ChevronDown } from 'lucide-react';
import React from 'react';

export function ViewPicker({
  view,
  setView,
}: {
  view: string;
  setView: (view: CalendarViewKey) => void;
}) {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        {view === 'month'
          ? 'Měsíc'
          : view === 'day'
            ? 'Den'
            : view === 'week'
              ? 'Týden'
              : view === 'work_week'
                ? 'Pracovní dny'
                : view === 'agenda'
                  ? 'Agenda'
                  : ''}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onSelect={() => setView('month')}>Měsíc</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('week')}>Týden</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('work_week')}>
          Pracovní dny
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('day')}>Den</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('agenda')}>Agenda</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
