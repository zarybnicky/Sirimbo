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
  views = ['month', 'week', 'work_week', 'day', 'agenda'],
}: {
  view: CalendarViewKey;
  setView: (view: CalendarViewKey) => void;
  views?: readonly CalendarViewKey[];
}) {
  const labels: Record<CalendarViewKey, string> = {
    month: 'Měsíc',
    week: 'Týden',
    work_week: 'Pracovní dny',
    day: 'Den',
    agenda: 'Agenda',
    range: 'Celý rozpis',
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        {labels[view]}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {views.map((item) => (
          <DropdownMenuButton key={item} onSelect={() => setView(item)}>
            {labels[item]}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
