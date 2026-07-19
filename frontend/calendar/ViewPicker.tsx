import { CalendarViewKey } from '@/calendar/CalendarViews';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { ChevronDown } from 'lucide-react';

const labels: Record<CalendarViewKey, string> = {
  month: 'Měsíc',
  week: 'Týden',
  work_week: 'Pracovní dny',
  day: 'Den',
  agenda: 'Agenda',
  range: 'Soustředění',
};

export function ViewPicker({
  view,
  setView,
  views = ['month', 'week', 'work_week', 'day', 'agenda'],
}: {
  view: CalendarViewKey;
  setView: (view: CalendarViewKey) => void;
  views?: readonly CalendarViewKey[];
}) {
  if (views.length <= 1) return;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ size: 'sm', variant: 'outline' })}>
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
