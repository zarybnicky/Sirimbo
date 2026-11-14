import { CalendarViewKey } from '@/calendar/CalendarViews';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { ChevronDown } from 'lucide-react';

const labels: Record<CalendarViewKey | 'year', string> = {
  month: 'Měsíc',
  year: 'Rok',
  week: 'Týden',
  work_week: 'Pracovní dny',
  day: 'Den',
  agenda: 'Agenda',
  range: 'Soustředění',
};

export function ViewPicker<View extends keyof typeof labels>({
  view,
  setView,
  views,
}: {
  view: View;
  setView: (view: View) => void;
  views: readonly View[];
}) {
  if (views.length <= 1) return;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ size: 'sm', variant: 'outline' })}>
        {labels[view]}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {views.map((x) => (
          <DropdownMenuButton key={x} onSelect={() => setView(x)}>
            {labels[x]}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
