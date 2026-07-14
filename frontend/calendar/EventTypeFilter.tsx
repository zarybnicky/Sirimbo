import { eventTypes, eventTypesFilterAtom } from '@/calendar/state';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { formatEventType } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { useAtom } from 'jotai';
import { CheckCircle2, Circle, Filter } from 'lucide-react';

export function EventTypeFilter() {
  const [selected, setSelected] = useAtom(eventTypesFilterAtom);
  const label =
    selected.length === eventTypes.length
      ? 'Typy'
      : selected.length === 1
        ? formatEventType(selected[0])
        : `Typy (${selected.length})`;

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
        <Filter />
        {label}
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton
          onSelect={(event) => {
            event.preventDefault();
            setSelected([...eventTypes]);
          }}
        >
          {selected.length === eventTypes.length ? <CheckCircle2 /> : <Circle />}
          Všechny typy
        </DropdownMenuButton>
        {eventTypes.map((type) => (
          <DropdownMenuButton
            key={type}
            onSelect={(event) => {
              event.preventDefault();
              setSelected((current) =>
                current.includes(type)
                  ? current.filter((candidate) => candidate !== type)
                  : [...current, type],
              );
            }}
          >
            {selected.includes(type) ? <CheckCircle2 /> : <Circle />}
            {formatEventType(type)}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
