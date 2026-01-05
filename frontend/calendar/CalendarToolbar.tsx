import { Spinner } from '@/ui/Spinner';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { useTenant } from '@/ui/useTenant';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { useAtom } from 'jotai';
import {
  CheckCircle2Icon,
  ChevronDown,
  CircleIcon,
  FilterIcon,
  MoveLeft,
  MoveRight,
} from 'lucide-react';
import React from 'react';
import { groupByAtom, trainerIdsFilterAtom } from './state';

export type CalendarViewKey = 'month' | 'week' | 'work_week' | 'day' | 'agenda';

type CalendarToolbarProps = {
  label: string;
  view: CalendarViewKey;
  onViewChange: (view: CalendarViewKey) => void;
  onNavigate: (direction: -1 | 1) => void;
  onToday: () => void;
  onlyMine: boolean;
  onToggleOnlyMine: () => void;
  supportsGrouping: boolean;
  isFetching: boolean;
};

export function CalendarToolbar({
  label,
  view,
  onViewChange,
  onNavigate,
  onToday,
  onlyMine,
  onToggleOnlyMine,
  supportsGrouping,
  isFetching,
}: CalendarToolbarProps) {
  return (
    <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse lg:flex-row items-center">
      <div className="flex gap-2 flex-wrap items-start">
        <div className={buttonGroupCls()}>
          <button
            type="button"
            className={buttonCls({ variant: 'outline', className: 'py-0' })}
            onClick={() => onNavigate(-1)}
          >
            <MoveLeft className="!size-6 mx-1" />
          </button>
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={onToday}
          >
            Dnes
          </button>
          <button
            type="button"
            className={buttonCls({ variant: 'outline', className: 'py-0' })}
            onClick={() => onNavigate(1)}
          >
            <MoveRight className="!size-6 mx-1" />
          </button>
        </div>

        <ViewPicker view={view} onViewChange={onViewChange} />

        <button
          type="button"
          className={buttonCls({ variant: onlyMine ? 'primary' : 'outline' })}
          onClick={onToggleOnlyMine}
        >
          Pouze moje
        </button>

        {!onlyMine && supportsGrouping && <GroupByPicker />}

        <TrainerFilter />

        {isFetching && <Spinner />}
      </div>

      <span className="grow px-3 text-right">{label}</span>
    </div>
  );
}

function TrainerFilter() {
  const [trainerIds, setTrainerIds] = useAtom(trainerIdsFilterAtom);
  const { data: tenant } = useTenant();
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        <FilterIcon className="my-0.5" />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {tenant?.tenantTrainersList?.map((x) => (
          <DropdownMenuButton
            key={x.id}
            onSelect={(e) => {
              e.preventDefault();
              const { person } = x;
              if (person)
                setTrainerIds((xs) =>
                  xs.includes(person.id)
                    ? xs.filter((y) => y !== person.id)
                    : [...xs, person.id],
                );
            }}
          >
            {trainerIds.includes(x.person?.id || '') ? (
              <CheckCircle2Icon />
            ) : (
              <CircleIcon />
            )}
            {x.person?.name}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function GroupByPicker() {
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

function ViewPicker({
  view,
  onViewChange,
}: {
  view: CalendarViewKey;
  onViewChange: (view: CalendarViewKey) => void;
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
        <DropdownMenuButton onSelect={() => onViewChange('month')}>
          Měsíc
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => onViewChange('week')}>
          Týden
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => onViewChange('work_week')}>
          Pracovní dny
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => onViewChange('day')}>
          Den
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => onViewChange('agenda')}>
          Agenda
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
