import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQueryVariables,
  type EventInstanceRangeQuery,
  MoveEventInstanceDocument,
} from '@/graphql/Event';
import { cn } from '@/ui/cn';
import { Dialog, DialogContent } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { datetimeRangeToTimeRange, fullDateFormatter } from '@/ui/format';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { add, endOf, startOf } from 'date-arithmetic';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import {
  CheckCircle2Icon,
  ChevronDown,
  CircleIcon,
  FilterIcon,
  MoveLeft,
  MoveRight,
} from 'lucide-react';
import React from 'react';
import { useClient, useMutation, useQuery } from 'urql';
import { BooleanParam, StringParam, useQueryParam, withDefault } from 'use-query-params';
import TimeGrid from './TimeGrid';
import { format, range } from './localizer';
import {
  dragListenersAtom,
  groupByAtom,
  isDraggingAtom,
  trainerIdsFilterAtom,
} from './state';
import type {
  CalendarEvent,
  InteractionInfo,
  Resource,
  SlotInfo,
  ViewProps,
} from './types';
import Agenda from './views/Agenda';
import Month from './views/Month';
import { Spinner } from '@/ui/Spinner';
import { useTenant } from '@/ui/useTenant';
import { EventFormType } from '@/ui/event-form/types';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import { tenantConfigAtom } from '@/ui/state/auth';

type View = {
  component: React.ComponentType<ViewProps>;
  range: (d: Date) => Date[];
  nav: (d: Date, dir: -1 | 1) => Date;
  label: (d: Date) => string;
  supportsGrouping: boolean;
};

const Views = {
  month: {
    component: Month,
    range: (d: Date) => {
      const first = startOf(startOf(d, 'month'), 'week', 1);
      const last = endOf(endOf(d, 'month'), 'week', 1);
      return range(first, last, 'day');
    },
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'month'),
    label: (d: Date) => format(d, 'MMMM yyyy'),
    supportsGrouping: false,
  },
  week: {
    component: TimeGrid,
    range: (d: Date) => range(startOf(d, 'week', 1), endOf(d, 'week', 1)),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: (d: Date) => {
      const s = startOf(d, 'week', 1);
      const e = endOf(d, 'week', 1);
      return fullDateFormatter.formatRange(s, e).replace(' – ', ' – ');
    },
    supportsGrouping: true,
  },
  work_week: {
    component: TimeGrid,
    range: (d: Date) =>
      range(startOf(d, 'week', 1), endOf(d, 'week', 1)).filter(
        (x) => ![0, 6].includes(x.getDay()),
      ),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: (d: Date) => {
      const s = startOf(d, 'week', 1);
      const e = endOf(d, 'week', 1);
      return fullDateFormatter.formatRange(s, e).replace(' – ', ' – ');
    },
    supportsGrouping: true,
  },
  day: {
    component: TimeGrid,
    range: (d: Date) => [startOf(d, 'day')],
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'day'),
    label: (d: Date) => format(d, 'cccc dd. MM. yyyy'),
    supportsGrouping: true,
  },
  agenda: {
    component: Agenda,
    range: (d: Date) => range(d, add(d, 6, 'day'), 'day'),
    nav: (d: Date, dir: -1 | 1) => add(d, 7 * dir, 'day'),
    label: (d: Date) =>
      fullDateFormatter.formatRange(d, add(d, 6, 'day')).replace(' – ', ' – '),
    supportsGrouping: false,
  },
} as const satisfies Record<string, View>;

type ViewKey = keyof typeof Views;

const emptyArray: readonly [] = [];
const preventDefault = (e: Event) => e.preventDefault();

function prepareVariables(
  range: readonly Date[],
  trainerIds: string[],
  onlyMine: boolean,
): EventInstanceRangeQueryVariables {
  return {
    start: startOf(range[0]!, 'day').toISOString(),
    end: add(startOf(range.at(-1)!, 'day'), 1, 'day').toISOString(),
    trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
    onlyMine,
  };
}

const getResourceKey = (type: string, id: string) => `${type}:${id}`;
function parseResourceKey(key: string | undefined) {
  const pos = key?.indexOf(':') ?? -1;
  if (!key || pos === -1) return ['', ''] as const;
  return [key.slice(0, pos), key.slice(pos + 1)] as const;
}

function mapInstancesToCalendar(
  list: EventInstanceRangeQuery['list'],
  groupBy: 'none' | 'trainer' | 'room',
): { events: CalendarEvent[]; resources: Resource[] } {
  const events: CalendarEvent[] = [];
  const resourceMap = new Map<string, Resource>();
  const put = (r: Resource) => resourceMap.set(r.resourceId, r);

  for (const instance of list ?? []) {
    const { event } = instance ?? {};
    if (!event) continue;

    const start = new Date(instance.since);
    const end = new Date(instance.until);

    const resourceIds: string[] = [];

    if (groupBy === 'trainer') {
      for (const trainer of instance.trainersList ?? []) {
        const id = trainer.personId;
        if (!id) continue;
        resourceIds.push(getResourceKey('person', id));
        put({
          resourceId: getResourceKey('person', id),
          resourceTitle: trainer.person?.name || '',
        });
      }
    } else if (groupBy === 'room') {
      if (event.location?.id) {
        resourceIds.push(getResourceKey('location', event.location.id));
        put({
          resourceId: getResourceKey('location', event.location.id),
          resourceTitle: event.location.name,
        });
      } else if (event.locationText) {
        resourceIds.push(getResourceKey('locationText', event.locationText));
        put({
          resourceId: getResourceKey('locationText', event.locationText),
          resourceTitle: event.locationText,
        });
      }
    }
    if (groupBy !== 'none' && resourceIds.length === 0) {
      resourceIds.push('');
      put({ resourceId: '', resourceTitle: '-' });
    }
    events.push({ event, instance, resourceIds, start, end });
  }

  const resources = [...resourceMap.values()].toSorted((a, b) =>
    a.resourceTitle.localeCompare(b.resourceTitle),
  );

  return { events, resources };
}

export function Calendar() {
  const auth = useAuth();
  const client = useClient();
  const { lockEventsByDefault } = useAtomValue(tenantConfigAtom);
  const [onlyMine, setOnlyMine] = useQueryParam('my', withDefault(BooleanParam, false));
  const [viewInput, setView] = useQueryParam('v', withDefault(StringParam, 'agenda'));
  const view = Views[(viewInput as ViewKey) ?? ''] ?? Views.agenda;
  const [date, setDate] = React.useState(new Date());

  const isDragging = useAtomValue(isDraggingAtom);
  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const trainerIds = useAtomValue(trainerIdsFilterAtom);

  const { range, vars } = React.useMemo(() => {
    const range = view.range(date);
    const prev = view.range(view.nav(date, -1));
    const next = view.range(view.nav(date, 1));
    return {
      range,
      vars: {
        current: prepareVariables(range, trainerIds, onlyMine),
        prev: prepareVariables(prev, trainerIds, onlyMine),
        next: prepareVariables(next, trainerIds, onlyMine),
      },
    };
  }, [view, date, trainerIds, onlyMine]);

  React.useEffect(() => {
    const t = setTimeout(() => {
      client
        .query(EventInstanceRangeDocument, vars.prev, { requestPolicy: 'cache-first' })
        .toPromise();
      client
        .query(EventInstanceRangeDocument, vars.next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);
    return () => clearTimeout(t);
  }, [client, vars.prev, vars.next]);

  const [{ data, fetching }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: vars.current,
    requestPolicy: 'cache-and-network',
  });
  const list = data?.list ?? null;

  const { events, resources } = React.useMemo(() => {
    return mapInstancesToCalendar(list, onlyMine ? 'none' : groupBy);
  }, [groupBy, list, onlyMine]);

  const [, moveEvent] = useMutation(MoveEventInstanceDocument);
  const onMove = React.useCallback(
    async ({ instance }: CalendarEvent, info: InteractionInfo) => {
      const [type, resourceId] = parseResourceKey(info.resource?.resourceId);

      await moveEvent({
        input: {
          id: instance.id,
          since: info.start.toISOString(),
          until: info.end.toISOString(),
          trainerPersonId: type === 'person' && resourceId ? resourceId : null,
          locationId: type === 'location' && resourceId ? resourceId : null,
          locationText: type === 'locationText' && resourceId ? resourceId : null,
        },
      });
    },
    [moveEvent],
  );

  const onResize = React.useCallback(
    async ({ instance }: CalendarEvent, { start, end }: InteractionInfo) => {
      await moveEvent({
        input: {
          id: instance.id,
          since: start.toISOString(),
          until: end.toISOString(),
        },
      });
    },
    [moveEvent],
  );

  const [creating, setCreating] = React.useState<undefined | Partial<EventFormType>>();

  const onSelectSlot = React.useCallback(
    (slot: SlotInfo) => {
      const end = slot.action === 'click' ? add(slot.start, 45, 'minutes') : slot.end;

      const def: Partial<EventFormType> = {
        instances: [
          {
            itemId: null,
            ...datetimeRangeToTimeRange(slot.start, end),
            isCancelled: false,
            trainers: [],
          },
        ],
        isVisible: true,
        isLocked: lockEventsByDefault,
        type: 'LESSON',
        capacity: 2,
        locationId: 'none',
      };

      const [type, resourceId] = parseResourceKey(slot.resource?.resourceId);
      if (type === 'person' && resourceId) {
        def.trainers = [{ itemId: null, personId: resourceId, lessonsOffered: 0 }];
      } else if (onlyMine && !slot.resource) {
        const trainer = auth.persons.find((x) => x.isTrainer);
        if (trainer) {
          def.trainers = [{ itemId: null, personId: trainer.id, lessonsOffered: 0 }];
        }
      }

      if (type === 'location' && resourceId) {
        def.locationId = resourceId;
      }
      if (type === 'locationText' && resourceId) {
        def.locationId = 'other';
        def.locationText = resourceId;
      }
      if (def.trainers?.[0] && def.locationId === 'none') {
        const thisTrainer = def.trainers[0].personId!;
        let closestPrev: CalendarEvent | undefined;
        const thisInstance = def.instances?.[0]!;
        for (const event of events) {
          if (!event.instance.since.startsWith(thisInstance.date!)) continue;
          if (!event.instance.trainersList?.some((x) => x.personId === thisTrainer))
            continue;
          if (event.instance.until.slice(11, 19) > thisInstance.startTime) continue;
          if (!closestPrev || closestPrev.start < event.start) {
            closestPrev = event;
          }
        }
        if (closestPrev?.event?.locationText) {
          def.locationId = 'other';
          def.locationText = closestPrev.event.locationText;
        }
        if (closestPrev?.event?.location?.id) {
          def.locationId = closestPrev.event.location.id;
        }
      }

      setTimeout(() => setCreating((prev) => prev || def));
    },
    [lockEventsByDefault, onlyMine, auth.persons, events],
  );

  React.useEffect(() => {
    setDragListeners({ onMove, onResize, onSelectSlot, onDrillDown: setDate });
    return () => setDragListeners({});
  }, [onMove, onResize, onSelectSlot, setDate, setDragListeners]);

  const label = React.useMemo(() => view.label(date), [view, date]);

  return (
    <div
      className={cn(
        'overscroll-contain h-[calc(100dvh-68px)] lg:h-full rbc-calendar col-full overflow-hidden',
        isDragging && 'rbc-is-dragging',
      )}
    >
      <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse lg:flex-row items-center">
        <div className="flex gap-2 flex-wrap items-start">
          <div className={buttonGroupCls()}>
            <button
              type="button"
              className={buttonCls({ variant: 'outline', className: 'py-0' })}
              onClick={() => setDate(view.nav(date, -1))}
            >
              <MoveLeft className="!size-6 mx-1" />
            </button>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(new Date())}
            >
              Dnes
            </button>
            <button
              type="button"
              className={buttonCls({ variant: 'outline', className: 'py-0' })}
              onClick={() => setDate(view.nav(date, +1))}
            >
              <MoveRight className="!size-6 mx-1" />
            </button>
          </div>

          <ViewPicker view={viewInput} setView={setView} />

          <button
            type="button"
            className={buttonCls({ variant: onlyMine ? 'primary' : 'outline' })}
            onClick={() => setOnlyMine((x) => !x)}
          >
            Pouze moje
          </button>

          {!onlyMine && view.supportsGrouping && <GroupByPicker />}

          <TrainerFilter />

          {fetching && <Spinner />}
        </div>

        <span className="grow px-3 text-right">{label}</span>
      </div>

      <view.component
        date={date}
        range={range}
        events={events}
        backgroundEvents={emptyArray}
        resources={resources}
      />

      <CalendarConflictsIndicator start={vars.current.start} end={vars.current.end} />

      {auth.isTrainerOrAdmin && (
        <Dialog
          open={!!creating}
          onOpenChange={(open) => {
            if (!open) setTimeout(() => setCreating(undefined));
          }}
          modal={false}
        >
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            <UpsertEventForm initialValue={creating} />
          </DialogContent>
        </Dialog>
      )}
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
        {tenant?.tenantTrainersList
          ?.filter((x) => x.status === 'ACTIVE')
          .map((x) => (
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
  setView,
}: {
  view: string;
  setView: (view: ViewKey) => void;
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
