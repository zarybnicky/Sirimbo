import { MoveEventInstanceDocument } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent } from '@/ui/dialog';
import { QuickEventCreateForm } from '@/ui/event-form/QuickEventForms';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useAtomValue, useSetAtom } from 'jotai';
import { parseAsBoolean, parseAsStringLiteral, useQueryState } from 'nuqs';
import React from 'react';
import { useMutation } from 'urql';
import {
  dragListenersAtom,
  groupByAtom,
  isDraggingAtom,
  participantIdsFilterAtom,
  trainerIdsFilterAtom,
} from './state';
import type { CalendarInstanceEvent, InteractionInfo, SlotInfo } from './types';
import { Spinner } from '@/ui/Spinner';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import { CalendarViews } from '@/calendar/CalendarViews';
import { useCalendarData } from '@/calendar/useCalendarData';
import { TrainerFilter } from '@/calendar/TrainerFilter';
import { GroupByPicker } from '@/calendar/GroupByPicker';
import { ViewPicker } from '@/calendar/ViewPicker';
import { CalendarDatePicker } from '@/calendar/CalendarDatePicker';
import { ParticipantFilter } from '@/calendar/ParticipantFilter';
import {
  parseResourceKey,
  quickDefaultsFromSlot,
  type QuickEventCreateDefaults,
} from '@/calendar/quickEventDefaults';

const emptyArray: readonly [] = [];
const preventDefault = (e: Event) => e.preventDefault();
const calendarViewKeys = ['month', 'week', 'work_week', 'day', 'agenda'] as const;

export function Calendar({
  parentId,
  initialDate,
}: { parentId?: string; initialDate?: string }) {
  const auth = useAuth();
  const [onlyMine, setOnlyMine] = useQueryState(
    'my',
    parseAsBoolean.withDefault(false).withOptions({ history: 'push' }),
  );
  const [viewInput, setView] = useQueryState(
    'v',
    parseAsStringLiteral(calendarViewKeys)
      .withDefault('agenda')
      .withOptions({ history: 'push' }),
  );
  const view = CalendarViews[viewInput];
  const [date, setDate] = React.useState(() => initialDate ? new Date(initialDate) : new Date());

  const isDragging = useAtomValue(isDraggingAtom);
  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const trainerIds = useAtomValue(trainerIdsFilterAtom);
  const participantIds = useAtomValue(participantIdsFilterAtom);
  const filters = React.useMemo(
    () => ({
      onlyMine,
      trainerIds,
      participantIds,
      myPersonIds: auth.personIds,
      parentId,
    }),
    [auth.personIds, onlyMine, trainerIds, participantIds, parentId],
  );

  const { fetching, range, events, resources } = useCalendarData(
    view,
    date,
    filters,
    groupBy,
  );
  const [, moveEvent] = useMutation(MoveEventInstanceDocument);
  const onMove = React.useCallback(
    async ({ instance }: CalendarInstanceEvent, info: InteractionInfo) => {
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
    async ({ instance }: CalendarInstanceEvent, { start, end }: InteractionInfo) => {
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

  const [creating, setCreating] = React.useState<QuickEventCreateDefaults>();

  const onSelectSlot = React.useCallback(
    (slot: SlotInfo) => {
      const defaults = quickDefaultsFromSlot(slot, events, auth.persons, onlyMine);
      setTimeout(() => setCreating((prev) => prev || defaults));
    },
    [onlyMine, auth.persons, events],
  );

  const onDrillDown = React.useCallback(
    (date: Date) => {
      setDate(date);
      setView('day');
    },
    [setView],
  );

  React.useEffect(() => {
    setDragListeners({ onMove, onResize, onSelectSlot, onDrillDown });
    return () => setDragListeners({});
  }, [onMove, onResize, onSelectSlot, onDrillDown, setDragListeners]);

  return (
    <div
      className={cn(
        'overscroll-contain h-[calc(100dvh-68px)] lg:h-full rbc-calendar col-full overflow-hidden',
        isDragging && 'rbc-is-dragging',
      )}
    >
      <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse lg:flex-row items-center">
        <div className="flex gap-2 flex-wrap items-start">
          <CalendarDatePicker date={date} setDate={setDate} view={view} />
          <ViewPicker view={viewInput} setView={setView} />
          {!onlyMine && view.supportsGrouping && <GroupByPicker />}
          <button
            type="button"
            className={buttonCls({ variant: onlyMine ? 'primary' : 'outline', size: 'sm' })}
            onClick={() => setOnlyMine((x) => !x)}
          >
            Pouze moje
          </button>
          <TrainerFilter />
          <ParticipantFilter />
          {fetching && <Spinner />}
        </div>

        <span className="grow px-3 text-right">{view.label(range)}</span>
      </div>

      <view.component
        range={range}
        events={events}
        backgroundEvents={emptyArray}
        resources={resources}
      />

      <CalendarConflictsIndicator range={range} />

      {auth.isTrainerOrAdmin && (
        <Dialog
          open={!!creating}
          onOpenChange={(open) => {
            if (!open) setTimeout(() => setCreating(undefined));
          }}
          modal={false}
        >
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            {creating && <QuickEventCreateForm defaults={creating} parentId={parentId} />}
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
}
