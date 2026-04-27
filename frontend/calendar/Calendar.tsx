import { MoveEventInstanceDocument } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent } from '@/ui/dialog';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { add } from 'date-arithmetic';
import { useAtomValue, useSetAtom } from 'jotai';
import React from 'react';
import { useMutation } from 'urql';
import { BooleanParam, StringParam, useQueryParam, withDefault } from 'use-query-params';
import {
  dragListenersAtom,
  groupByAtom,
  isDraggingAtom,
  trainerIdsFilterAtom,
} from './state';
import type { CalendarEvent, InteractionInfo, SlotInfo } from './types';
import { Spinner } from '@/ui/Spinner';
import { EventFormType } from '@/ui/event-form/types';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import { CalendarViewKey, CalendarViews } from '@/calendar/CalendarViews';
import { useCalendarData } from '@/calendar/useCalendarData';
import { TrainerFilter } from '@/calendar/TrainerFilter';
import { GroupByPicker } from '@/calendar/GroupByPicker';
import { ViewPicker } from '@/calendar/ViewPicker';
import { CalendarDatePicker } from '@/calendar/CalendarDatePicker';

const emptyArray: readonly [] = [];
const preventDefault = (e: Event) => e.preventDefault();

function parseResourceKey(key: string | undefined) {
  const pos = key?.indexOf(':') ?? -1;
  if (!key || pos === -1) return ['', ''] as const;
  return [key.slice(0, pos), key.slice(pos + 1)] as const;
}

function slotToEventForm(
  slot: SlotInfo,
  events: CalendarEvent[],
  persons: { id: string; isTrainer: boolean | null }[],
  onlyMine: null | boolean,
) {
  const end = slot.action === 'click' ? add(slot.start, 45, 'minutes') : slot.end;

  const def: Partial<EventFormType> = {
    instances: [
      {
        itemId: null,
        since: slot.start.toISOString(),
        until: end.toISOString(),
        isCancelled: false,
        trainers: [],
      },
    ],
    isVisible: true,
    type: 'LESSON',
    capacity: 2,
    locationId: 'none',
  };

  const [type, resourceId] = parseResourceKey(slot.resource?.resourceId);
  if (type === 'person' && resourceId) {
    def.trainers = [{ itemId: null, personId: resourceId, lessonsOffered: 0 }];
  } else if (onlyMine && !slot.resource) {
    const trainer = persons.find((x) => x.isTrainer);
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
    const thisInstance = def.instances?.[0];
    if (thisInstance?.since && thisInstance.until) {
      for (const event of events) {
        if (!event.instance.since.startsWith(thisInstance.since.slice(0, 10))) continue;
        if (!event.instance.trainersList?.some((x) => x.personId === thisTrainer))
          continue;
        if (event.instance.until.slice(11, 19) > thisInstance.since.slice(11, 19))
          continue;
        if (!closestPrev || closestPrev.start < event.start) {
          closestPrev = event;
        }
      }
    }
    if (closestPrev?.instance?.locationText) {
      def.locationId = 'other';
      def.locationText = closestPrev.instance.locationText;
    }
    if (closestPrev?.instance?.location?.id) {
      def.locationId = closestPrev.instance.location.id;
    }
  }
  return def;
}

export function Calendar() {
  const auth = useAuth();
  const [onlyMine, setOnlyMine] = useQueryParam('my', withDefault(BooleanParam, false));
  const [viewInput, setView] = useQueryParam('v', withDefault(StringParam, 'agenda'));
  const view =
    CalendarViews[(viewInput as CalendarViewKey) ?? ''] ?? CalendarViews.agenda;
  const [date, setDate] = React.useState(new Date());

  const isDragging = useAtomValue(isDraggingAtom);
  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const trainerIds = useAtomValue(trainerIdsFilterAtom);
  const filters = React.useMemo(
    () => ({
      onlyMine,
      trainerIds,
    }),
    [onlyMine, trainerIds],
  );

  const { fetching, variables, range, events, resources } = useCalendarData(
    view,
    date,
    filters,
    groupBy,
  );

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
      const def = slotToEventForm(slot, events, auth.persons, onlyMine);
      setTimeout(() => setCreating((prev) => prev || def));
    },
    [onlyMine, auth.persons, events],
  );

  React.useEffect(() => {
    setDragListeners({ onMove, onResize, onSelectSlot, onDrillDown: setDate });
    return () => setDragListeners({});
  }, [onMove, onResize, onSelectSlot, setDate, setDragListeners]);

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

        <span className="grow px-3 text-right">{view.label(range)}</span>
      </div>

      <view.component
        range={range}
        events={events}
        backgroundEvents={emptyArray}
        resources={resources}
      />

      <CalendarConflictsIndicator start={variables.start} end={variables.end} />

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
