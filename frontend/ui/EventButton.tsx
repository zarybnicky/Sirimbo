import type { EventInstanceWithEventFragment } from '@/graphql/Event';
import { EventSummary } from '@/ui/EventSummary';
import { cn } from '@/ui/cn';
import { dateTimeFormatter, formatEventType, formatRegistrant, shortTimeFormatter } from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { useAuth } from '@/ui/use-auth';
import { diff } from 'date-arithmetic';
import React from 'react';

type Props = {
  instance: EventInstanceWithEventFragment;
  showDate?: boolean;
  alwaysExpanded?: boolean;
  viewer: 'auto' | 'trainer' | 'couple';
};

type NonEmptyArray<T> = [T, ...T[]];
const isNonEmpty = <T,>(array: Array<T>): array is NonEmptyArray<T> => array.length > 0;

export const EventButton = ({ instance, viewer, showDate }: Props) => {
  const auth = useAuth();

  const event = instance.event;
  if (!event) return null;

  const registrations = event.eventRegistrations.nodes || [];

  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const duration = diff(start, end, 'minutes');

  const trainerIds = instance.event?.eventTrainersList.map(x => x.personId) || [];
  const showTrainer =
    viewer === 'couple' ? true :
      viewer === 'trainer' ? false :
        !trainerIds.filter(id => auth.personIds.includes(id)).length;

  // icon by type: camp=calendar, reservation=question mark, holiday=beach, lesson=milestone
  // icon, trainer name(s)/participant name(s) + "..."

  // camp: spots/lessons, location, trainers, přihláška na stránce události
  // reservation: spots/lessons, location, trainers, lekce ve vyskakovacím okně
  // lesson: duration, spots/lessons, location, trainers, účastníci/skupiny (top 3), přihláška jako tlačítko
  // holiday: no popup

  return (
    <div
      className={cn(
        'group flex gap-1 rounded-lg',
        'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
      )}
    >
      <Popover>
        <PopoverTrigger asChild>
          <div
            className={cn(
              'group grow flex gap-3 p-2.5 rounded-lg',
              'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
              (event?.type === 'LESSON' && event.capacity > event.eventRegistrations.totalCount * 2)
                ? 'hover:bg-green-3/80 bg-green-3 text-green-11'
                : 'hover:bg-accent-4',
            )}
          >
            <div className="text-neutral-11">
              {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
            </div>
            <div className={cn("grow", instance.isCancelled ? 'line-through' : '')}>
              {event.name || (showTrainer ? (
                `${formatEventType(event)}: ${event.eventTrainersList.map(x => x.name).join(', ')}`
              ) : (
                isNonEmpty(registrations)
                  ? formatRegistrant(registrations[0]) + (registrations.length > 1 ? ', ...' : '')
                  : 'VOLNO'
              ))}
            </div>
            {duration < 120 && <div className="text-neutral-11">{duration}&apos;</div>}
          </div>
        </PopoverTrigger>

        <PopoverContent align="start">
          <EventSummary offsetButtons instance={instance} />
        </PopoverContent>
      </Popover>
    </div>
  );
};
