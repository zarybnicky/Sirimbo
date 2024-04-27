import React from 'react';
import { formatEventType, formatRegistrant } from '@/ui/format';
import { dateTimeFormatter, shortTimeFormatter } from '@/ui/format';
import { EventInstanceWithEventFragment } from '@/graphql/Event';
import { diff } from 'date-arithmetic';
import { Popover, PopoverContent, PopoverTrigger } from './popover';
import { EventSummary } from './EventSummary';
import { useAuth } from './use-auth';
import { cn } from './cn';

type Props = {
  instance: EventInstanceWithEventFragment;
  showDate?: boolean;
  alwaysExpanded?: boolean;
  viewer: 'auto' | 'trainer' | 'couple';
};

export const EventButton = ({ instance, viewer, showDate }: Props) => {
  const [open, setOpen] = React.useState(false);
  const { persons } = useAuth();

  const event = instance.event;
  if (!event) return null;

  const registrations = event.eventRegistrations.nodes || [];

  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const duration = diff(start, end, 'minutes');

  const personIds = persons.map(x => x.id);
  const trainerIds = instance.event?.eventTrainersList.map(x => x.person?.id || '') || [];
  const showTrainer =
    viewer === 'couple' ? true :
      viewer === 'trainer' ? false :
        !trainerIds.filter(id => personIds.includes(id)).length;

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
      <Popover onOpenChange={setOpen} open={open}>
        <PopoverTrigger asChild>
          <div
            className={cn(
              'group grow flex gap-3 p-2.5 rounded-lg',
              'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
              (event?.type === 'LESSON' && (event.remainingLessons ?? 0) > 0)
                ? 'hover:bg-green-100/80 bg-green-100 text-green-900'
                : 'hover:bg-accent-4',
            )}
          >
            <div className="text-neutral-11">
              {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
            </div>
            <div className={cn("grow", instance.isCancelled ? 'line-through' : '')}>
              {event.name || (showTrainer ? (
                (formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')
              ) : (
                registrations.length === 0
                  ? 'VOLNO'
                  : formatRegistrant(registrations[0]!) + (registrations.length > 1 ? ', ...' : '')
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
