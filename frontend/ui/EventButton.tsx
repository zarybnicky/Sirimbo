import React from 'react';
import classNames from 'classnames';
import { formatEventType, formatRegistrant } from '@app/ui/format';
import { dateTimeFormatter, shortTimeFormatter } from '@app/ui/format';
import { EventInstanceWithEventFragment } from '@app/graphql/Event';
import { diff } from 'date-arithmetic';
import { Popover, PopoverContent, PopoverTrigger } from './popover';
import { EventSummary } from './EventSummary';
import { UpsertEventSmallButton } from './event-form/UpsertEventForm';
import { DeleteInstanceButton } from './DeleteEventButton';

type Props = {
  instance: EventInstanceWithEventFragment;
  showTrainer?: boolean;
  showDate?: boolean;
  alwaysExpanded?: boolean;
};

export const EventButton = ({ instance, showTrainer, showDate }: Props) => {
  const [open, setOpen] = React.useState(false);
  const event = instance.event;

  if (!event) return null;

  const registrations = event.eventRegistrations.nodes || [];

  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const duration = diff(start, end, 'minutes');

  // icon by type: camp=calendar, reservation=question mark, holiday=beach, lesson=milestone
  // icon, trainer name(s)/participant name(s) + "..."

  // camp: spots/lessons, location, trainers, přihláška na stránce události
  // reservation: spots/lessons, location, trainers, lekce ve vyskakovacím okně
  // lesson: duration, spots/lessons, location, trainers, účastníci/skupiny (top 3), přihláška jako tlačítko
  // holiday: no popup

  return (
    <Popover onOpenChange={setOpen} open={open}>
      <PopoverTrigger asChild>
        <div
          className={classNames(
            'group flex gap-3 p-2.5 rounded-lg',
            'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
            event?.type === 'LESSON' && (event.remainingLessons ?? 0) > 0
              ? 'hover:bg-green-100/80 bg-green-100 text-green-900'
              : 'hover:bg-accent-4',
          )}
        >
          <div className="text-neutral-11">
            {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
          </div>
          <div className="grow">
            {event.name || (showTrainer ? (
              (formatEventType(event) + ': ') + event.eventTrainersList.map(x => x.person?.name).join(', ')
            ) : (
              registrations.length === 0
              ? 'Volná lekce'
              : formatRegistrant(registrations[0]!) + (registrations.length > 1 ? ', ...' : '')
            ))}
          </div>
          {duration < 120 && <div className="text-neutral-11">{duration}&apos;</div>}
        </div>
      </PopoverTrigger>

      <PopoverContent align="start" className="pt-10">
        <EventSummary instance={instance} />
        {instance.event && <UpsertEventSmallButton className="absolute top-4 right-16" event={instance.event} />}
        {instance && <DeleteInstanceButton className="absolute top-4 right-10" instance={instance} />}
      </PopoverContent>
    </Popover>
  );
};
