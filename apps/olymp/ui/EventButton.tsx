import React from 'react';
import classNames from 'classnames';
import { useAuth } from '@app/ui/use-auth';
import { formatRegistrant } from '@app/ui/format';
import { dateTimeFormatter, shortTimeFormatter } from '@app/ui/format';
import { EventInstanceExtendedFragment } from '@app/graphql/Event';
import { diff } from 'date-arithmetic';
import { Popover, PopoverContent, PopoverTrigger } from './popover';
import { EventSummary } from './EventSummary';

type Props = {
  instance: EventInstanceExtendedFragment;
  showTrainer?: boolean;
  showDate?: boolean;
  alwaysExpanded?: boolean;
};

export const EventButton = ({ instance, showTrainer, showDate }: Props) => {
  const { perms } = useAuth();
  const [open, setOpen] = React.useState(false);
  const event = instance.event!;

  const registrations = event.eventRegistrationsList || [];
  const hasMyRegistrations = registrations.some(
    (x) => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId),
  );

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
            'leading-4 text-sm tabular-nums cursor-pointer',
            event?.type === 'LESSON' && (event.remainingLessons ?? 0) > 0
              ? 'hover:bg-green-100/80 bg-green-100 text-green-900'
              : 'hover:bg-accent-4',
            !showTrainer && hasMyRegistrations && 'bg-accent-5',
          )}
        >
          <div className="text-neutral-11">
            {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
          </div>
          <div className="grow">
            {event.name ||
              (registrations.length === 0
                ? 'VOLNÁ'
                : formatRegistrant(registrations[0]!) +
                  (registrations.length > 1 ? ', ...' : ''))}
          </div>
          {duration < 120 && <div className="text-neutral-11">{duration}&apos;</div>}
        </div>
      </PopoverTrigger>

      <PopoverContent align="start" asChild>
        <EventSummary instance={instance} />
      </PopoverContent>
    </Popover>
  );
};