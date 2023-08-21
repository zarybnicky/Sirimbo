import React from 'react';
import classNames from 'classnames';
import { useAuth } from '@app/ui/use-auth';
import { formatEventType, formatRegistrant } from '@app/ui/format-name';
import { dateTimeFormatter, shortTimeFormatter } from '@app/ui/format-date';
import {
  CancelRegistrationDocument,
  EventInstanceExtendedFragment,
  RegisterToEventDocument,
} from '@app/graphql/Event';
import { Calendar, Clock, User, Users } from 'lucide-react';
import { fullDateFormatter } from '@app/ui/format-date';
import { diff } from 'date-arithmetic';
import { Popover, PopoverContent, PopoverTrigger } from './popover';
import Link from 'next/link';
import { useMutation } from 'urql';

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

  const create = useMutation(RegisterToEventDocument)[1];
  const cancel = useMutation(CancelRegistrationDocument)[1];
  const registrations = event.eventRegistrationsList || [];
  const myRegistrations = registrations.filter(
    (x) => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId),
  );
  const trainers = event.eventTrainersList.map((x) => x.person!) || [];

  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const duration = diff(start, end, 'minutes');

  // icon by type: camp=calendar, reservation=question mark, holiday=beach, lesson=milestone
  // icon, trainer name(s)/participant name(s) + "..."
  //
  // camp: spots/lessons, location, trainers, přihláška na stránce události
  // reservation: spots/lessons, location, trainers, lekce ve vyskakovacím okně
  // lesson: duration, spots/lessons, location, trainers, účastníci/skupiny (top 3), přihláška jako tlačítko
  // holiday: no popup

  const trigger = (
    <div
      className={classNames(
        'group flex gap-3 p-2.5 rounded-lg',
        'leading-4 text-sm tabular-nums cursor-pointer',
        event?.type === 'LESSON' && (event.remainingLessons ?? 0) > 0
          ? 'hover:bg-green-100/80 bg-green-100 text-green-900'
          : 'hover:bg-accent-4',
        !showTrainer && myRegistrations.length > 0 && 'bg-accent-5',
      )}
    >
      <div className="text-neutral-11">
        {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
      </div>
      <div className="grow">
        {event.name ||
          (registrations.length === 0
            ? 'VOLNÁ'
           : (formatRegistrant(registrations[0]!) + (registrations.length > 1 ? ', ...' : '')))}
      </div>
      {duration < 120 && <div className="text-neutral-11">{duration}&apos;</div>}
    </div>
  );

  return (
    <Popover onOpenChange={setOpen} open={open}>
      <PopoverTrigger asChild>{trigger}</PopoverTrigger>
      <PopoverContent align="start" className="flex flex-col gap-4">
        <div className="flex items-center gap-2">
          <div className="w-6 h-6 text-red-500" />
          {formatEventType(event)}
          <Link href={`/akce/${event.id}`}>Otevřít</Link>
        </div>
        <div className="flex items-center gap-2">
          <Calendar className="w-6 h-6 text-red-500" />
          {fullDateFormatter.formatRange(start, end)}
        </div>

        {event.type === 'LESSON' && (
          <div className="flex items-center gap-2">
            <Clock className="w-6 h-6 text-red-500" />
            {shortTimeFormatter.formatRange(start, end)}
          </div>
        )}

        <div className="flex items-center gap-2">
          <User className="w-6 h-6 text-red-500" />
          {trainers.map((x) => `${x.firstName} ${x.lastName}`).join(', ')}
        </div>

        <div className="flex items-center gap-2">
          <Users className="w-6 h-6 text-red-500" />
          <span>
            {registrations.length === 0 ? (
              <div>VOLNÁ</div>
            ) : (
              registrations.map((reg) => <div key={reg.id}>{formatRegistrant(reg)}</div>)
            )}
          </span>
        </div>

        {/* {canBook && (
        <SubmitButton
          className="col-span-2"
          loading={bookFetching}
          onClick={async () => {
            await book({ id: lesson.id })
            setOpen(false);
          }}
        >
          Přihlásit
        </SubmitButton>
      )}

      {canCancel && (
        <SubmitButton
          className="col-span-2"
          loading={cancelFetching}
          onClick={async () => {
            await cancel({ id: lesson.id });
            setOpen(false);
          }}
        >
          Zrušit
        </SubmitButton>
      )} */}
      </PopoverContent>
    </Popover>
  );
};
