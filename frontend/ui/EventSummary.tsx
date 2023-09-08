import { formatRegistrant } from '@app/ui/format';
import { shortTimeFormatter } from '@app/ui/format';
import { DeleteEventInstanceDocument, EventInstanceWithEventFragment } from '@app/graphql/Event';
import { Clock, MapPin, User, Users } from 'lucide-react';
import Link from 'next/link';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { buttonCls } from './style';
import { useAuth } from './use-auth';
import { useMutation } from 'urql';
import React from 'react';
import { useConfirm } from './Confirm';

export function EventSummary({ instance }: {
  instance: EventInstanceWithEventFragment;
}) {
  const { perms } = useAuth();
  const confirm = useConfirm();
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];
  const event = instance.event;

  const deleteInstance = React.useCallback(async () => {
    if ((instance.event?.eventInstancesList.length ?? 0) < 2) {
      await confirm({ description: 'Opravdu chcete smazat CELOU UDÁLOST? Smažete tím všechny záznamy o účasti i platbách.' });
    } else {
      await confirm({ description: 'Opravdu chcete smazat JEDEN TERMÍN události? Smažete tím všechny záznamy o účasti i platbách.' });
    }
    await deleteMutation({ id: instance.id });
  }, [instance, deleteMutation]);

  if (!event) return null;

  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  return (
    <div className="flex flex-col gap-2 text-sm">
      <div className="flex items-center gap-2">
        <Clock className="w-6 h-6 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {event.locationText && (
        <div className="flex items-center gap-2">
          <MapPin className="w-6 h-6 text-accent-11" />
          {event.locationText}
        </div>
      )}

      {event.eventTrainersList.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="w-6 h-6 text-accent-11" />
          {event.eventTrainersList.map((x) => x.person?.name).join(', ')}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="w-6 h-6 text-accent-11" />
        <span>
          {event.eventTargetCohortsList.length > 0 ? (
            event.eventTargetCohortsList.map(x => (
              <div key={x.id}>{x.cohort?.sName}</div>
            ))
          ) : registrationCount  === 0 ? (
            <div>VOLNÁ</div>
          ) : myRegistrations.length > 0 ? (
            myRegistrations.map((reg) => <div key={reg.id}>{formatRegistrant(reg)}</div>).concat(
              registrationCount > myRegistrations.length ? [(
                <div key="more">a dalších {registrationCount - myRegistrations.length} účastníků</div>
              )] : []
            )
          ) : `${registrationCount} účastníků`}
        </span>
      </div>

      <div className="flex flex-wrap gap-3">
        <MyRegistrationsDialog event={event} />
        <Link href={`/akce/${event.id}`} className={buttonCls({ variant: 'outline' })}>
          Více info...
        </Link>
        {perms.isAdmin && (
          <button type="button" className={buttonCls({ variant: 'outline' })} onClick={deleteInstance}>
            Smazat
          </button>
        )}
      </div>
    </div>
  );
}
