import { formatRegistrant } from '@app/ui/format';
import { shortTimeFormatter, fullDateFormatter } from '@app/ui/format';
import { EventInstanceWithRegistrationsFragment } from '@app/graphql/Event';
import { Calendar, Clock, User, Users } from 'lucide-react';
import Link from 'next/link';
import { useAuth } from '@app/ui/use-auth';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { buttonCls } from './style';

export function EventSummary({ instance }: {
  instance: EventInstanceWithRegistrationsFragment;
}) {
  const { perms } = useAuth();
  const event = instance.event;

  if (!event) return null;

  const registrations = event.eventRegistrationsList || [];
  const myRegistrations = registrations.filter(
    (x) => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId),
  );
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  return (
    <div className="flex flex-col gap-2">
      <div className="flex items-center gap-2">
        <Calendar className="w-6 h-6 text-accent-11" />
        {fullDateFormatter.formatRange(start, end)}
      </div>

      <div className="flex items-center gap-2">
        <Clock className="w-6 h-6 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {event.eventTrainersList.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="w-6 h-6 text-accent-11" />
          {event.eventTrainersList.map((x) => x.person?.name).join(', ')}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="w-6 h-6 text-accent-11" />
        <span>
          {event.eventRegistrationsList.length === 0 ? (
            <div>VOLNÁ</div>
          ) : myRegistrations.length > 0 ? (
            myRegistrations.map((reg) => <div key={reg.id}>{formatRegistrant(reg)}</div>).concat(
              registrations.length > myRegistrations.length ? [(
                <div key="more">a dalších {registrations.length - myRegistrations.length} účastníků</div>
              )] : []
            )
          ) : `${registrations.length} účastníků`}
        </span>
      </div>

      <div className="flex flex-wrap gap-4">
        <MyRegistrationsDialog event={event} />
        <Link href={`/akce/${event.id}`} className={buttonCls({ variant: 'outline' })}>
          Více info...
        </Link>
      </div>
    </div>
  );
}
