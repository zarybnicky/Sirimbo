import type { EventWithAttendanceQuery } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { canManageInstance, eventRegistrationActions, eventExternalRegistrationActions } from '@/lib/actions/eventInstance';
import { ActionRow } from '@/ui/ActionRow';
import { ActionGroup } from '@/ui/ActionGroup';
import { EventAttendance } from '@/ui/EventAttendance';
import { dateTimeFormatter, formatCoupleName, numericDateFormatter } from '@/ui/format';
import Link from 'next/link';
import { useAuth } from '@/lib/auth';

export function EventRegistrations({
  instance,
}: {
  instance: NonNullable<EventWithAttendanceQuery['event']>;
}) {
  const auth = useAuth();
  const registrations = instance.eventInstanceRegistrationsByInstanceId.nodes;
  const sortedRegistrations = registrations
    .toSorted((a, b) =>
      (a.person ? a.person.lastName + a.person.firstName : formatCoupleName(a.couple))
        .localeCompare(b.person ? b.person.lastName + b.person.firstName : formatCoupleName(b.couple)),
    );
  const rows = sortedRegistrations
    .filter((r) => !r.parentRegistrationId)
    .flatMap((r) => [r, ...sortedRegistrations.filter((x) => x.parentRegistrationId === r.id)]);
  const attendees = rows.filter((r) => r.person && r.status);
  const canEditAttendance = canManageInstance({ auth, item: instance });
  const externalRegistrations = instance.externalRegistrations;
  const externalRegistrationActionMap = useActionMap(
    eventExternalRegistrationActions,
    externalRegistrations,
  );
  const registrationActionMap = useActionMap(
    eventRegistrationActions,
    registrations.filter((r) => !r.parentRegistrationId).map((r) => ({ ...r, instance })),
  );

  return (
    <div className="prose prose-accent max-w-none">
      {auth.isTrainer && instance.seriesId && (
        <Link href={`/terminy/${instance.seriesId}`}>Zpět na seznam termínů</Link>
      )}
      <table className="mt-0">
        <thead>
          <tr>
            <th>
              {numericDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
            </th>
            {auth.isTrainer && (
              <th>
                <div className="flex justify-center gap-2">
                  <span className="rounded-full bg-green-3 px-3 py-2 tabular-nums text-sm font-medium text-green-11">
                    {attendees.filter((r) => r.status === 'ATTENDED').length}
                  </span>
                  <span className="rounded-full bg-danger-3 px-3 py-2 tabular-nums text-sm font-medium text-danger-11">
                    {attendees.filter((r) => r.status === 'NOT_EXCUSED').length}
                  </span>
                </div>
              </th>
            )}
          </tr>
        </thead>
        <tbody>
          {rows.map((x) => {
            return (
              <tr key={x.id}>
                <td className={x.parentRegistrationId ? 'align-middle pl-8' : 'align-middle'}>
                  <div className="flex items-center gap-3">
                    <ActionGroup
                      actions={registrationActionMap.get(x.id) ?? []}
                      primary="eventRegistration.edit"
                      iconOnly
                    />
                    <div>
                      {x.person ? (
                        <Link href={`/clenove/${x.person.id}`}>{x.person.name}</Link>
                      ) : x.couple ? (
                        <Link href={`/pary/${x.couple.id}`}>{formatCoupleName(x.couple)}</Link>
                      ) : null}
                      {canEditAttendance && instance.seriesId && x.person && (
                        <div className="text-xs text-neutral-9">
                          Poslední účast: {x.lastAttended ? dateTimeFormatter.format(new Date(x.lastAttended)) : '-'}
                        </div>
                      )}
                      {auth.isTrainer && (
                        <div className="text-sm text-neutral-11">
                          {x.requests.map((request) => (
                            <div key={request.id}>
                              {request.lessonCount}× {request.trainer?.person?.name}
                            </div>
                          ))}
                          {x.note && <div className="whitespace-pre-wrap">{x.note}</div>}
                        </div>
                      )}
                    </div>
                  </div>
                </td>
                {auth.isTrainer && (
                  <td className="text-center align-middle py-0">
                    {x.person && x.status && (
                      <EventAttendance attendance={x} canEdit={canEditAttendance} />
                    )}
                  </td>
                )}
              </tr>
            );
          })}
          {externalRegistrations.length > 0 && (
            <tr>
              <th colSpan={auth.isTrainer ? 2 : 1}>Externí přihlášky</th>
            </tr>
          )}
          {externalRegistrations.map((r) => (
            <tr key={r.id}>
              <td colSpan={auth.isTrainer ? 2 : 1}>
                <ActionRow actions={externalRegistrationActionMap.get(r.id)!} className="mb-0">
                  <div>
                    {r.prefixTitle} {r.firstName} {r.lastName}{' '}
                    {r.suffixTitle}
                    {auth.isTrainer && r.note && (
                      <div className="whitespace-pre-wrap text-sm text-neutral-11">{r.note}</div>
                    )}
                  </div>
                </ActionRow>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
