import { memo } from 'react';
import type { EventWithAttendanceQuery } from '@/graphql/Event';
import { useActionMap } from '@/lib/actions';
import { eventRegistrationActions } from '@/lib/actions/event';
import { ActionGroup } from '@/ui/ActionGroup';
import { EventAttendance } from '@/ui/EventAttendance';
import { Table } from '@/ui/Table';
import { dateTimeFormatter } from '@/ui/format';
import { EntityLink } from '@/ui/EntityLink';
import { useAuth } from '@/lib/auth';
import { compareText } from '@/lib/compare';

export const EventRegistrations = memo(function EventRegistrations({
  event,
}: {
  event: NonNullable<EventWithAttendanceQuery['event']>;
}) {
  const auth = useAuth();
  const registrations = event.attendanceRegistrations.nodes;
  const sortedRegistrations = registrations.toSorted(
    (a, b) =>
      compareText(
        a.person?.lastName ?? a.couple?.shortName ?? '',
        b.person?.lastName ?? b.couple?.shortName ?? '',
      ) ||
      compareText(
        a.person?.firstName ?? a.couple?.name ?? '',
        b.person?.firstName ?? b.couple?.name ?? '',
      ),
  );
  const rows = sortedRegistrations
    .filter((r) => !r.parentRegistrationId)
    .flatMap((r, groupIndex) =>
      [r, ...sortedRegistrations.filter((x) => x.parentRegistrationId === r.id)].map(
        (registration) => ({ ...registration, groupIndex }),
      ),
    );
  const attendees = rows.filter((r) => r.person && r.status);
  const canEditAttendance = auth.canManageEvent(event);
  const registrationActionMap = useActionMap(
    eventRegistrationActions,
    registrations.filter((r) => !r.parentRegistrationId).map((r) => ({ ...r, event })),
  );

  return (
    <Table
      bordered={false}
      empty="Žádné přihlášky"
      columns={[
        <span key="attendee" className="sr-only">
          Účastník
        </span>,
        auth.isTrainer && rows.length > 0 && (
          <div key="attendance" className="flex justify-center gap-2">
            <span className="rounded-full bg-green-3 px-3 py-2 tabular-nums text-sm font-medium text-green-11">
              {attendees.filter((r) => r.status === 'ATTENDED').length}
            </span>
            <span className="rounded-full bg-danger-3 px-3 py-2 tabular-nums text-sm font-medium text-danger-11">
              {attendees.filter((r) => r.status === 'NOT_EXCUSED').length}
            </span>
          </div>
        ),
      ]}
    >
      {rows.map((x) => (
        <tr
          key={x.id}
          className={x.groupIndex % 2 === 0 ? 'bg-neutral-3/80' : 'bg-neutral-1'}
        >
          <td>
            <div
              className={`flex items-center gap-3 ${x.parentRegistrationId ? 'pl-8' : 'pl-1'}`}
            >
              <ActionGroup
                actions={registrationActionMap.get(x.id) ?? []}
                primary="eventRegistration.edit"
                iconOnly
              />
              <div>
                <EntityLink type="participant" entity={x}>
                  {x.person?.name || x.couple?.name}
                </EntityLink>
                {x.person?.isExternal && (
                  <span className="ml-2 rounded bg-neutral-3 px-1.5 py-0.5 text-xs text-neutral-11">
                    Externí
                  </span>
                )}
                {canEditAttendance && (event.seriesInfo?.length ?? 0) > 1 && (
                  <div className="text-xs text-neutral-9">
                    Poslední účast:{' '}
                    {x.lastAttended
                      ? dateTimeFormatter.format(new Date(x.lastAttended))
                      : '-'}
                  </div>
                )}
                {auth.isTrainer && (
                  <div className="text-sm text-neutral-11">
                    {x.lessonRequestsList.map((request) => (
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
            <td className="text-center">
              <EventAttendance attendance={x} canEdit={canEditAttendance} />
            </td>
          )}
        </tr>
      ))}
    </Table>
  );
});
