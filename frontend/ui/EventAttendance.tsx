import {
  type EventRegistrationAttendanceFragment,
  EventWithAttendanceDocument,
  UpdateAttendanceDocument,
} from '@/graphql/Event';
import { dateTimeFormatter, numericDateFormatter } from '@/ui/format';
import { useAuth } from '@/lib/auth';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { cn } from '@/lib/cn';
import Link from 'next/link';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { canManageInstance } from '@/lib/actions/eventInstance';

export function EventAttendance({ id }: { id: string }) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !id,
  });

  const event = data?.event;
  if (!event) return null;

  const canEditAttendance = canManageInstance({ auth, item: event });
  const attendanceList = event.eventInstanceRegistrationsByInstanceId.nodes
    .filter(keyIsNonNull('status'))
    .filter(keyIsNonNull('person'))
    .toSorted((x, y) =>
      `${x.person.lastName}${x.person.firstName}`.localeCompare(
        `${y.person.lastName}${y.person.firstName}`,
      ),
    );
  const attendedCount = attendanceList.filter((x) => x.status === 'ATTENDED').length;
  const notAttendedCount = attendanceList.filter(
    (x) => x.status === 'NOT_EXCUSED',
  ).length;

  return (
    <div className="prose prose-accent max-w-none">
      <nav className="flex flex-wrap gap-x-4 gap-y-1" aria-label="Docházka">
        {event.seriesId && (
          <Link href={`/terminy/${event.seriesId}?tab=attendance`}>
            Zpět na seznam termínů
          </Link>
        )}
      </nav>
      <table className="mt-0">
        <thead>
          <tr>
            <th>
              {numericDateFormatter.formatRange(
                new Date(event.since),
                new Date(event.until),
              )}
            </th>
            <th className="flex justify-center gap-2">
              <div className="rounded-full flex gap-2 items-center bg-green-3 px-3 py-2 tabular-nums text-sm font-medium text-green-11">
                {attendedCount}
              </div>
              <div className="flex items-center gap-2 rounded-full bg-danger-3 px-3 py-2 text-sm font-medium tabular-nums text-danger-11">
                {notAttendedCount}
              </div>
            </th>
          </tr>
        </thead>
        <tbody>
          {attendanceList.map((x) => (
            <tr key={x.id}>
              <td className="align-middle">
                <div>{x.person?.name}</div>
                {canEditAttendance && event.seriesId && (
                  <div className="text-xs text-neutral-9">
                    Poslední účast:{' '}
                    {x.lastAttended
                      ? dateTimeFormatter.format(new Date(x.lastAttended))
                      : '—'}
                  </div>
                )}
              </td>
              {canEditAttendance ? (
                <td className="text-center align-middle py-0">
                  <AttendanceItem attendance={x} />
                </td>
              ) : (
                <td className="text-center align-middle">
                  {React.createElement(attendanceIcons[x.status], {
                    className: 'mx-auto',
                  })}
                </td>
              )}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export const attendanceIcons: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
};
const toggleableAttendanceIcons = {
  ATTENDED: Check,
  NOT_EXCUSED: X,
} satisfies Record<'ATTENDED' | 'NOT_EXCUSED', LucideIcon>;

function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'NOT_EXCUSED', 'UNKNOWN'].includes(x);
}

function AttendanceItem({
  attendance,
}: {
  attendance: EventRegistrationAttendanceFragment;
}) {
  const update = useMutation(UpdateAttendanceDocument)[1];
  const setStatus = useAsyncCallback(async (status: string) => {
    const nextStatus = status === '' ? 'UNKNOWN' : status;
    if (isAttendanceType(nextStatus)) {
      await update({
        input: {
          eirId: attendance.id,
          note: attendance.attendanceNote,
          status: nextStatus,
        },
      });
    }
  });

  return (
    <div className="flex flex-nowrap justify-center">
      {Object.entries(toggleableAttendanceIcons).map(([key, label]) => (
        <button
          type="button"
          onClick={() => setStatus.execute(attendance.status === key ? 'UNKNOWN' : key)}
          disabled={setStatus.loading}
          aria-pressed={attendance.status === key}
          title={attendance.status === key ? 'Kliknutím zrušíte výběr' : undefined}
          key={`group-item-${key}-${label}`}
          className={cn(
            'group bg-neutral-1 text-neutral-11 hover:bg-neutral-3',
            'px-2 py-1 text-sm first:rounded-l-xl border last:rounded-r-xl',
            'border-y border-l last:border-r border-neutral-6',
            'disabled:border-neutral-6 disabled:bg-neutral-2 disabled:text-neutral-8',
            'focus:relative focus:outline-hidden focus-visible:z-30 focus-visible:ring-3 focus-visible:ring-accent-10',
            attendance.status === key &&
              key === 'ATTENDED' &&
              'border-green-10 bg-green-9 hover:bg-green-8 text-white',
            attendance.status === key &&
              key === 'NOT_EXCUSED' &&
              'border-danger-11 bg-danger-9 text-white hover:bg-danger-10',
          )}
        >
          {React.createElement(label)}
        </button>
      ))}
    </div>
  );
}
