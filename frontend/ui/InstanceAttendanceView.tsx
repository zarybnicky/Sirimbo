import {
  type EventAttendanceFragment,
  EventInstanceWithAttendanceDocument,
  UpdateAttendanceDocument,
} from '@/graphql/Event';
import { dateTimeFormatter, numericDateFormatter } from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, type LucideIcon, OctagonMinus, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { cn } from '@/lib/cn';
import Link from 'next/link';
import { isTruthy, keyIsNonNull } from '@/lib/truthyFilter';

export function InstanceAttendanceView({ id }: { id: string }) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: EventInstanceWithAttendanceDocument,
    variables: { id },
    pause: !id,
  });
  const instance = data?.eventInstance;

  const managerPersonIds = React.useMemo(
    () =>
      new Set([
        ...(instance?.trainersList?.map((x) => x.personId).filter(isTruthy) ?? []),
        ...(instance?.event?.eventTrainersList?.map((x) => x.personId).filter(isTruthy) ?? []),
      ]),
    [instance?.trainersList, instance?.event?.eventTrainersList],
  );

  if (!instance) return null;

  const canEditAttendance =
    auth.isAdmin ||
    (auth.isTrainer && auth.personIds.some((personId) => managerPersonIds.has(personId)));
  const attendanceList = instance.eventAttendancesByInstanceIdList
    .filter((x) => x.status !== 'CANCELLED')
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
      <Link
        href={{
          pathname: '/akce/[id]',
          query: { id: instance.eventId, tab: 'attendance' },
        }}
      >
        Zpět na seznam termínů
      </Link>
      <table className="mt-0">
        <thead>
          <tr>
            <th>
              {numericDateFormatter.formatRange(
                new Date(instance.since),
                new Date(instance.until),
              )}
            </th>
            <th className="flex justify-center gap-2">
              <div className="rounded-full flex gap-2 items-center bg-green-3 px-3 py-2 tabular-nums text-sm font-medium text-green-11">
                {attendedCount}
              </div>
              <div className="rounded-full flex gap-2 items-center bg-[#fbe4e8] px-3 py-2 tabular-nums text-sm font-medium text-[#b42346] dark:bg-[#471823] dark:text-[#ffb4c2]">
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
                {canEditAttendance && (
                  <div className="text-xs text-neutral-9">
                    Poslední účast:{' '}
                    {x.registration?.lastAttended
                      ? dateTimeFormatter.format(new Date(x.registration.lastAttended))
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
  CANCELLED: OctagonMinus,
};
const toggleableAttendanceIcons = {
  ATTENDED: Check,
  NOT_EXCUSED: X,
} satisfies Record<'ATTENDED' | 'NOT_EXCUSED', LucideIcon>;

function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'NOT_EXCUSED', 'UNKNOWN', 'CANCELLED'].includes(x);
}

function AttendanceItem({
  attendance,
}: {
  attendance: Partial<EventAttendanceFragment>;
}) {
  const update = useMutation(UpdateAttendanceDocument)[1];
  const setStatus = useAsyncCallback(async (status: string) => {
    const nextStatus = status === '' ? 'UNKNOWN' : status;
    if (isAttendanceType(nextStatus)) {
      await update({
        input: {
          status: nextStatus,
          instanceId: attendance.instanceId,
          note: attendance.note,
          personId: attendance.personId,
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
            'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
            attendance.status === key &&
              key === 'ATTENDED' &&
              'border-green-10 bg-green-9 hover:bg-green-8 text-white',
            attendance.status === key &&
              key === 'NOT_EXCUSED' &&
              'border-[#b42346] bg-[#d94b67] hover:bg-[#e05570] text-white',
          )}
        >
          {React.createElement(label)}
        </button>
      ))}
    </div>
  );
}
