import { UpdateAttendanceDocument, type EventAttendanceFragment, EventInstanceWithAttendanceDocument } from '@/graphql/Event';
import { numericDateFormatter } from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';
import type { AttendanceType } from '@/graphql';
import { Annoyed, Check, HelpCircle, type LucideIcon, OctagonMinus, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { cn } from '@/ui/cn';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import Link from 'next/link';

export function InstanceAttendanceView({ id }: { id: string }) {
  const auth = useAuth();
  const [{ data }] = useQuery({ query: EventInstanceWithAttendanceDocument, variables: { id }, pause: !id });
  const instance = data?.eventInstance

  if (!instance?.event) return null;
  const { event } = instance;
  const isMyEvent = auth.isAdmin || (auth.isTrainer && event.eventTrainersList.find(x => auth.personIds.some(id => id === x.personId)));
  const attendanceList = instance.eventAttendancesByInstanceIdList
    .filter((x) => x.status !== 'CANCELLED')
    .filter((x) => x.person)
    .sort((x, y) => `${x.person?.lastName}${x.person?.firstName}`.localeCompare(`${y.person?.lastName}${y.person?.firstName}`));

  return (
    <div className="max-w-full overflow-x-auto">
      <div className="prose prose-accent max-w-none">
        <table>
          <thead>
            <tr>
              <th>
                <Link
                  href={{
                    pathname: '/akce/[id]',
                    query: { id: event.id, tab: 'attendance' }
                  }}
                >
                  Zpět na seznam termínů
                </Link>
              </th>
              <th className="text-center">
                {numericDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </th>
            </tr>
          </thead>
          <tbody>
            {attendanceList.map(x => (
              <tr key={x.id}>
                <td>{x.person?.name}</td>
                {isMyEvent ? (
                  <td className="text-center align-middle py-0">
                    <AttendanceItem attendance={x} />
                  </td>
                ) : (
                  <td className="text-center align-middle">
                    {React.createElement(attendanceIcons[x.status], { className: "mx-auto" })}
                  </td>
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

export const attendanceIcons: { [key in AttendanceType]: LucideIcon} = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  EXCUSED: Annoyed,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus,
}
function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'EXCUSED', 'NOT_EXCUSED', 'UNKNOWN', 'CANCELLED'].includes(x);
}

function AttendanceItem({ attendance }: { attendance: Partial<EventAttendanceFragment> }) {
  const update = useMutation(UpdateAttendanceDocument)[1];
  const setStatus = useAsyncCallback(async (status: string) => {
    if (isAttendanceType(status)) {
      await update({
        input: {
          status,
          instanceId: attendance.instanceId,
          note: attendance.note,
          personId: attendance.personId,
        },
      });
    }
  });

  return (
    <ToggleGroupPrimitive.Root
      value={attendance.status}
      onValueChange={setStatus.execute}
      type="single"
      className="flex flex-nowrap justify-center"
    >
      {Object.entries(attendanceIcons).filter(([key]) => key !== 'CANCELLED').map(([key, label]) => (
        <ToggleGroupPrimitive.Item
          key={`group-item-${key}-${label}`}
          value={key}
          className={cn(
            'group data-[state=on]:text-white data-[state=on]:bg-accent-9 bg-neutral-1 text-accent-11',
            'px-2 py-1 text-sm first:rounded-l-xl border last:rounded-r-xl',
            'border-y border-l last:border-r border-accent-7 data-[state=on]:border-accent-10',
            'disabled:border-neutral-6 disabled:data-[state=on]:border-neutral-10 disabled:data-[state=on]:bg-neutral-9 disabled:text-neutral-11 disabled:data-[state=on]:text-white',
            'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
          )}
        >
          {React.createElement(label)}
        </ToggleGroupPrimitive.Item>
      ))}
    </ToggleGroupPrimitive.Root>
  );
}
