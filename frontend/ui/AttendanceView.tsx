import { UpdateAttendanceDocument, EventAttendanceFragment, EventInstanceWithAttendanceDocument } from '@app/graphql/Event';
import { numericDateFormatter } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';
import { AttendanceType } from '@/graphql';
import { Annoyed, Check, HelpCircle, LucideIcon, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { cn } from './cn';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import Link from 'next/link';

export function AttendanceView({ id }: { id: string }) {
  const { perms } = useAuth();
  const [{ data }] = useQuery({ query: EventInstanceWithAttendanceDocument, variables: { id }, pause: !id });
  const instance = data?.eventInstance

  if (!instance?.event) return null;
  const { event } = instance;
  const isMyEvent = perms.isAdmin || (perms.isTrainer && event.eventTrainersList.find(x => perms.isCurrentPerson(x.person?.id || '')));

  return (
    <div className="max-w-full overflow-x-auto">
      <div className="prose prose-accent max-w-none">
        <table>
          <thead>
            <tr>
              <th>
                <Link href={`/akce/${event.id}?tab=attendance`}>Zpět na seznam termínů</Link>
              </th>
              <th className="text-center" key={instance.id}>
                {numericDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </th>
            </tr>
          </thead>
          <tbody>
            {instance.eventAttendancesByInstanceIdList.map(x => (
              <tr key={x.person?.id}>
                <td>{x.person?.name}</td>
                {isMyEvent ? (
                  <td className="text-center align-middle py-0">
                    <AttendanceItem key={instance.id} attendance={x} />
                  </td>
                ) : (
                  <td className="text-center align-middle">
                    {React.createElement(labels[x.status], { className: "mx-auto" })}
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

const labels: { [key in AttendanceType]: LucideIcon} = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  EXCUSED: Annoyed,
  NOT_EXCUSED: X,
}
function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'EXCUSED', 'NOT_EXCUSED', 'UNKNOWN'].includes(x);
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
      {Object.entries(labels).map(([key, label]) => (
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
