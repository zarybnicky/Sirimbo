import { EventWithAttendanceFragment, EventWithRegistrantsFragment, UpdateAttendanceDocument, EventAttendanceFragment } from '@app/graphql/Event';
import { numericDateFormatter } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { useMutation } from 'urql';
import { AttendanceType } from '@/graphql';
import { PersonFragment } from '@/graphql/Person';
import { DropdownMenu, DropdownMenuTrigger } from './dropdown';
import { buttonCls } from './style';
import { Bed, Check, ChevronDown, HelpCircle, LucideIcon, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { DropdownMenuRadioGroup, DropdownMenuRadioItem } from '@radix-ui/react-dropdown-menu';
import * as DropdownMenuPrimitive from '@radix-ui/react-dropdown-menu';
import { cn } from './cn';

export function AttendanceView({ event }: { event: EventWithAttendanceFragment & EventWithRegistrantsFragment }) {
  const { perms } = useAuth();
  const isMyEvent = perms.isAdmin || (perms.isTrainer && event.eventTrainersList.find(x => perms.isCurrentPerson(x.person?.id || '')));

  const data = React.useMemo(() => {
    const data: Map<string, {
      person: PersonFragment;
      instances: { [key: string]: Omit<EventAttendanceFragment, 'id' | 'registrationId'> }
    }> = new Map();
    for (const instance of event.eventInstancesList) {
      for (const person of (event.registrantsList ?? [])) {
        if (!data.get(person.id)) {
          data.set(person.id, { person, instances: {} });
        }
        data.get(person.id)!.instances[instance.id] = {
          status: 'UNKNOWN',
          instanceId: instance.id,
          personId: person.id,
          note: null,
        };
      }
      for (const attendance of instance.eventAttendancesByInstanceIdList) {
        const person = data.get(attendance.personId);
        if (person) person.instances[instance.id] = attendance;
      }
    }
    return data;
  }, [event]);

  return (
    <div className="overflow-x-auto">
    <div className="prose prose-accent max-w-none">
      <table>
        <thead>
          <tr>
            <th></th>
            <th></th>
            {event.eventInstancesList.map((instance) => (
              <th className="text-center" key={instance.id}>
                {numericDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {[...data.values()].map(reg => (
            <tr key={reg.person.id}>
              <td className="whitespace-nowrap">{reg.person.lastName}</td>
              <td className="whitespace-nowrap">{reg.person.firstName}</td>
              {Object.entries(reg.instances).map(([instanceId, attendance]) => (
                isMyEvent ? (
                  <td className="text-center align-middle py-0">
                    <AttendanceItem key={instanceId} attendance={attendance} />
                  </td>
                ) : (
                  <td className="text-center align-middle">
                    {React.createElement(labels[attendance.status], { key: instanceId, className: "mx-auto" })}
                  </td>
                )
              ))}
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
  EXCUSED: Bed,
  NOT_EXCUSED: X,
  UNKNOWN: HelpCircle,
}
function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'EXCUSED', 'NOT_EXCUSED', 'UNKNOWN'].includes(x);
}

function AttendanceItem({ attendance }: { attendance: Partial<EventAttendanceFragment> }) {
  const status = attendance.status || 'UNKNOWN';
  const label = labels[status];
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
      })
    }
  });

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button type="button" className={buttonCls({ className: 'justify-between max-w-[10rem]', variant: 'outline' })}>
          {React.createElement(label)}
          <ChevronDown />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuPrimitive.Portal>
        <DropdownMenuPrimitive.Content
          className={cn(
            'bg-neutral-2 rounded-md p-[5px] z-30 flex flex-col',
            'shadow-[0px_10px_38px_-10px_rgba(22,_23,_24,_0.35),_0px_10px_20px_-15px_rgba(22,_23,_24,_0.2)] will-change-[opacity,transform]',
            'data-[side=top]:animate-slideDownAndFade data-[side=right]:animate-slideLeftAndFade data-[side=bottom]:animate-slideUpAndFade data-[side=left]:animate-slideRightAndFade',
          )}
        >
          <DropdownMenuPrimitive.Arrow className="fill-current text-neutral-0" />
          <DropdownMenuRadioGroup value={attendance.status} onValueChange={setStatus.execute}>
            {Object.entries(labels).map(([key, label]) => (
              <DropdownMenuRadioItem key={key} value={key} className={cn("flex justify-between p-1", key === attendance.status ? 'bg-accent-9 text-accent-0' : '')}>
                {React.createElement(label)}
              </DropdownMenuRadioItem>
            ))}
          </DropdownMenuRadioGroup>
        </DropdownMenuPrimitive.Content>
      </DropdownMenuPrimitive.Portal>
    </DropdownMenu>
  );
}
