import { AttendanceType } from '@/graphql';
import { EventDocument, EventFragment, EventRegistrationsFragment } from '@/graphql/Event';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { RichTextView } from '@/ui/RichTextView';
import { TabMenu } from '@/ui/TabMenu';
import { TitleBar } from '@/ui/TitleBar';
import { DropdownMenu, DropdownMenuTrigger } from '@/ui/dropdown';
import { formatDefaultEventName, formatLongCoupleName, fullDateFormatter } from '@/ui/format';
import { EventMenu } from '@/ui/menus/EventMenu';
import { useAuth } from '@/ui/use-auth';
import { Annoyed, Check, HelpCircle, LucideIcon, X } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { StringParam, useQueryParam } from 'use-query-params';

const labels: { [key in AttendanceType]: LucideIcon} = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  EXCUSED: Annoyed,
  NOT_EXCUSED: X,
}

export function EventView({ id }: { id: string }) {
  const auth = useAuth();
  const [variant, setVariant] = useQueryParam('tab', StringParam);

  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;

  if (!event) return null;

  const tabs = [];
  if (event.description || (auth.user && event.descriptionMember)) {
    tabs.push({
      id: 'info',
      label: 'Informace',
      contents: <EventInfo key="info" event={event} />
    });
  }
  if (auth.user && (event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'registrations',
      label: `Přihlášky (${event.eventRegistrationsList.length ?? 0})`,
      contents: <Registrations key="registrations" event={event} />
    });
  }
  if (auth.user && (event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'attendance',
      label: `Účast`,
      contents: <div className="prose prose-accent">
        <table>
          <thead>
            <tr>
              <th></th>
              {Object.values(labels).map((x, i) => (
                <th className="text-center" key={i}>
                  {React.createElement(x, {className: 'inline-block'})}
                </th>
              ))}
            </tr>
          </thead>
         <tbody>
           {event.eventInstancesList.map(instance => (
             <tr key={instance.id}>
               <td>
                 <Link href={`/akce/${event.id}/termin/${instance.id}`}>
                   {fullDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
                 </Link>
               </td>
               {Object.keys(labels).map((status) => (
                 <td className="text-center" key={status}>
                   {instance.attendanceSummaryList?.find(x => x?.status === status)?.count ?? 0}
                 </td>
               ))}
             </tr>
           ))}
         </tbody>
        </table>
      </div>
    });
  }

  return (
    <>
      <TitleBar title={event.name || formatDefaultEventName(event)}>
        {(auth.isAdmin || (auth.isTrainer && event.eventTrainersList.find(x => auth.personIds.some(id => id === x.personId)))) && (
          <DropdownMenu>
            <DropdownMenuTrigger.CornerDots />
            <EventMenu data={event} />
          </DropdownMenu>
        )}
      </TitleBar>

      <BasicEventInfo event={event} />

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4 relative max-w-full">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </>
  );
};

function EventInfo({ event }: { event: EventFragment }) {
  const auth = useAuth();
  return (
    <div>
      <RichTextView value={event.description} />
      {!!auth.user && <RichTextView value={event.descriptionMember} />}
    </div>
  );
}

function Registrations({ event }: { event: EventFragment & EventRegistrationsFragment; }) {
  return (
    <div>
      {event.eventRegistrationsList?.map((x) => (
        <div key={x.id} className="p-1">
          <div>{x.person ? x.person.name || '' : formatLongCoupleName(x.couple!)}</div>
          {(x.note || x.eventLessonDemandsByRegistrationIdList) && (
            <div className="ml-3">
              {x.eventLessonDemandsByRegistrationIdList.map(x => (
                <div key={x.id}>
                  {x.lessonCount}x {event.eventTrainersList.find(y => y.id === x.trainerId)?.name}
                </div>
              ))}
              {x.note}
            </div>
          )}
        </div>
      ))}
    </div>
  );
}
