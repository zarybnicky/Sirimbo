import type { AttendanceType } from '@/graphql';
import {
  EventDocument,
  type EventAttendanceSummaryFragment,
  EventPaymentsDocument,
  type EventFragment,
  type EventRegistrationsFragment,
} from '@/graphql/Event';
import { DeletePaymentDocument } from '@/graphql/Payment';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { RichTextView } from '@/ui/RichTextView';
import { TabMenu } from '@/ui/TabMenu';
import { TitleBar } from '@/ui/TitleBar';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import {
  formatDefaultEventName,
  formatLongCoupleName,
  fullDateFormatter,
  moneyFormatter,
} from '@/ui/format';
import { EventMenu } from '@/ui/menus/EventMenu';
import { useAuth } from '@/ui/use-auth';
import { Annoyed, Check, HelpCircle, type LucideIcon, OctagonMinus, X } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';
import { StringParam, useQueryParam } from 'use-query-params';

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  EXCUSED: Annoyed,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus
};

export function EventView({ id }: { id: string }) {
  const auth = useAuth();
  const [variant, setVariant] = useQueryParam('tab', StringParam);

  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;

  const tabs = React.useMemo(() => {
    const tabs: {
      id: string;
      title: React.ReactNode;
      contents: () => React.ReactNode;
    }[] = [];
    if (!event) return [];

    if (event.description || (auth.user?.id && event.descriptionMember)) {
      tabs.push({
        id: 'info',
        title: 'Informace',
        contents: () => <EventInfo event={event} />,
      });
    }
    if (auth.user?.id && (event.eventRegistrationsList?.length ?? 0) > 0) {
      tabs.push({
        id: 'registrations',
        title: `Přihlášky (${event.eventRegistrationsList.length ?? 0})`,
        contents: () => <Registrations event={event} />,
      });

      tabs.push({
        id: 'attendance',
        title: 'Účast',
        contents: () => <Attendance event={event} />,
      });
    }

    if (auth.isTrainerOrAdmin) {
      tabs.push({
        id: 'payments',
        title: 'Platby',
        contents: () => <Payments event={event} />,
      });
    }
    return tabs;
  }, [auth.isTrainerOrAdmin, auth.user?.id, event]);

  if (!event) return null;

  return (
    <>
      <TitleBar title={event.name || formatDefaultEventName(event)}>
        <EventMenu align="end" data={event}>
          <DropdownMenuTrigger.CornerDots />
        </EventMenu>
      </TitleBar>

      <BasicEventInfo event={event} />

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </>
  );
}

function EventInfo({ event }: { event: EventFragment }) {
  const auth = useAuth();
  return (
    <div>
      <RichTextView value={event.description} />
      {!!auth.user && <RichTextView value={event.descriptionMember} />}
    </div>
  );
}

function Attendance({
  event,
}: {
  event: EventAttendanceSummaryFragment & EventFragment;
}) {
  return (
    <div className="prose prose-accent">
      <table>
        <thead>
          <tr>
            <th />
            {Object.entries(labels).filter(([k]) => k !== 'CANCELLED').map(([k, x]) => (
              <th className="text-center" key={k}>
                {React.createElement(x, { className: 'inline-block' })}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {event.eventInstancesList.map((instance) => (
            <tr key={instance.id}>
              <td>
                <Link href={`/akce/${event.id}/termin/${instance.id}`}>
                  {fullDateFormatter.formatRange(
                    new Date(instance.since),
                    new Date(instance.until),
                  )}
                </Link>
              </td>
              {Object.keys(labels).filter(x => x !== 'CANCELLED').map((status) => (
                <td className="text-center" key={status}>
                  {instance.attendanceSummaryList?.find((x) => x?.status === status)
                    ?.count ?? 0}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function Registrations({ event }: { event: EventFragment & EventRegistrationsFragment }) {
  return (
    <div>
      {event.eventRegistrationsList?.map((x) => (
        <div key={x.id} className="p-1">
          <div>{x.person ? x.person.name || '' : formatLongCoupleName(x.couple)}</div>
          {(x.note || x.eventLessonDemandsByRegistrationIdList) && (
            <div className="ml-3">
              {x.eventLessonDemandsByRegistrationIdList.map((x) => (
                <div key={x.id}>
                  {x.lessonCount}x{' '}
                  {event.eventTrainersList.find((y) => y.id === x.trainerId)?.name}
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

function PaymentMenu({ id, children }: { id: string; children: React.ReactNode }) {
  const doDelete = useMutation(DeletePaymentDocument)[1];
  const onDelete = React.useCallback(() => doDelete({ id }), [id, doDelete]);
  return (
    <DropdownMenu>
      {children}
      <DropdownMenuContent>
        <DropdownMenuButton onClick={onDelete}>Smazat platbu</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function Payments({ event }: { event: EventFragment }) {
  const [{ data }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id: event.id },
  });

  return (
    <div>
      {data?.event?.eventInstancesList.map((reg) =>
        reg.paymentsList.flatMap((payment) => (
          <div key={payment.id} className="prose">
            {payment.transactions.nodes.map((transaction) => (
              <div key={transaction.id}>
                <div className="flex gap-2 items-center">
                  <PaymentMenu id={payment.id}>
                    <DropdownMenuTrigger.RowDots />
                  </PaymentMenu>
                  Za lekci {fullDateFormatter.format(new Date(reg.since))}
                </div>
                <ul>
                  {transaction.postingsList.map((posting) => (
                    <li key={posting.id}>
                      {moneyFormatter.format(posting.amount)}
                      {' - '}
                      {posting.account?.person?.name || posting.account?.tenant?.name}
                    </li>
                  ))}
                </ul>
              </div>
            ))}
          </div>
        )),
      )}
    </div>
  );
}
