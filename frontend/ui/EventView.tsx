import type { AttendanceType } from '@/graphql';
import {
  type EventAttendanceSummaryFragment,
  type EventFragment,
  type EventFullFragment,
  EventPaymentsDocument,
  type EventRegistrationsFragment,
} from '@/graphql/Event';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { RichTextView } from '@/ui/RichTextView';
import { Spinner } from '@/ui/Spinner';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import {
  formatDefaultEventName,
  formatLongCoupleName,
  formatOpenDateRange,
  fullDateFormatter,
  moneyFormatter,
} from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import { Check, HelpCircle, type LucideIcon, OctagonMinus, X } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { StringParam, useQueryParam } from 'use-query-params';
import { isTruthy } from '@/lib/truthyFilter';
import { useActionMap, useActions } from '@/lib/actions';
import { eventActions, eventExternalRegistrationActions } from '@/lib/actions/event';
import { paymentActions } from '@/lib/actions/payment';
import { ActionGroup } from '@/ui/ActionGroup';

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus,
};

export function EventView({ event }: { event: EventFullFragment }) {
  const auth = useAuth();
  const [variant, setVariant] = useQueryParam('tab', StringParam);
  const actions = useActions(eventActions, event);

  const tabs = React.useMemo(() => {
    const tabs: {
      id: string;
      title: React.ReactNode;
      contents: () => React.ReactNode;
    }[] = [];
    if (!event) return [];

    if (event.description) {
      tabs.push({
        id: 'info',
        title: 'Informace',
        contents: () => <RichTextView value={event.description} />,
      });
    }

    const numRegistrations =
      (event.eventRegistrationsList.length ?? 0) +
      (event.eventExternalRegistrationsList.length ?? 0);
    if (auth.user?.id && numRegistrations > 0) {
      tabs.push({
        id: 'registrations',
        title: `Přihlášky (${numRegistrations})`,
        contents: () => <Registrations event={event} />,
      });
    }

    if (auth.isTrainerOrAdmin) {
      tabs.push(
        {
          id: 'attendance',
          title: 'Účast',
          contents: () => <Attendance event={event} />,
        },
        {
          id: 'payments',
          title: 'Platby',
          contents: () => <Payments event={event} />,
        },
        {
          id: 'instances',
          title: 'Termíny',
          contents: () => <EventInstances event={event} />,
        },
      );
    }
    return tabs;
  }, [auth.isTrainerOrAdmin, auth.user?.id, event]);

  if (!event) return null;

  return (
    <>
      <PageHeader title={event.name || formatDefaultEventName(event)} actions={actions} />

      <BasicEventInfo event={event} />

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </>
  );
}

function Attendance({
  event,
}: {
  event: EventAttendanceSummaryFragment & EventFragment;
}) {
  return (
    <table className="prose prose-accent max-w-full">
      <thead>
        <tr>
          <th />
          {Object.entries(labels)
            .filter(([k]) => k !== 'CANCELLED')
            .map(([k, x]) => (
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
              <Link
                href={{
                  pathname: '/akce/[id]/termin/[instance]',
                  query: { id: event.id, instance: instance.id },
                }}
              >
                {fullDateFormatter.formatRange(
                  new Date(instance.since),
                  new Date(instance.until),
                )}
              </Link>
            </td>
            {Object.keys(labels)
              .filter((x) => x !== 'CANCELLED')
              .map((status) => (
                <td className="text-center" key={status}>
                  {instance.attendanceSummaryList?.find((x) => x?.status === status)
                    ?.count ?? 0}
                </td>
              ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
}

function EventInstances({ event }: { event: EventFullFragment }) {
  return (
    <table className="prose prose-accent max-w-none">
      <thead>
        <tr>
          <th scope="col" className="text-left">
            Termín
          </th>
          <th scope="col" className="text-left">
            Trenéři
          </th>
        </tr>
      </thead>
      <tbody>
        {event.eventInstancesList.map((instance) => {
          return (
            <tr key={instance.id} className={instance.isCancelled ? 'opacity-50' : ''}>
              <td>
                <Link
                  href={{
                    pathname: '/akce/[id]/termin/[instance]',
                    query: { id: event.id, instance: instance.id },
                  }}
                  className={instance.isCancelled ? 'line-through' : ''}
                >
                  {formatOpenDateRange(instance)}
                </Link>
                {instance.isCancelled && (
                  <div className="text-sm text-neutral-10">Zrušeno</div>
                )}
              </td>
              <td>
                {instance.trainersList
                  ?.map((x) => x.person?.name)
                  .filter(isTruthy)
                  .join(', ') ?? '—'}
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

function Registrations({ event }: { event: EventFragment & EventRegistrationsFragment }) {
  const externalRegistrationActionMap = useActionMap(
    eventExternalRegistrationActions,
    event.eventExternalRegistrationsList ?? [],
  );

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
      {event.eventExternalRegistrationsList?.map((x) => (
        <div key={x.id} className="p-1">
          <div className="flex gap-2 items-center justify-between">
            <div>
              {x.prefixTitle} {x.firstName} {x.lastName} {x.suffixTitle}
            </div>
            <ActionGroup
              variant="row"
              actions={externalRegistrationActionMap.get(x.id)!}
            />
          </div>
          {x.note && <div className="ml-3">{x.note}</div>}
        </div>
      ))}
    </div>
  );
}

function Payments({ event }: { event: EventFragment }) {
  const [{ data, fetching }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id: event.id },
  });

  const payments = (data?.event?.eventInstancesList || []).flatMap((registration) =>
    registration.paymentsList.flatMap((payment) =>
      payment.transactions.nodes.map((trans) => [registration, payment, trans] as const),
    ),
  );
  const actionMap = useActionMap(
    paymentActions,
    data?.event?.eventInstancesList.flatMap((x) => x.paymentsList) ?? [],
  );

  if (fetching && !data) {
    return (
      <div className="flex justify-center py-8">
        <Spinner />
      </div>
    );
  }

  return (
    <div className="prose prose-accent">
      {payments.map(([registration, payment, transaction]) => (
        <div key={transaction.id}>
          <div className="flex gap-2 items-center">
            <ActionGroup variant="row" actions={actionMap.get(payment.id)!} />
            Za lekci {fullDateFormatter.format(new Date(registration.since))}
          </div>
          <ul>
            {transaction.postingsList.map((posting) => (
              <li key={posting.id}>
                {moneyFormatter.format({ amount: posting.amount, currency: 'CZK' })}
                {' - '}
                {posting.account?.person?.name ||
                  (posting.account?.tenantId ? 'Klub' : '-')}
              </li>
            ))}
          </ul>
        </div>
      ))}
    </div>
  );
}
