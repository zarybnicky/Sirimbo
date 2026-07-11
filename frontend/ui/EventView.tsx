import type { AttendanceType } from '@/graphql';
import {
  type EventAttendanceSummaryFragment,
  type EventFragment,
  type EventFullFragment,
  EventPaymentsDocument,
  type EventRegistrationsFragment,
} from '@/graphql/Event';
import { BasicEventInfo } from './BasicEventInfo';
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
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';
import { isTruthy } from '@/lib/truthyFilter';
import { useActionMap, useActions } from '@/lib/actions';
import { eventActions, eventExternalRegistrationActions } from '@/lib/actions/event';
import { paymentActions } from '@/lib/actions/payment';
import { ActionRow } from '@/ui/ActionRow';
import { Calendar } from '@/calendar/Calendar';

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus,
};

export function EventView({ event }: { event: EventFullFragment }) {
  const auth = useAuth();
  const rootInstance =
    event.eventInstancesList.find((instance) => !instance.parentId) ??
    event.eventInstancesList[0];
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
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

    if (event.type === 'CAMP') {
      const parentId = rootInstance?.id;
      if (parentId) {
        tabs.push({
          id: 'schedule',
          title: 'Program',
          contents: () => <Calendar parentId={parentId} initialDate={rootInstance.since} />,
        });
      }
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
  }, [auth.isTrainerOrAdmin, auth.user?.id, event, rootInstance]);

  if (!event) return null;

  return (
    <>
      <PageHeader title={event.name || formatDefaultEventName(event)} actions={actions} />

      {rootInstance && (
        <BasicEventInfo instance={rootInstance} />
      )}

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
    <table className="prose prose-accent max-w-none">
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
                href={`/termin/${instance.id}`}
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
                  {JSON.parse(instance.stats)[status] ?? 0}
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
    <table className="prose prose-accent w-full">
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
                  href={`/termin/${instance.id}`}
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
              {x.eventLessonDemandsByRegistrationIdList.map((demand) => (
                <div key={demand.id}>
                  {demand.lessonCount}x {demand.trainer?.person?.name}
                </div>
              ))}
              {x.note}
            </div>
          )}
        </div>
      ))}
      {event.eventExternalRegistrationsList?.map((x) => (
        <ActionRow key={x.id} actions={externalRegistrationActionMap.get(x.id)!}>
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <div>
              {x.prefixTitle} {x.firstName} {x.lastName} {x.suffixTitle}
            </div>
            {x.note && <div className="ml-3">{x.note}</div>}
          </div>
        </ActionRow>
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
          <ActionRow actions={actionMap.get(payment.id)!} className="mb-0">
            Za lekci {fullDateFormatter.format(new Date(registration.since))}
          </ActionRow>
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
