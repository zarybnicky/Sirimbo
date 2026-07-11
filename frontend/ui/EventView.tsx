import type { AttendanceType } from '@/graphql';
import {
  type EventFullFragment,
  type EventInstanceWithTrainerFragment,
} from '@/graphql/Event';
import { BasicEventInfo } from './BasicEventInfo';
import { EventInstancePayments } from '@/ui/EventInstancePayments';
import { EventInstanceRegistrations } from '@/ui/EventInstanceRegistrations';
import { RichTextView } from '@/ui/RichTextView';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import {
  formatDefaultInstanceName,
  formatEventType,
  formatOpenDateRange,
  fullDateFormatter,
} from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import { Check, HelpCircle, type LucideIcon, OctagonMinus, X } from 'lucide-react';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { isTruthy } from '@/lib/truthyFilter';
import { useActions } from '@/lib/actions';
import { eventActions } from '@/lib/actions/event';
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
    const compositionInstances: EventInstanceWithTrainerFragment[] = rootInstance
      ? [rootInstance, ...rootInstance.childEventInstancesList]
      : [];

    if (event.description) {
      tabs.push({
        id: 'info',
        title: 'Informace',
        contents: () => <RichTextView value={event.description} />,
      });
    }

    const numRegistrations = rootInstance
      ? rootInstance.registrations.totalCount +
        rootInstance.eventExternalRegistrationsByInstanceIdList.length
      : 0;
    if (auth.user?.id && numRegistrations > 0) {
      tabs.push({
        id: 'registrations',
        title: `Přihlášky (${numRegistrations})`,
        contents: () =>
          rootInstance && <EventInstanceRegistrations instance={rootInstance} />,
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
          contents: () => <Attendance instances={compositionInstances} />,
        },
        {
          id: 'payments',
          title: 'Platby',
          contents: () =>
            rootInstance && <EventInstancePayments instanceId={rootInstance.id} />,
        },
        {
          id: 'instances',
          title: 'Termíny',
          contents: () => <EventInstances instances={compositionInstances} />,
        },
      );
    }
    return tabs;
  }, [auth.isTrainerOrAdmin, auth.user?.id, event, rootInstance]);

  if (!event) return null;

  return (
    <>
      <PageHeader
        title={
          event.name ||
          (rootInstance && formatDefaultInstanceName(rootInstance)) ||
          formatEventType(event.type)
        }
        actions={actions}
      />

      {rootInstance && (
        <BasicEventInfo instance={rootInstance} />
      )}

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </>
  );
}

function Attendance({ instances }: { instances: EventInstanceWithTrainerFragment[] }) {
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
        {instances.map((instance) => {
          const stats =
            typeof instance.stats === 'string' ? JSON.parse(instance.stats) : instance.stats;

          return (
            <tr key={instance.id}>
              <td>
                <Link href={`/termin/${instance.id}`}>
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
                    {stats?.[status] ?? 0}
                  </td>
                ))}
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

function EventInstances({
  instances,
}: {
  instances: EventInstanceWithTrainerFragment[];
}) {
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
        {instances.map((instance) => {
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
