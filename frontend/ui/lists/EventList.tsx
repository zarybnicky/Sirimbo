import { EventListDocument } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { add, endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { QuickEventCreateForm } from '@/ui/event-form/QuickEventForms';
import type { QuickEventCreateDefaults } from '@/calendar/quickEventDefaults';
import Link from 'next/link';
import { buttonCls } from '@/ui/style';
import { cn } from '@/lib/cn';

const QueryParams = z.object({
  instance: zRouterId,
});

interface EventNode {
  id: string;
  title: string;
  date: string;
  subtitle: string;
  href: string;
}

interface EventListPageProps {
  search: string;
  currentId?: string;
}

function EventListPage({ search, currentId }: EventListPageProps) {
  const [{ data }] = useQuery({ query: EventListDocument });

  const nodes: EventNode[] = React.useMemo(() => {
    return (data?.eventInstancesList ?? []).map((instance) => {
      return {
        id: instance.id,
        title: instance.name || formatEventType(instance.type),
        date: instance.since,
        subtitle: [
          fullDateFormatter.formatRange(
            new Date(instance.since),
            new Date(instance.until),
          ),
          instance.location?.name,
          instance.locationText,
          (instance.capacity ?? 0) > 0
            ? `Zbývá ${instance.remainingPersonSpots} míst z ${instance.capacity}`
            : '',
        ]
          .filter(Boolean)
          .join(', '),
        href: `/termin/${instance.id}`,
      };
    });
  }, [data]);

  const fuzzy: EventNode[] = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col min-h-16">
      {fuzzy.map((item) => (
        <Link
          key={item.id}
          href={item.href}
          className={buttonCls({
            variant: currentId === item.id ? 'primary' : 'outline',
            display: 'none',
            className: 'pl-5 m-1 mt-0 grid',
          })}
        >
          <div>{item.title}</div>
          <div
            className={cn(
              'text-sm',
              currentId === item.id ? 'text-white' : 'text-neutral-11',
            )}
          >
            {item.subtitle}
          </div>
        </Link>
      ))}
    </div>
  );
}

export function EventList() {
  const [search, setSearch] = React.useState('');
  const {
    query: { instance: currentId },
  } = useTypedRouter(QueryParams);
  const auth = useAuth();

  const createDefaults = React.useMemo<QuickEventCreateDefaults>(() => {
    const day = startOf(endOf(new Date(), 'week', 1), 'day');
    return {
      since: add(day, 9, 'hours'),
      until: add(day, 17, 'hours'),
      trainerPersonIds: [],
      locationId: null,
      locationText: '',
    };
  }, []);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {auth.isTrainerOrAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" text="Přidat událost" />
            <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
              <QuickEventCreateForm defaults={createDefaults} initialType="CAMP" />
            </DialogContent>
          </Dialog>
        )}

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        <EventListPage search={search} currentId={currentId} />
      </div>
    </div>
  );
}

const preventDefault = (e: Event) => e.preventDefault();
