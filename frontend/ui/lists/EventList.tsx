'use client';

import { EventListDocument } from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { getEntityLink } from '@/ui/EntityLink';
import { EventForm } from '@/ui/event-form/EventForm';
import { TextField } from '@/ui/fields/text';
import { formatEventName, fullDateFormatter } from '@/ui/format';
import { ListItem } from '@/ui/ListItem';
import { endOfWeek, startOfDay } from 'date-fns';
import { CalendarDays } from 'lucide-react';
import { useSearchParams } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';

interface EventNode {
  id: string;
  title: string;
  date: string;
  subtitle: string;
  href: string;
  imageUrl: string | null;
}

interface EventListPageProps {
  search: string;
  currentId?: string;
}

function EventListPage({ search, currentId }: EventListPageProps) {
  const [{ data }] = useQuery({
    query: EventListDocument,
    variables: {
      condition: { type: { eq: 'CAMP' }, parentId: null },
      orderBy: ['SINCE_DESC'],
      includeCoverImage: true,
    },
  });

  const nodes: EventNode[] = React.useMemo(() => {
    return (data?.eventsList ?? []).map((event) => ({
      id: event.id,
      title: formatEventName(event) ?? '',
      href: getEntityLink('event', event).href,
      date: event.since,
      subtitle: [
        fullDateFormatter.formatRange(new Date(event.since), new Date(event.until)),
        event.location?.name,
        event.locationText,
        (event.capacity ?? 0) > 0
          ? `Zbývá ${event.registrationInfo.remainingCapacity} míst z ${event.capacity}`
          : '',
      ]
        .filter(Boolean)
        .join(', '),
      imageUrl: event.coverImage?.url ?? null,
    }));
  }, [data]);

  const fuzzy: EventNode[] = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col min-h-16">
      {fuzzy.map((item) => (
        <ListItem
          key={item.id}
          href={item.href}
          title={item.title}
          subtitle={item.subtitle}
          selected={currentId === item.id}
          imageUrl={item.imageUrl}
          icon={<CalendarDays className="size-5" />}
          wrap
        />
      ))}
    </div>
  );
}

export function EventList() {
  const [search, setSearch] = React.useState('');
  const currentId = useSearchParams()?.get('instance') ?? undefined;
  const auth = useAuth();

  const [createDate] = React.useState(() =>
    startOfDay(endOfWeek(new Date(), { weekStartsOn: 1 })),
  );

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {auth.canCreateEvent && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" text="Přidat událost" />
            <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
              <EventForm type="CAMP" date={createDate} />
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
