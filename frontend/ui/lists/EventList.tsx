'use client';

import { ListItem } from '@/ui/ListItem';

import { EventListDocument } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { endOf, startOf } from 'date-arithmetic';
import { useSearchParams } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { CalendarDays } from 'lucide-react';

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
      condition: { type: 'CAMP', parentId: null },
      orderBy: ['SINCE_DESC'],
      includeCoverImage: true,
    },
  });

  const nodes: EventNode[] = React.useMemo(() => {
    return (data?.eventsList ?? []).map((event) => {
      return {
        id: event.id,
        title: event.name || formatEventType(event.type),
        date: event.since,
        subtitle: [
          fullDateFormatter.formatRange(new Date(event.since), new Date(event.until)),
          event.location?.name,
          event.locationText,
          (event.capacity ?? 0) > 0
            ? `Zbývá ${event.registrationInfo?.remainingCapacity} míst z ${event.capacity}`
            : '',
        ]
          .filter(Boolean)
          .join(', '),
        href: `/termin/${event.id}`,
        imageUrl: event.coverImage?.url ?? null,
      };
    });
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

  const [createDate] = React.useState(() => startOf(endOf(new Date(), 'week', 1), 'day'));

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {auth.isTrainer && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" text="Přidat událost" />
            <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
              <CreateEventForm type="CAMP" date={createDate} trainerPersonIds={[]} />
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
