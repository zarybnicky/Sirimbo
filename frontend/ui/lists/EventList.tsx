'use client';

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
import Link from 'next/link';
import { buttonCls } from '@/ui/style';
import { cn } from '@/lib/cn';
import Image from '@/ui/Image';
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
  const [{ data }] = useQuery({ query: EventListDocument });

  const nodes: EventNode[] = React.useMemo(() => {
    return (data?.eventsList ?? []).map((instance) => {
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
            ? `Zbývá ${instance.registrationInfo?.remainingCapacity} míst z ${instance.capacity}`
            : '',
        ]
          .filter(Boolean)
          .join(', '),
        href: `/termin/${instance.id}`,
        imageUrl: instance.coverImage?.url ?? null,
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
            size: 'none',
            className: 'm-1 mt-0 flex items-center gap-2 rounded-xl p-1.5 px-2 text-sm',
          })}
        >
          <span className="flex size-10 shrink-0 items-center justify-center overflow-hidden rounded-md bg-neutral-3 text-neutral-10">
            {item.imageUrl ? (
              <Image src={item.imageUrl} alt="" width={40} height={40} className="size-10 object-cover" />
            ) : (
              <CalendarDays className="size-5" aria-hidden="true" />
            )}
          </span>
          <div className="min-w-0 flex-1">
            <div>{item.title}</div>
            <div
              className={cn(
                'text-sm',
                currentId === item.id ? 'text-white' : 'text-neutral-11',
              )}
            >
              {item.subtitle}
            </div>
          </div>
        </Link>
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
