import { EventListDocument } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { datetimeRangeToTimeRange, fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { add, endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { UpsertEventForm } from '../event-form/UpsertEventForm';
import Link from "next/link";
import { buttonCls } from "@/ui/style";
import { cn } from "@/ui/cn";

const QueryParams = z.object({
  id: zRouterId,
});

interface EventNode {
  id: string;
  title: string;
  date: string;
  subtitle: string;
  href: {
    pathname: string;
    query: { id: string };
  };
}

interface EventListPageProps {
  cursor?: number;
  search: string;
  onLoadMore?: (cursor: number) => void;
  currentId?: string;
}

function EventListPage({ cursor, search, onLoadMore, currentId }: EventListPageProps) {
  const [{ data }] = useQuery({
    query: EventListDocument,
    variables: { first: 50, cursor },
  });

  const hasMore = !!data?.events?.pageInfo.hasNextPage;

  const nodes: EventNode[] = React.useMemo(() => {
    return (data?.events?.edges || []).map(({ node: x }) => {
      let closestInstance = x.eventInstancesList[0];
      const refDate = Date.now();
      for (const instance of x.eventInstancesList) {
        const intervalA = new Date(closestInstance!.since).getTime() - refDate;
        const intervalB = new Date(instance.since).getTime() - refDate;
        if ((intervalA < 0 && intervalB > intervalA)) {
          closestInstance = instance;
        }
      }

      return {
        id: x.id,
        title: x.name,
        date: closestInstance?.since || '',
        subtitle: [
          closestInstance ?
            fullDateFormatter.formatRange(new Date(closestInstance.since), new Date(closestInstance.until)) : '',
          x.location?.name,
          x.locationText,
          (x.capacity ?? 0) > 0 ? `Zbývá ${x.remainingPersonSpots} míst z ${x.capacity}` : '',
        ].filter(Boolean).join(', '),
        href: {
          pathname: '/akce/[id]',
          query: { id: x.id },
        },
      };
    }).sort((a, b) => b.date?.localeCompare(a.date));
  }, [data]);

  const fuzzy: EventNode[] = useFuzzySearch(nodes, ['id', 'title'], search);

  const handleLoadMore = React.useCallback(() => {
    const endCursor = data?.events?.pageInfo?.endCursor;
    if (endCursor && onLoadMore) {
      onLoadMore(endCursor);
    }
  }, [data, onLoadMore]);

  return (
    <div className="flex flex-col min-h-16">
      {fuzzy.map((item) => (
        <Link
          key={item.id}
          href={item.href}
          className={buttonCls({ variant: currentId === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
        >
          <div>{item.title}</div>
          <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
            {item.subtitle}
          </div>
        </Link>
      ))}

      {(hasMore && onLoadMore) && (
        <div className="p-2 flex justify-center items-center h-16">
          <SubmitButton type="button" onClick={handleLoadMore}>
            Načíst starší...
          </SubmitButton>
        </div>
      )}
    </div>
  );
}

export function EventList() {
  const [pages, setPages] = React.useState<(number | undefined)[]>([undefined]);
  const [search, setSearch] = React.useState('');
  const { query: { id: currentId } } = useTypedRouter(QueryParams);
  const auth = useAuth();

  const handleLoadMore = React.useCallback((cursor: number) => {
    setPages((xs) => [...xs, cursor]);
  }, []);

  const emptyEvent = React.useMemo(() => {
    const day = startOf(endOf(new Date(), 'week', 1), 'day');
    return {
      instances: [{
        ...datetimeRangeToTimeRange(
          add(day, 9, 'hours'),
          add(day, 17, 'hours'),
        ),
        isCancelled: false,
      }],
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
              <UpsertEventForm initialValue={emptyEvent} />
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
        {pages.map((cursor, i) => (
          <EventListPage
            key={cursor}
            cursor={cursor}
            search={search}
            onLoadMore={(i === pages.length - 1) ? handleLoadMore : undefined}
            currentId={currentId}
          />
        ))}
      </div>
    </div>
  );
}

const preventDefault = (e: Event) => e.preventDefault();
