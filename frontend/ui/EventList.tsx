import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { EventListDocument } from '@app/graphql/Event';
import Link from 'next/link';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { z } from 'zod';
import { cn } from './cn';
import { UpsertEventButton } from './event-form/UpsertEventForm';
import { TextField } from './fields/text';
import { fullDateFormatter } from './format';
import { buttonCls } from './style';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';
import { useFuzzySearch } from './use-fuzzy-search';

const QueryParams = z.object({
  id: zRouterId,
});

export function EventList() {
  const { perms } = useAuth();
  const [cursor, setCursor] = React.useState<number | undefined>(undefined);
  const [{ data, fetching }] = useQuery({
    query: EventListDocument,
    variables: { first: 100, cursor },
  });
  const loadMore = React.useCallback(() => {
    const info = data?.events?.pageInfo;
    if (info?.endCursor) {
      setCursor(info.endCursor);
    }
  }, [data]);
  const hasMore = data?.events?.pageInfo.hasNextPage !== false;

  const nodes = React.useMemo(() => {
    return (data?.events?.nodes || []).map((x) => {
      let closestInstance = x.eventInstancesList[0];
      const refDate = new Date().getTime();
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
        href: `/akce/${x.id}`,
      };
    }).sort((a, b) => b.date?.localeCompare(a.date));
  }, [data]);
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {perms.isTrainerOrAdmin && (
          <UpsertEventButton />
        )}

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={RenderListItem}
        components={{ Footer: hasMore ? Footer : undefined }}
        context={{ currentId, loading: fetching, loadMore }}
      />
    </div>
  );
}

export function RenderListItem(
  _n: number,
  item: {
    id: string;
    href: string;
    title?: React.ReactNode;
    subtitle?: React.ReactNode;
    children?: React.ReactNode;
  },
  { currentId }: { currentId: string },
) {
  return (
    <Link
      key={item.id}
      href={`/akce/${item.id}`}
      className={buttonCls({ variant: currentId === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
    >
      <div><b>{item.title}</b></div>
      <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
        {item.subtitle}
      </div>
      {item.children}
    </Link>
  );
}

type FooterContext = { loadMore: () => void; loading: boolean };
const Footer = ({ context }: { context?: FooterContext }) => {
  return (
    <div className="p-2 flex justify-center">
      <SubmitButton type="button" disabled={context?.loading} onClick={context?.loadMore}>
        Načíst starší...
      </SubmitButton>
    </div>
  );
};
