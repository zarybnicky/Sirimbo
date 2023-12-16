import React from 'react';
import { EventListDocument } from '@app/graphql/Event';
import { NextRouter, useRouter } from 'next/router';
import { useFuzzySearch } from './use-fuzzy-search';
import { useQuery } from 'urql';
import { TextField } from './fields/text';
import { Virtuoso } from 'react-virtuoso';
import { fullDateFormatter } from './format';
import { SubmitButton } from './submit';
import { buttonCls } from './style';
import Link from 'next/link';
import { fromSlugArray } from './slugify';
import { cn } from './cn';
import { UpsertEventButton } from './event-form/UpsertEventForm';
import { useAuth } from './use-auth';

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
    return (data?.events?.nodes || []).map((x) => ({
      id: x.id,
      title: x.name,
      date: x.eventInstancesList?.[0]?.since || '',
      subtitle: [
        x.eventInstancesList.map((x) =>
          fullDateFormatter.formatRange(new Date(x.since), new Date(x.until)),
        )?.[0],
        x.location?.name,
        x.locationText,
        (x.capacity ?? 0) > 0 ? `Zbývá ${x.remainingPersonSpots} míst z ${x.capacity}` : '',
      ].filter(Boolean).join(', '),
      href: `/akce/${x.id}`,
    })).sort((a, b) => b.date?.localeCompare(a.date));
  }, [data]);
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {perms.isAdmin && (
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
        context={{ router, loading: fetching, loadMore }}
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
  { router }: { router: NextRouter },
) {
  const id = fromSlugArray(router.query.id);
  return (
    <Link
      key={item.id}
      href={`/akce/${item.id}`}
      className={buttonCls({ variant: id === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
    >
      <div><b>{item.title}</b></div>
      <div className={cn('text-sm', id === item.id ? 'text-white' : 'text-neutral-11')}>
        {item.subtitle}
      </div>
      {item.children}
    </Link>
  );
}

type FooterContext = { router: NextRouter; loadMore: () => void; loading: boolean };
const Footer = ({ context }: { context?: FooterContext }) => {
  return (
    <div className="p-2 flex justify-center">
      <SubmitButton type="button" disabled={context?.loading} onClick={context?.loadMore}>
        Načíst starší...
      </SubmitButton>
    </div>
  );
};
