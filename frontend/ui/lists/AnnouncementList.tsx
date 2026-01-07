import { TextField } from '@/ui/fields/text';
import { useAuth } from '@/ui/use-auth';
import {
  AnnouncementListDocument,
  AnnouncementListQueryVariables,
} from '@/graphql/Announcement';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { buttonCls } from '@/ui/style';
import { cn } from '@/ui/cn';
import { SubmitButton } from '@/ui/submit';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { AnnouncementSortControls, type SortOption } from '@/ui/Announcements';

const QueryParams = z.object({
  id: zRouterId,
});

export function AnnouncementList() {
  const router = useTypedRouter(QueryParams);
  const auth = useAuth();
  const [search, setSearch] = React.useState('');
  const [pages, setPages] = React.useState<(number | undefined)[]>([undefined]);
  const [sort, setSort] = React.useState<SortOption>('created');

  const orderBy = React.useMemo<AnnouncementListQueryVariables['orderBy']>(() => {
    return sort === 'created' ? 'CREATED_AT_DESC' : 'UPDATED_AT_DESC';
  }, [sort]);

  React.useEffect(() => {
    setPages([undefined]);
  }, [sort]);
  const handleLoadMore = React.useCallback((endCursor: number) => {
    setPages((xs) => [...xs, endCursor]);
  }, []);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Nástěnka</div>

        {auth.isAdmin && (
          <Link
            href="/nastenka/add"
            className={buttonCls({
              size: 'sm',
              variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
            })}
          >
            Přidat příspěvek
          </Link>
        )}

        <div className="flex flex-col gap-2 w-full mt-2">
          <TextField
            type="search"
            className="w-full"
            placeholder="Vyhledat..."
            value={search}
            onChange={(e) => setSearch(e.currentTarget.value)}
          />
          <AnnouncementSortControls sort={sort} onChange={setSort} />
        </div>
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        {pages.map((cursor, index) => (
          <AnnouncementListPage
            key={cursor}
            cursor={cursor}
            currentId={router.query.id}
            search={search}
            orderBy={orderBy}
            onLoadMore={index === pages.length - 1 ? handleLoadMore : undefined}
          />
        ))}
      </div>
    </div>
  );
}

interface AnnouncementListPageProps {
  cursor?: number;
  search: string;
  currentId?: string;
  orderBy: AnnouncementListQueryVariables['orderBy'];
  onLoadMore?: (endCursor: number) => void;
}

function AnnouncementListPage({
  cursor,
  search,
  currentId,
  orderBy,
  onLoadMore,
}: AnnouncementListPageProps) {
  const [{ data, fetching }] = useQuery({
    query: AnnouncementListDocument,
    variables: { first: 50, cursor, orderBy },
  });

  const handleLoadMore = React.useCallback(() => {
    const endCursor = data?.announcements?.pageInfo?.endCursor;
    if (endCursor && onLoadMore) {
      onLoadMore(endCursor);
    }
  }, [data, onLoadMore]);

  const hasMore = !!data?.announcements?.pageInfo.hasNextPage;

  const nodes = React.useMemo(() => {
    return (data?.announcements?.nodes || []).map((item) => {
      const authorName = item.author
        ? [item.author.uJmeno, item.author.uPrijmeni].filter(Boolean).join(' ')
        : undefined;

      return {
        id: item.id,
        title: item.title,
        subtitle: (
          <div className="flex flex-wrap justify-between items-baseline gap-4">
            <div className="flex flex-col gap-1">
              {authorName && <div>{authorName}</div>}
              <div className="flex items-center gap-1 text-xs text-neutral-11">
                <time
                  dateTime={item.createdAt}
                  title={numericFullFormatter.format(new Date(item.createdAt))}
                >
                  {numericDateWithYearFormatter.format(new Date(item.createdAt))}
                </time>
                {item.updatedAt !== null && (
                  <>
                    <span>-</span>
                    <time
                      dateTime={item.updatedAt}
                      title={numericFullFormatter.format(new Date(item.updatedAt))}
                    >
                      Upraveno
                    </time>
                  </>
                )}
              </div>
            </div>
            <AnnouncementAudienceBadges audiences={item.announcementAudiences.nodes} />
          </div>
        ),
        href: {
          pathname: '/nastenka/[id]',
          query: { id: item.id },
        },
      };
    });
  }, [data]);

  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  if (!data && fetching) {
    return <div className="p-4 text-center">Loading...</div>;
  }

  return (
    <>
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
      {hasMore && onLoadMore && (
        <div className="p-2 flex justify-center">
          <SubmitButton type="button" disabled={fetching} onClick={handleLoadMore}>
            Načíst starší...
          </SubmitButton>
        </div>
      )}
    </>
  );
}
