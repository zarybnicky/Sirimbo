import { TextField } from '@/ui/fields/text';
import { useAuth } from '@/ui/use-auth';
import { AnnouncementListDocument } from '@/graphql/Announcement';
import { CohortColorBoxes } from '@/ui/CohortColorBox';
import { fullDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { cn } from '@/ui/cn';
import { SubmitButton } from '@/ui/submit';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { useFuzzySearch } from "@/ui/use-fuzzy-search";

const QueryParams = z.object({
  id: zRouterId,
});

export function AnnouncementList() {
  const router = useTypedRouter(QueryParams);
  const auth = useAuth();
  const [search, setSearch] = React.useState('');
  const [pages, setPages] = React.useState<(number | undefined)[]>([undefined]);

  const handleLoadMore = React.useCallback((endCursor: number) => {
    setPages(xs => [...xs, endCursor]);
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

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        {pages.map((cursor, index) => (
          <AnnouncementListPage
            key={cursor}
            cursor={cursor}
            currentId={router.query.id}
            search={search}
            onLoadMore={(index === pages.length - 1) ? handleLoadMore : undefined}
          />
        ))}
      </div>
    </div>
  );
}

export interface AnnouncementListPageProps {
  cursor?: number;
  search: string;
  currentId?: string;
  onLoadMore?: (endCursor: number) => void;
}

export function AnnouncementListPage({ cursor, search, currentId, onLoadMore }: AnnouncementListPageProps) {
  const [{ data, fetching }] = useQuery({
    query: AnnouncementListDocument,
    variables: { first: 50, cursor },
  });

  const handleLoadMore = React.useCallback(() => {
    const endCursor = data?.upozornenis?.pageInfo?.endCursor;
    if (endCursor && onLoadMore) {
      onLoadMore(endCursor);
    }
  }, [data, onLoadMore]);

  const hasMore = !!data?.upozornenis?.pageInfo?.hasNextPage;

  const nodes = React.useMemo(() => {
    return (data?.upozornenis?.nodes || []).map((item) => ({
      id: item.id,
      title: item.upNadpis,
      subtitle: (
        <div className="flex flex-wrap justify-between items-baseline gap-4">
          <div>
            {[
              item.userByUpKdo &&
              `${item.userByUpKdo.uJmeno} ${item.userByUpKdo.uPrijmeni}`,
              fullDateFormatter.format(new Date(item.upTimestampAdd)),
            ]
              .filter(Boolean)
              .join(', ')}
          </div>
          <CohortColorBoxes
            items={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
              (x) => x.cohortByUpsIdSkupina,
            )}
          />
        </div>
      ),
      href: {
        pathname: '/nastenka/[id]',
        query: { id: item.id },
      },
    }));
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
            className: 'pl-5 m-1 mt-0 grid'
          })}
        >
          <div>{item.title}</div>
          <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
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
