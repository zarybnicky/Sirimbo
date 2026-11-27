import { ListItem } from '@/ui/ListItem';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { TextField } from '@/ui/fields/text';
import { useAuth } from '@/lib/auth';
import { AnnouncementsDocument, type AnnouncementStatus } from '@/graphql/Announcement';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { STATUS_LABEL } from '@/ui/AnnouncementShared';
import { buttonCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { Pin, Plus } from 'lucide-react';
import Link from 'next/link';
import { useParams, usePathname } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { AnnouncementSortControls, type SortOption } from '@/ui/Announcements';
import { AnnouncementsOrderBy } from '@/graphql';
import { DotSeparated } from '../DotSeparated';

const memberStatuses = [
  { id: null, label: 'Všechny' },
  { id: 'PUBLISHED', label: 'Aktuální' },
  { id: 'ARCHIVED', label: 'Archiv' },
];
const trainerStatuses = [
  { id: null, label: 'Všechny' },
  { id: 'DRAFT', label: 'Koncepty' },
  { id: 'SCHEDULED', label: 'Naplánované' },
  { id: 'PUBLISHED', label: 'Aktuální' },
  { id: 'ARCHIVED', label: 'Archiv' },
];

export function AnnouncementList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const pathname = usePathname() ?? '';
  const auth = useAuth();
  const [search, setSearch] = React.useState('');
  const [pages, setPages] = React.useState<(string | undefined)[]>([undefined]);
  const [sort, setSort] = React.useState<SortOption>('created');
  const [status, setStatus] = React.useState<AnnouncementStatus | null>('PUBLISHED');

  const orderBy = React.useMemo<AnnouncementsOrderBy>(() => {
    return sort === 'created' ? 'CREATED_AT_DESC' : 'UPDATED_AT_DESC';
  }, [sort]);

  const handleLoadMore = React.useCallback((endCursor: string) => {
    setPages((xs) => [...xs, endCursor]);
  }, []);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 my-2 flex gap-2 items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase flex-1">Nástěnka</div>

        {auth.canCreateAnnouncement && (
          <Link
            href="/nastenka/add"
            className={buttonCls({
              size: 'icon',
              variant: pathname.endsWith('/add') ? 'primary' : 'outline',
            })}
          >
            <Plus />
          </Link>
        )}
        <ComboboxButton
          value={status}
          placeholder="Všechny stavy"
          options={auth.canCreateAnnouncement ? trainerStatuses : memberStatuses}
          searchable={false}
          onChange={(value) => {
            setStatus((value ?? null) as AnnouncementStatus | null);
            setPages([undefined]);
          }}
        />
        <AnnouncementSortControls
          sort={sort}
          onChange={(value) => {
            setSort(value);
            setPages([undefined]);
          }}
        />

        <TextField
          type="search"
          className="w-full"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        {pages.map((cursor, index) => (
          <AnnouncementListPage
            key={cursor ?? status ?? 'all'}
            cursor={cursor}
            currentId={currentId}
            search={search}
            status={status}
            orderBy={orderBy}
            onLoadMore={index === pages.length - 1 ? handleLoadMore : undefined}
          />
        ))}
      </div>
    </div>
  );
}

interface AnnouncementListPageProps {
  cursor?: string;
  search: string;
  currentId?: string;
  status: AnnouncementStatus | null;
  orderBy: AnnouncementsOrderBy;
  onLoadMore?: (endCursor: string) => void;
}

function AnnouncementListPage({
  cursor,
  search,
  currentId,
  status,
  orderBy,
  onLoadMore,
}: AnnouncementListPageProps) {
  const [{ data, error, fetching }] = useQuery({
    query: AnnouncementsDocument,
    variables: { first: 50, cursor, orderBy, condition: status ? { status } : undefined },
  });

  const pageInfo = data?.announcements?.pageInfo;
  const items = useFuzzySearch(data?.announcements?.nodes ?? [], ['id', 'title'], search);

  if (!data && fetching) {
    return <div className="p-4 text-center">Načítám...</div>;
  }
  if (error) {
    return <p className="p-4 text-sm text-neutral-11">Příspěvky se nepodařilo načíst.</p>;
  }
  if (!cursor && data?.announcements?.totalCount === 0) {
    return <p className="p-4 text-sm text-neutral-11">Žádné příspěvky.</p>;
  }

  return (
    <>
      {items.map((item) => (
        <ListItem
          key={item.id}
          href={`/nastenka/${item.id}`}
          selected={currentId === item.id}
          wrap
          title={
            <div className="flex flex-wrap items-center gap-2 font-medium">
              {item.isSticky && (
                <span title="Stálý příspěvek">
                  <Pin className="size-3.5 rotate-45" />
                </span>
              )}
              <span>{item.title}</span>
            </div>
          }
          subtitle={
            <div className="flex flex-wrap justify-between items-baseline gap-4 text-xs">
              <DotSeparated>
                {item.authorName && <div>{item.authorName}</div>}
                <time
                  dateTime={item.createdAt}
                  title={numericFullFormatter.format(new Date(item.createdAt))}
                >
                  {numericDateWithYearFormatter.format(new Date(item.createdAt))}
                </time>
                {item.updatedAt !== null && (
                  <time
                    dateTime={item.updatedAt}
                    title={numericFullFormatter.format(new Date(item.updatedAt))}
                  >
                    Upraveno
                  </time>
                )}
                {STATUS_LABEL[item.status]}
              </DotSeparated>
              <AnnouncementAudienceBadges audiences={item.announcementAudiences.nodes} />
            </div>
          }
        />
      ))}
      {pageInfo?.hasNextPage && pageInfo.endCursor && onLoadMore && (
        <div className="p-2 flex justify-center">
          <SubmitButton
            type="button"
            disabled={fetching}
            onClick={() => onLoadMore(pageInfo.endCursor)}
          >
            Načíst starší...
          </SubmitButton>
        </div>
      )}
    </>
  );
}
