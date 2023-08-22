import { AnnouncementListDocument } from '@app/graphql/Announcement';
import { CohortColorBoxes } from '@app/ui/CohortColorBox';
import { fullDateFormatter } from '@app/ui/format-date';
import { TextField } from '@app/ui/fields/text';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import Link from 'next/link';
import { NextRouter, useRouter } from 'next/router';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { RenderListItem } from './generic/AdminEntityList';
import { buttonCls } from './style/button';
import { useAuth } from './use-auth';
import { SubmitButton } from './submit';

export function AnnouncementList() {
  const router = useRouter();
  const { perms } = useAuth();

  const [cursor, setCursor] = React.useState<number | undefined>(undefined);
  const [{ data, fetching }] = useQuery({
    query: AnnouncementListDocument,
    variables: { first: 100, cursor },
  });
  const loadMore = React.useCallback(() => {
    const info = data?.upozornenis?.pageInfo;
    if (info?.endCursor) {
      setCursor(info.endCursor as number);
    }
  }, [data]);
  const hasMore = data?.upozornenis?.pageInfo?.hasNextPage !== false;

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
              (x) => x.skupinyByUpsIdSkupina,
            )}
          />
        </div>
      ),
      href: `/nastenka/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Platby</div>

        {perms.isAdmin && (
          <Link
            href="/platby/structure/category/add"
            className={buttonCls({
              size: 'sm',
              variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
            })}
          >
            Přidat platbu
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

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={RenderListItem}
        components={{ Footer: hasMore ? Footer : undefined }}
        context={{ router, loading: fetching, loadMore }}
      />
    </>
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
