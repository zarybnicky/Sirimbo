import { RenderListItem } from '@app/ui/generic/AdminEntityList';
import React from 'react';
import { EventListDocument } from '@app/graphql/Event';
import { NextRouter, useRouter } from 'next/router';
import { useFuzzySearch } from './use-fuzzy-search';
import { useQuery } from 'urql';
import { buttonCls } from './style/button';
import { TextField } from './fields/text';
import { Virtuoso } from 'react-virtuoso';
import Link from 'next/link';
import { Plus } from 'lucide-react';
import { fullDateFormatter } from './format-date';
import { SubmitButton } from './submit';

export function EventList() {
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
      subtitle: x.eventInstancesList.map((x) =>
        fullDateFormatter.formatRange(new Date(x.since), new Date(x.until)),
      )?.[0],
      href: `/akce/${x.id}`,
    }));
  }, [data]);
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>
        <Link
          href="/platby/structure/group/add"
          className={buttonCls({
            size: 'sm',
            variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
          })}
        >
          <Plus />
          Vytvořit
        </Link>

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
