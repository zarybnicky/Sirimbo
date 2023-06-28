import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { List } from '@app/ui/List';
import { TextField } from '@app/ui/fields/text';
import { fromSlugArray } from '@app/ui/slugify';
import { useRouter } from 'next/router';
import React, { ReactNode } from 'react';
import { Plus } from 'lucide-react';
import { Virtuoso } from 'react-virtuoso';
import { SubmitButton } from '@app/ui/submit';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { NextRouter } from 'next/router';
import { LinkProps } from 'next/link';
import { useQuery } from 'urql';

type Route = LinkProps['href'];

export interface AdminEntity {
  name: (num: number) => string;
  listRoute: Route;
  addRoute: Route;
  editRoute: (id: string) => Route;
}

type ListItem = {
  id: string;
  title?: string;
  subtitle?: ReactNode;
  children?: ReactNode;
};

interface PageInfo {
  __typename: string;
  endCursor: null | number;
  startCursor: null | number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}
type NullArray<T> = Array<null | T | NullArray<T>>;
interface Page {
  __typename: string;
  edges: NullArray<string>;
  nodes: NullArray<string>;
  pageInfo: PageInfo;
}
function getPageInfo<T>(data?: T): PageInfo | undefined {
  if (!data) return;
  for (const key in data) {
    const page = data?.[key] as Page;
    if (page?.pageInfo) {
      return page?.pageInfo;
    }
  }
};

export const makeAdminList =
  <T extends object>(
    entity: AdminEntity,
    document: TypedDocumentNode<T, { first?: number; cursor?: number }>,
  ) =>
  <Orig,>(getList: (x: T) => Orig[] | undefined) =>
  <New extends ListItem>(mapper: (x: Orig) => New) =>
  ({
    indexedFields = ['id', 'title'],
    Header,
    pageSize = undefined,
  }: {
    pageSize?: number;
    indexedFields?: (keyof New)[];
    Header?: React.JSXElementConstructor<{}>;
  }): React.JSXElementConstructor<{}> =>
    function AdminEntityList() {
      const [cursor, setCursor] = React.useState<number|undefined>(undefined);
      const [{ data, fetching }] = useQuery({query: document, variables: { first: pageSize, cursor } });
      const loadMore = React.useCallback(() => {
        const info = getPageInfo(data);
        if (info?.endCursor) {
          setCursor(info.endCursor);
        }
      }, [data]);
      const hasMore = getPageInfo(data)?.hasNextPage !== false;

      const nodes = React.useMemo(() => {
        if (!data) {
          return [];
        }
        return (getList(data) || []).map((x) => {
          const y = mapper(x);
          return {...y, href: entity.editRoute(y.id) };
        });
      }, [data]);
      const router = useRouter();
      const [search, setSearch] = React.useState('');
      const fuzzy = useFuzzySearch(nodes, indexedFields, search);

      return (
        <List>
          <List.TitleBar title={entity.name(2)}>
            <List.TitleButton
              active={router.asPath.endsWith('add')}
              icon={Plus}
              href={entity.addRoute}
            >
              Vytvořit
            </List.TitleButton>

            {Header && (
              <div className="mt-2 w-full flex gap-2 justify-end"><Header /></div>
            )}

            <TextField
              type="search"
              className="w-full mt-2"
              placeholder="Vyhledat..."
              value={search}
              onChange={(e) => setSearch(e.currentTarget.value)}
            />
          </List.TitleBar>

          <Virtuoso<New & { href: Route }, FooterContext>
            className="grow h-full overflow-y-auto scrollbar"
            data={fuzzy}
            itemContent={RenderItem}
            components={{ Footer: (hasMore && pageSize) ? Footer : undefined }}
            context={{ router, loading: fetching, loadMore }}
          />
        </List>
      );
    };

function RenderItem(
  _n: number,
  item: {
    id: string;
    href: Route;
    title?: ReactNode;
    subtitle?: ReactNode;
    children?: ReactNode;
  },
  { router }: FooterContext,
) {
  const id = fromSlugArray(router.query.id);
  return (
    <List.Item
      key={item.id}
      active={id === item.id}
      href={item.href}
      title={item.title}
      subtitle={item.subtitle}
    >
      {item.children}
    </List.Item>
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
