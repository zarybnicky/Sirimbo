import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { TextField } from '@app/ui/fields/text';
import { fromSlugArray } from '@app/ui/slugify';
import { useRouter } from 'next/router';
import React, { ReactNode } from 'react';
import { Plus } from 'lucide-react';
import { Virtuoso } from 'react-virtuoso';
import { SubmitButton } from '@app/ui/submit';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { NextRouter } from 'next/router';
import { useQuery } from 'urql';
import classNames from 'classnames';
import Link from 'next/link';
import { buttonCls } from '../style/button';

export interface AdminEntity {
  name: (num: number) => string;
  listRoute: string;
  addRoute: string;
  editRoute: (id: string) => string;
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
    disableAdd,
    pageSize = undefined,
  }: {
    pageSize?: number;
    disableAdd?: boolean;
    indexedFields?: (keyof New)[];
    Header?: React.JSXElementConstructor<{}>;
  }): React.ComponentType<{}> =>
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
        <div className="flex flex-col h-full">
          <div className="px-1 py-4 flex items-center justify-between flex-wrap">
            <div className="font-bold first-letter:uppercase">{entity.name(2)}</div>
            {!disableAdd && (
              <a href={entity.addRoute} className={buttonCls({ size: 'sm', variant: router.asPath.endsWith('add') ? 'primary' : 'outline' })}>
                <Plus />
                Vytvořit
              </a>
            )}

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
          </div>

          <Virtuoso<New & { href: string }, FooterContext>
            className="grow h-full overflow-y-auto scrollbar"
            data={fuzzy}
            itemContent={RenderListItem}
            components={{ Footer: (hasMore && pageSize) ? Footer : undefined }}
            context={{ router, loading: fetching, loadMore }}
          />
        </div>
      );
    };

export function RenderListItem(
  _n: number,
  item: {
    id: string;
    href: string;
    title?: ReactNode;
    subtitle?: ReactNode;
    children?: ReactNode;
  },
  { router }: FooterContext,
) {
  const id = fromSlugArray(router.query.id);
  return (
    <Link
      key={item.id}
      href={item.href}
      className={classNames(
        'relative p-2 pl-5 mr-2 my-1 rounded-lg grid',
        id === item.id ? 'font-semibold bg-primary text-white shadow-md' : 'hover:bg-neutral-4',
      )}
    >
      <div>{item.title}</div>
      <div className={classNames('text-sm', id === item.id ? 'text-white' : 'text-neutral-11')}>
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