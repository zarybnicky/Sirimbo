import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { List } from 'components/layout/List';
import { TextField } from 'components/TextField';
import { fromSlugArray } from 'lib/slugify';
import { useRouter } from 'next/router';
import { Route } from 'nextjs-routes';
import React, { ReactNode } from 'react';
import { Plus } from 'lucide-react';
import { Virtuoso } from 'react-virtuoso';
import { SubmitButton } from 'components/SubmitButton';
import { useFuzzySearch } from 'lib/use-fuzzy-search';
import { NextRouter } from 'next/router';
import { useQuery } from 'urql';

export interface AdminListEntity {
  name: (num: number) => string;
  addRoute: Route | Exclude<Route, { query: any }>['pathname'];
  editRoute: (id: string) => Route;
}

type ListItem = {
  id: string;
  title?: string;
  subtitle?: ReactNode;
  children?: ReactNode;
};

export const makeAdminList =
  <T,>(
    entity: AdminListEntity,
    document: TypedDocumentNode<T, { limit?: number; offset?: number }>,
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
      const [variables, setVariables] = React.useState({
        limit: pageSize || undefined,
        offset: 0,
      });
      const loadMore = React.useCallback(() => {
        setVariables(x => ({...x, offset: x.offset + (x.limit ?? 0)}))
      }, []);
      const [{ data, fetching }] = useQuery({query: document, variables });
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
            components={{ Footer: pageSize ? Footer : undefined }}
            context={{ router, loading: fetching, loadMore }}
          />
        </List>
      );
    };

function RenderItem(
  _n: number,
  item: {
    id: string;
    href: Route | Exclude<Route, { query: any }>['pathname'];
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
