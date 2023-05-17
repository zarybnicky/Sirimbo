import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { FuzzyList } from 'components/FuzzyList';
import { List } from 'components/layout/List';
import { TextField } from 'components/TextField';
import { useGqlQuery } from 'lib/query';
import { fromSlugArray } from 'lib/slugify';
import { useRouter } from 'next/router';
import { Route } from 'nextjs-routes';
import React, { ReactNode } from 'react';
import { Plus } from 'react-feather';

export interface AdminListEntity {
  name: (num: number) => string;
  addRoute: Route | Exclude<Route, { query: any }>['pathname'];
  editRoute: (id: string) => Route | Exclude<Route, { query: any }>['pathname'];
}

export const makeAdminList =
  <T,>(entity: AdminListEntity, document: TypedDocumentNode<T, {}>) =>
  <Orig,>(getter: (x: T) => Orig[] | undefined) =>
  <New extends { id: string; title?: string; subtitle?: ReactNode }>(
    mapper: (x: Orig) => New,
  ) =>
    ({indexedFields = ['id', 'title'], headerExtra}: {
      indexedFields?: (keyof New)[];
      headerExtra?: React.ReactNode;
    }): React.JSXElementConstructor<{}> =>
    function AdminEntityList() {
      const { data } = useGqlQuery(document, {});
      const nodes = React.useMemo(() => {
        return data ? (getter(data) || []).map(mapper) : [];
      }, [data]);
      const router = useRouter();
      const [search, setSearch] = React.useState('');
      const id = fromSlugArray(router.query.id);

      return (
        <List>
          <List.TitleBar title={entity.name(2)}>
            <List.TitleButton
              active={router.asPath.endsWith('add')}
              icon={Plus}
              href={entity.addRoute}
            >
              Nový {entity.name(1)}
            </List.TitleButton>

            {headerExtra && (
              <div className="mt-2 w-full flex gap-2 justify-end">{headerExtra}</div>
            )}

            <TextField
              type="search"
              className="w-full mt-2"
              placeholder="Vyhledat..."
              value={search}
              onChange={(e) => setSearch(e.currentTarget.value)}
            />
          </List.TitleBar>

          <FuzzyList
            data={nodes}
            fields={indexedFields}
            search={search}
            renderItem={(item) => (
              <List.Item
                key={item.id}
                active={id === item.id}
                href={entity.editRoute(item.id)}
                title={item.title}
                subtitle={item.subtitle}
              />
            )}
          />
        </List>
      );
    };
