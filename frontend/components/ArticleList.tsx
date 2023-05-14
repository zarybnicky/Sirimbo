import * as React from 'react';
import { useArticlesQuery } from 'lib/graphql/Articles';
import { useRouter } from 'next/router';
import { Plus } from 'react-feather';
import { fullDateFormatter } from 'lib/format-date';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import { TextField } from './TextField';
import { fromSlugArray } from 'lib/slugify';

export function ArticleList() {
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const { data, refetch } = useArticlesQuery();
  const id = fromSlugArray(router.query.id);

  return (
    <List>
      <List.TitleBar title="Nabídky">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/aktuality/add"
        >
          Nový článek
        </List.TitleButton>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </List.TitleBar>

      <FuzzyList
        data={data?.aktualities?.nodes || []}
        fields={['id', 'atJmeno']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{ pathname: '/admin/aktuality/[id]', query: { id: item.id } }}
            title={item.atJmeno}
            subtitle={
              item.atTimestampAdd
                ? fullDateFormatter.format(new Date(item.atTimestampAdd))
                : ''
            }
          />
        )}
      />
    </List>
  );
}
