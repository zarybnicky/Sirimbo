import React from 'react';
import { Plus } from 'react-feather';
import { useRouter } from 'next/router';
import { List } from 'components/layout/List';
import { TextField } from 'components/TextField';
import { FuzzyList } from 'components/FuzzyList';
import { useEventListQuery } from 'lib/graphql/Event';
import { fullDateFormatter } from 'lib/format-date';
import { fromSlugArray } from 'lib/slugify';

export const EventList = () => {
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const { data } = useEventListQuery();
  const id = fromSlugArray(router.query.id);

  return (
    <List>
      <List.TitleBar title="Akce">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/akce/add"
        >
          Nová akce
        </List.TitleButton>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </List.TitleBar>

      <List.Scroll>
        <FuzzyList
          data={data?.events?.nodes || []}
          fields={['id', 'name']}
          search={search}
          renderItem={(item) => (
            <List.Item
              key={item.id}
              active={id === item.id}
              href={{ pathname: '/admin/akce/[id]', query: { id: item.id } }}
              title={item.name}
              subtitle={fullDateFormatter.formatRange(
                new Date(item.since),
                new Date(item.until),
              )}
            />
          )}
        />
      </List.Scroll>
    </List>
  );
};