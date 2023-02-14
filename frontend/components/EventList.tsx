import React from "react";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";
import { TextField } from "components/TextField";
import { FuzzyList } from "components/FuzzyList";
import { useEventListQuery } from "lib/graphql/Event";
import { formatLongDateRange } from "lib/format-date";

export const EventList = () => {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;
  const [search, setSearch] = React.useState('');
  const { data } = useEventListQuery();

  return <List>
    <List.TitleBar title="Akce">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/akce/add">
        Nová akce
      </List.TitleButton>

      <TextField
        type="search" className="w-full mt-2" placeholder="Vyhledat..."
        value={search} onChange={(e) => setSearch(e.currentTarget.value)}
      />
    </List.TitleBar>

    <List.Scroll>
      <FuzzyList
        data={data?.akces?.nodes || []}
        fields={['id', 'aJmeno']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={active === item.id} href={`/admin/akce/${item.id}`}
            title={item.aJmeno}
            subtitle={formatLongDateRange(new Date(item.aOd), new Date(item.aDo))}
          />
        )}
      />
    </List.Scroll>
  </List>;
}
