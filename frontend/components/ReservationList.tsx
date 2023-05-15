import { Plus } from 'react-feather';
import { useRouter } from 'next/router';
import { List } from 'components/layout/List';
import { fullDateFormatter } from 'lib/format-date';
import React from 'react';
import { FuzzyList } from './FuzzyList';
import { TextField } from './TextField';
import { fromSlugArray } from 'lib/slugify';
import { useGqlQuery } from 'lib/query';
import { ReservationListDocument } from 'lib/graphql/Reservation';

export function ReservationList() {
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const { data } = useGqlQuery(ReservationListDocument, {});
  const nodes = React.useMemo(() => {
    return (data?.nabidkas?.nodes || []).map((item) => ({
      id: item.id,
      date: fullDateFormatter.formatRange(new Date(item.nOd), new Date(item.nDo)),
      trainer: item.userByNTrener?.fullName,
    }));
  }, [data]);

  const id = fromSlugArray(router.query.id);

  return (
    <List>
      <List.TitleBar title="Nabídky">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/nabidka/add"
        >
          Nová nabídka
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
        data={nodes}
        fields={['id', 'date', 'trainer']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{pathname: '/admin/nabidka/[id]', query: {id: item.id}}}
            title={`${item.date} ${item.trainer}`}
          />
        )}
      />
    </List>
  );
}
