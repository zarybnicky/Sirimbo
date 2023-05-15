import * as React from 'react';
import { useScheduleListQuery } from 'lib/graphql/Schedule';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useScheduleListQuery();

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
      <Button href="/admin/rozpis/add">Nový rozpis</Button>

      <FuzzyList
        data={data?.rozpis?.nodes || []}
        fields={['id', 'userByRTrener', 'rDatum']}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{pathname: '/admin/rozpis/[id]', query: {id: item.id}}}
            title={item.userByRTrener?.fullName}
            subtitle={item.rDatum ? fullDateFormatter.format(new Date(item.rDatum)) : ''}
          />
        )}
      />
    </div>
  );
}

Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = "Rozpisy";

export default Page;
