import * as React from 'react';
import { useScheduleListQuery } from 'lib/graphql/Schedule';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';

export default function RozpisAdminList() {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? (id as string) : null;
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
            active={active === item.id}
            href={{pathname: '/admin/rozpis/[id]', query: {id: item.id}}}
            title={item.userByRTrener?.fullName}
            subtitle={item.rDatum ? fullDateFormatter.format(new Date(item.rDatum)) : ''}
          />
        )}
      />
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis,
  PermissionLevel.P_VIEW,
);
