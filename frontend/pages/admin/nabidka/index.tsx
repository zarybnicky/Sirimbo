import * as React from 'react';
import { useReservationListQuery } from 'lib/graphql/Reservation';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { List } from "components/layout/List";
import { FuzzyList } from "components/FuzzyList";

export default function ReservationAdminList() {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;
  const [search, setSearch] = React.useState('');
  const { data } = useReservationListQuery();

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/nabidka/add">Nová nabídka</Button>
    <FuzzyList
      data={data?.nabidkas?.nodes || []}
      fields={['id', 'userByNTrener']}
      search={search}
      renderItem={(item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/nabidka/${item.id}`}
          title={`${fullDateFormatter.formatRange(new Date(item.nOd), new Date(item.nDo))} ${item.userByNTrener?.fullName}`}
        />
      )}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka, PermissionLevel.P_VIEW,
);
