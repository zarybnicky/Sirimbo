import { useRouter } from 'next/router';
import { usePaymentItemListQuery } from "lib/graphql/Payment";
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { List } from "components/layout/List";
import { FuzzyList } from "components/FuzzyList";
import React from 'react';

export default function PlatbyItemListPage() {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;
  const { data } = usePaymentItemListQuery();
  const [search, setSearch] = React.useState('');

  return <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
    <Button href="/admin/platby/items/add">Nová platba</Button>

    <FuzzyList
      data={data?.platbyItems?.nodes || []}
      fields={['id', 'piAmount', 'piDate', 'piIdUser', 'piIdCategory']}
      search={search}
      renderItem={(item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/platby/items/${item.id}`}
          title={`${item.piAmount} ${item.platbyCategoryByPiIdCategory?.pcName} ${fullDateFormatter.format(new Date(item.piDate))} ${item.userByPiIdUser?.uJmeno} ${item.userByPiIdUser?.uPrijmeni}`}
        />
      )}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby, PermissionLevel.P_OWNED,
);
