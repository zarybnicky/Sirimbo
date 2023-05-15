import { useRouter } from 'next/router';
import { usePaymentItemListQuery } from 'lib/graphql/Payment';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import React from 'react';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const { data } = usePaymentItemListQuery();
  const [search, setSearch] = React.useState('');
  const id = fromSlugArray(router.query.id);

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
      <Button href="/admin/platby/items/add">Nová platba</Button>

      <FuzzyList
        data={data?.platbyItems?.nodes || []}
        fields={['id', 'piAmount', 'piDate', 'piIdUser', 'piIdCategory']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{ pathname: '/admin/platby/items/[id]', query: { id: item.id } }}
            title={`${item.piAmount} ${
              item.platbyCategoryByPiIdCategory?.pcName
            } ${fullDateFormatter.format(new Date(item.piDate))} ${
              item.userByPiIdUser?.uJmeno
            } ${item.userByPiIdUser?.uPrijmeni}`}
          />
        )}
      />
    </div>
  );
}

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
