import * as React from 'react';
import { useRouter } from 'next/router';
import { usePaymentGroupListQuery } from 'lib/graphql/Payment';
import { Button } from 'components/Button';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = usePaymentGroupListQuery();

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
      <Button href="/admin/platby/structure/group/add">Nová skupina plateb</Button>

      <FuzzyList
        data={data?.platbyGroups?.nodes || []}
        fields={['id', 'pgName']}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{
              pathname: '/admin/platby/structure/group/[id]',
              query: { id: item.id },
            }}
            title={item.pgName}
          />
        )}
      />
    </div>
  );
};

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];

export default Page;
