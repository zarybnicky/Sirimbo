import * as React from 'react';
import { useRouter } from 'next/router';
import { usePaymentCategoryListQuery } from 'lib/graphql/Payment';
import { Button } from 'components/Button';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import { fromSlugArray } from 'lib/slugify';

export default function PlatbyCategoryListPage() {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [search, setSearch] = React.useState('');
  const { data } = usePaymentCategoryListQuery();

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
      <Button href="/admin/platby/structure/category/add">Nová platba</Button>

      <FuzzyList
        data={data?.platbyCategories?.nodes || []}
        fields={['id', 'pcName', 'pcSymbol']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{pathname: '/admin/platby/structure/category/[id]', query: {id: item.id}}}
            title={item.pcName}
            subtitle={item.pcSymbol}
          />
        )}
      />
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby,
  PermissionLevel.P_OWNED,
);
