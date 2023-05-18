import * as React from 'react';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { List } from 'components/layout/List';
import { FuzzyList } from 'components/FuzzyList';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';
import { PaymentCategoryListDocument } from 'lib/graphql/Payment';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [search, setSearch] = React.useState('');
  const { data } = useGqlQuery(PaymentCategoryListDocument, {});

  return (
    <div className="container mx-auto max-w-5xl" style={{ padding: '4rem 0 6rem' }}>
      <Button href="/admin/platby/structure/category/add">Nová platba</Button>

      <FuzzyList
        data={data?.platbyCategories?.nodes || []}
        fields={['id', 'pcName', 'pcSymbol']}
        search={search}
        renderItem={(n, item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{
              pathname: '/admin/platby/structure/category/[id]',
              query: { id: item.id },
            }}
            title={item.pcName}
            subtitle={item.pcSymbol}
          />
        )}
      />
    </div>
  );
};

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
