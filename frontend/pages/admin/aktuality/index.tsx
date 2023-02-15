import * as React from 'react';
import { useArticlesQuery } from 'lib/graphql/Articles';
import { useRouter } from 'next/router';
import { Button } from 'components/Button';
import { fullDateFormatter } from 'lib/format-date';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { List } from "components/layout/List";
import { FuzzyList } from "components/FuzzyList";

export default function ArticleAdminList() {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;
  const [search, setSearch] = React.useState('');
  const { data, refetch } = useArticlesQuery();

  return <div className="container mx-auto max-w-5xl mt-12 mb-8">
    <Button href="/admin/aktuality/add">Nový článek</Button>
    <FuzzyList
      data={data?.aktualities?.nodes || []}
      fields={['id', 'atJmeno']}
      search={search}
      renderItem={(item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/aktuality/${item.id}`}
          title={item.atJmeno}
          subtitle={item.atTimestampAdd ? fullDateFormatter.format(new Date(item.atTimestampAdd)) : ''}
        />
      )}
    />
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality, PermissionLevel.P_OWNED,
);
