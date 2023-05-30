import { CohortForm } from 'components/CohortForm';
import { DeleteButton } from 'components/DeleteButton';
import { CohortDocument, DeleteCohortDocument } from 'lib/graphql/Cohorts';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortList } from 'lib/entity-lists';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: CohortDocument, variables: { id }, pause: !!id });

  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/skupiny"
        title={data?.skupiny?.sName || '(Bez názvu)'}
      >
        <DeleteButton
          doc={DeleteCohortDocument}
          id={id}
          onDelete={() => router.push('/admin/skupiny')}
          title="smazat skupinu"
        />
      </Item.Titlebar>
      {data && <CohortForm data={data.skupiny || undefined} />}
    </Item>
  );
};

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
