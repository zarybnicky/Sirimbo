import { CohortForm } from 'components/CohortForm';
import { DeleteButton } from 'components/DeleteButton';
import { CohortDocument, DeleteCohortDocument } from 'lib/graphql/Cohorts';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { CohortsList } from 'components/CohortList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlMutation, useGqlQuery } from 'lib/query';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(CohortDocument, { id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useGqlMutation(DeleteCohortDocument, {
    onSuccess: () => router.push('/admin/skupiny'),
  });

  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/skupiny"
        title={data?.skupiny?.sName || '(Bez názvu)'}
      >
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat skupinu" />
      </Item.Titlebar>
      {data && <CohortForm data={data.skupiny || undefined} />}
    </Item>
  );
};

Page.list = <CohortsList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
