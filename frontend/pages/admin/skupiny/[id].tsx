import { CohortForm } from 'components/CohortForm';
import { DeleteButton } from 'components/DeleteButton';
import { useCohortQuery, useDeleteCohortMutation } from 'lib/graphql/Cohorts';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { CohortsList } from 'components/CohortList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useCohortQuery({ id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteCohortMutation({
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

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
