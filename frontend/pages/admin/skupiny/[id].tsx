import { CohortForm } from "components/CohortForm";
import { DeleteButton } from "components/DeleteButton";
import { useCohortQuery, useDeleteCohortMutation } from "lib/graphql/Cohorts";
import { useRouter } from "next/router";
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { CohortsList } from 'components/CohortList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function CohortEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCohortQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteCohortMutation({
    onSuccess: () => router.push('/admin/skupiny'),
  });
  return <Item>
    <Item.Titlebar backHref="/admin/skupiny" title={data?.skupiny?.sName || '(Bez názvu)'}>
      <DeleteButton onDelete={() => doDelete({ id: id as string })} title="smazat skupinu" />
    </Item.Titlebar>
    {data && <CohortForm data={data.skupiny || undefined} />}
  </Item>;
};

CohortEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
);
