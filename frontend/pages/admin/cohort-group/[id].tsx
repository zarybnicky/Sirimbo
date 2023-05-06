import { CohortGroupForm } from 'components/CohortGroupForm';
import { DeleteButton } from 'components/DeleteButton';
import {
  useCohortGroupListQuery,
  useCohortGroupQuery,
  useDeleteCohortGroupMutation,
} from 'lib/graphql/CohortGroup';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { CohortGroupList } from 'components/CohortGroupList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { useQueryClient } from '@tanstack/react-query';
import { SelectElement } from 'components/SelectElement';
import { CohortListForm } from 'components/CohortListForm';

export default function CohortGroupEditPage() {
  const router = useRouter();
  const queryClient = useQueryClient();
  const { id } = router.query;
  const { data } = useCohortGroupQuery(
    { id: id as string },
    { enabled: !!id, cacheTime: 0 },
  );
  const { mutateAsync: doDelete } = useDeleteCohortGroupMutation({
    onSuccess() {
      router.push('/admin/cohort-group');
      queryClient.invalidateQueries(useCohortGroupListQuery.getKey());
    },
  });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/cohort-group"
        title={data?.cohortGroup?.name || '(Bez názvu)'}
      >
        <DeleteButton
          onDelete={() => doDelete({ id: id as string })}
          title="smazat tréninkový program"
        />
      </Item.Titlebar>
      {data && <CohortGroupForm data={data.cohortGroup || undefined} />}
      {data?.cohortGroup && <CohortListForm data={data.cohortGroup}/>}
    </Item>
  );
}

CohortGroupEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortGroupList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
