import { CohortGroupForm } from 'components/CohortGroupForm';
import { DeleteButton } from 'components/DeleteButton';
import {
  useCohortGroupListQuery,
  useCohortGroupQuery,
  useDeleteCohortGroupMutation,
} from 'lib/graphql/CohortGroup';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { CohortGroupList } from 'components/CohortGroupList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { useQueryClient } from '@tanstack/react-query';
import { CohortListForm } from 'components/CohortListForm';
import { type NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const queryClient = useQueryClient();
  const id = fromSlugArray(router.query.id);
  const { data } = useCohortGroupQuery({ id }, { enabled: !!id, cacheTime: 0 });
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
          onDelete={() => doDelete({ id })}
          title="smazat tréninkový program"
        />
      </Item.Titlebar>
      {data && <CohortGroupForm data={data.cohortGroup || undefined} />}
      {data?.cohortGroup && <CohortListForm data={data.cohortGroup}/>}
    </Item>
  );
}

Page.list = <CohortGroupList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
