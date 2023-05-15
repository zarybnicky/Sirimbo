import { CohortGroupForm } from 'components/CohortGroupForm';
import { DeleteButton } from 'components/DeleteButton';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { CohortGroupList } from 'components/CohortGroupList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { useQueryClient } from '@tanstack/react-query';
import { CohortListForm } from 'components/CohortListForm';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import {
  CohortGroupDocument,
  CohortGroupListDocument,
  DeleteCohortGroupDocument,
} from 'lib/graphql/CohortGroup';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const queryClient = useQueryClient();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(
    CohortGroupDocument,
    { id },
    { enabled: !!id, cacheTime: 0 },
  );
  const { mutateAsync: doDelete } = useGqlMutation(DeleteCohortGroupDocument, {
    onSuccess() {
      router.push('/admin/cohort-group');
      queryClient.invalidateQueries(getGqlKey(CohortGroupListDocument, {}));
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
      {data?.cohortGroup && <CohortListForm data={data.cohortGroup} />}
    </Item>
  );
};

Page.list = <CohortGroupList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = 'Tréninkové programy';

export default Page;
