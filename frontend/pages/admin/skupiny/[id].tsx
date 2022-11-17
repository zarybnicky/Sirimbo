import { CohortForm } from "components/CohortForm";
import { DeleteButton } from "components/DeleteButton";
import { useCohortListQuery, useCohortQuery, useDeleteCohortMutation } from "lib/graphql/Cohorts";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { CohortsList } from 'components/CohortList';
import { useQueryClient } from "@tanstack/react-query";

export default function CohortEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCohortQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const queryClient = useQueryClient();
  const refetch = () => queryClient.invalidateQueries(useCohortListQuery.getKey());
  const { mutateAsync: doDelete } = useDeleteCohortMutation({
    onSuccess: () => router.push('/admin/skupiny'),
  });
  return <>
    <Item.Titlebar title={data?.skupiny?.sName}>
      <DeleteButton onDelete={() => doDelete({ id: id as string })} title="smazat skupinu" />
    </Item.Titlebar>
    {data && <CohortForm data={data.skupiny || undefined} onSuccess={refetch} />}
  </>;
};

CohortEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />} isDetail>{page}</Layout>
);
