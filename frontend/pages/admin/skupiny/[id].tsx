import { CohortForm } from "components/CohortForm";
import { DeleteButton } from "components/DeleteButton";
import { useCohortListQuery, useCohortQuery, useDeleteCohortMutation } from "lib/graphql/Cohorts";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";
import { ListDetailView } from 'components/layout/LayoutWithList';
import { Layout } from 'components/layout/Layout';
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
  return <div className="grow p-4 mt-12 mb-8">
    <DeleteButton key="del" onDelete={doDelete} id={id as string} title="smazat skupinu" />
    {data && <CohortForm data={data.skupiny || undefined} onSuccess={refetch} />}
  </div >;
};

CohortEditPage.getLayout = (page: React.ReactElement) => (
  <Layout>
    <ListDetailView list={<CohortsList />} hasDetail>{page}</ListDetailView>
  </Layout>
);
