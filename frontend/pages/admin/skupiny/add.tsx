import { CohortForm } from "components/CohortForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";
import { ListDetailView } from 'components/layout/LayoutWithList';
import { Layout } from 'components/layout/Layout';
import { CohortsList } from 'components/CohortList';

export default function CohortAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="p-4 mt-12 mb-8">
    <CohortForm onSuccess={() => router.back()} />
  </div>;
};

CohortAddPage.getLayout = (page: React.ReactElement) => (
  <Layout>
    <ListDetailView list={<CohortsList />} hasDetail>{page}</ListDetailView>
  </Layout>
);
