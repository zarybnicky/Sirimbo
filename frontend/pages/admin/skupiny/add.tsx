import { CohortForm } from "components/CohortForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";
import { Layout } from 'components/layout/Layout';
import { CohortsList } from 'components/CohortList';
import { withServerPermissions, PermissionKey, PermissionLevel } from "lib/data/use-server-permissions";

export default function CohortAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="p-4 mt-12 mb-8">
    <CohortForm onSuccess={() => router.back()} />
  </div>;
};

CohortAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(PermissionKey.peSkupiny, PermissionLevel.P_OWNED);
