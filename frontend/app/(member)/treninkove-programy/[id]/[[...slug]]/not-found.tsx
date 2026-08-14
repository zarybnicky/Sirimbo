/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import { StatusPage } from '@/ui/StatusPage';

export default function TrainingProgramNotFound() {
  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <StatusPage status="not-found" />
    </Layout>
  );
}
