/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import { StatusPage } from '@/ui/StatusPage';

export default function NotFound() {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <StatusPage status="not-found" />
    </Layout>
  );
}
