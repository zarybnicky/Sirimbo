import { Layout } from 'components/layout/Layout';
import { GetServerSideProps } from 'next';
import { dehydrate, QueryClient } from '@tanstack/react-query';
import { CohortsList } from 'components/CohortList';
import { pool } from 'lib/PgPool';
import { useCohortListQuery } from 'lib/graphql/Cohorts';

export default function CohortsPage() {
  return null;
}

CohortsPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />}>{page}</Layout>
);

const queryClient = new QueryClient();

export const getServerSideProps: GetServerSideProps = async (context) => {
  const { rows: [session] } = await pool.query(`
SELECT u_id, u_group, ss_id, permissions.* FROM session
LEFT JOIN users on u_id=ss_user
LEFT JOIN permissions on u_group=pe_id
WHERE ss_id='${context.req.cookies.PHPSESSID}'
  `);
  if ((session?.pe_skupiny || 0) < 8) {
    const params = new URLSearchParams({ from: context.resolvedUrl });
    return { redirect: { statusCode: 301, destination: `/login?${params}` } };
  }

  queryClient.prefetchQuery(useCohortListQuery.getKey(), useCohortListQuery.fetcher());

  return {
    props: { dehydratedState: dehydrate(queryClient) },
  };
};
