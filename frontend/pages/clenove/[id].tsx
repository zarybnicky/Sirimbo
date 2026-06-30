import { Layout } from '@/ui/Layout';
import { PersonList } from '@/ui/lists/PersonList';
import { WithSidebar } from '@/ui/WithSidebar';
import { PersonView } from '@/ui/PersonView';
import Link from 'next/link';
import { CornerLeftUp } from 'lucide-react';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { PersonMembershipsDocument } from '@/graphql/Person';

const QueryParams = z.object({
  id: zRouterId,
});

function PersonPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id },
    pause: !id,
  });

  return (
    <Layout requireMember>
      <NextSeo title={data?.person?.name || 'Člen'} />
      <WithSidebar sidebar={<PersonList />}>
        <div className="lg:hidden pt-4">
          <Link href="/clenove" className="flex gap-2">
            <CornerLeftUp className="size-4" />
            Zpět na seznam
          </Link>
        </div>

        <PersonView id={id} />
      </WithSidebar>
    </Layout>
  );
}

export default PersonPage;
