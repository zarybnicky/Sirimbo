import React from 'react';
import { Layout } from '@/components/layout/Layout';
import { PersonList } from '@/ui/PersonList';
import { WithSidebar } from '@/ui/WithSidebar';
import { PersonView } from '@/ui/PersonView';
import Link from 'next/link';
import { CornerLeftUp } from 'lucide-react';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterId,
});

function PersonPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<PersonList />}>
        <div className="lg:hidden">
          <Link href="/clenove" className="flex gap-2">
            <CornerLeftUp className="w-4 h-4" />
            Zpět na seznam
          </Link>
        </div>

        <PersonView id={id} />
      </WithSidebar>
    </Layout>
  );
}

export default PersonPage;
