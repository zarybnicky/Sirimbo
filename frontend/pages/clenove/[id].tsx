import React from 'react';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { PersonList } from '@app/ui/PersonList';
import { WithSidebar } from '@app/ui/WithSidebar';
import { PersonView } from '@app/ui/PersonView';
import Link from 'next/link';
import { CornerLeftUp } from 'lucide-react';

function PersonPage() {
  return (
    <Layout requireMember>
      <WithSidebar sidebar={<PersonList />}>
        <div className="lg:none">
          <Link href="/clenove">
            <CornerLeftUp className="w-4 h-4 mr-2" />
            Zpět na seznam
          </Link>
        </div>

        <PersonView id={fromSlugArray(useRouter().query.id)} />
      </WithSidebar>
    </Layout>
  );
}

export default PersonPage;
