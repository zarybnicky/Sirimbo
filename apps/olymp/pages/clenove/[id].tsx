import React from 'react';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from 'components/layout/Layout';
import { PersonList } from '@app/ui/PersonList';
import { WithSidebar } from '@app/ui/WithSidebar';
import { PersonView } from '@app/ui/PersonView';

function PersonPage() {
  return (
    <Layout requireMember>
      <WithSidebar sidebar={<PersonList />}>
        <PersonView id={fromSlugArray(useRouter().query.id)} />
      </WithSidebar>
    </Layout>
  );
}

export default PersonPage;
