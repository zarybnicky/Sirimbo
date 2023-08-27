import { useAuth } from '@app/ui/use-auth';
import React from 'react';
import { ChangePasswordDialog } from '@app/ui/ChangePasswordDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from 'components/layout/Layout';
import { PersonView } from '@app/ui/PersonView';

const Page = () => {
  const { persons } = useAuth();

  return (
    <Layout requireMember>
      <TitleBar title="Můj profil">
        <ChangePasswordDialog />
      </TitleBar>

      {persons.map((x) => (
        <PersonView key={x.id} id={x.id} />
      ))}
    </Layout>
  );
};

export default Page;
