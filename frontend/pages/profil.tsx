import { useAuth } from '@app/ui/use-auth';
import React from 'react';
import { ChangePasswordDialog } from '@app/ui/ChangePasswordDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import { PersonView } from '@app/ui/PersonView';
import { useQuery } from 'urql';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { CreateMembershipApplicationButton, MembershipApplicationCard } from '@/ui/CreateMembershipApplicationForm';
import { typographyCls } from '@/ui/style';

const Page = () => {
  const { persons } = useAuth();
  const [{ data }] = useQuery({ query: MyMembershipApplicationsDocument });

  return (
    <Layout requireUser>
      <TitleBar title="Můj profil">
        <ChangePasswordDialog />
      </TitleBar>

      {persons.map((x) => (
        <PersonView key={x.id} id={x.id} />
      ))}

      {!!data?.membershipApplicationsList?.length && (
        <h1 className={typographyCls({ variant: 'smallHeading' })}>
          Přihlášky ke členství
        </h1>
      )}
      {data?.membershipApplicationsList?.map(x => <MembershipApplicationCard key={x.id} item={x} />)}

      <CreateMembershipApplicationButton />
    </Layout>
  );
};

export default Page;
