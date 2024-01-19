import { useAuth } from '@app/ui/use-auth';
import React from 'react';
import { ChangePasswordDialog } from '@app/ui/ChangePasswordDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import { PersonView } from '@app/ui/PersonView';
import { useQuery } from 'urql';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { CreateMembershipApplicationButton, MembershipApplicationCard } from '@/ui/CreateMembershipApplicationForm';
import { TabMenu, TabMenuProps } from '@/ui/TabMenu';
import { StringParam, useQueryParam } from 'use-query-params';

const Page = () => {
  const { persons } = useAuth();
  const [{ data }] = useQuery({ query: MyMembershipApplicationsDocument });
  const [variant, setVariant] = useQueryParam('tab', StringParam);

  const tabs: (TabMenuProps['options'][0] & {contents: React.ReactNode})[] = [];
  persons.forEach(x => {
    tabs.push({
      id: x.id,
      label: x.name,
      contents: <PersonView key={x.id} id={x.id} />
    });
  });
  tabs.push({
    id: 'applications',
    label: 'Přihlášky člena',
    contents: <>
      {data?.membershipApplicationsList?.map(x => (
        <MembershipApplicationCard key={x.id} item={x} />
      ))}
      <CreateMembershipApplicationButton />
    </>
  });

  return (
    <Layout requireUser>
      <TitleBar title="Můj profil">
        <ChangePasswordDialog />
      </TitleBar>

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4 relative max-w-full">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </Layout>
  );
};

export default Page;
