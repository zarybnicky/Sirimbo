import { useAuth } from '@/ui/use-auth';
import React from 'react';
import { ChangePasswordDialog } from '@/ui/ChangePasswordDialog';
import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import { PersonView } from '@/ui/PersonView';
import { useQuery } from 'urql';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { CreateMembershipApplicationButton, MembershipApplicationCard } from '@/ui/forms/CreateMembershipApplicationForm';
import { TabMenu, TabMenuProps } from '@/ui/TabMenu';
import { StringParam, useQueryParam } from 'use-query-params';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';

type Tabs = (TabMenuProps['options'][0] & { contents: React.ReactNode })[];

const Page = () => {
  const auth = useAuth();
  const [{ data }] = useQuery({ query: MyMembershipApplicationsDocument });
  const [variant, setVariant] = useQueryParam('person', StringParam);

  const [tabs, setTabs] = React.useState<Tabs>([]);
  useLayoutEffect(() => {
    const newTabs: Tabs = [];
    auth.persons.forEach(x => {
      newTabs.push({
        id: x.id,
        label: x.name,
        contents: <PersonView key={x.id} id={x.id} />
      });
    });
    newTabs.push({
      id: 'applications',
      label: 'Přihlášky člena',
      contents: <React.Fragment key="applications">
        {data?.membershipApplicationsList?.map(x => (
          <MembershipApplicationCard key={x.id} item={x} />
        ))}
        <CreateMembershipApplicationButton />
      </React.Fragment>
    });
    setTabs(newTabs)
  }, [auth.persons, data?.membershipApplicationsList]);

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
