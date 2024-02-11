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

type Tabs = (TabMenuProps['options'][0] & { contents: React.ReactNode })[];

const Page = () => {
  const { persons } = useAuth();
  const [{ data }] = useQuery({ query: MyMembershipApplicationsDocument });
  const [variant, setVariant] = useQueryParam('tab', StringParam);

  const [tabs, setTabs] = React.useState<Tabs>([]);
  React.useLayoutEffect(() => {
    const newTabs: Tabs = [];
    persons.forEach(x => {
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
  }, [persons, data?.membershipApplicationsList]);

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
