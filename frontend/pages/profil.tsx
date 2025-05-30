import { Layout } from '@/components/layout/Layout';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { ChangePasswordForm } from '@/ui/forms/ChangePasswordForm';
import { PersonView } from '@/ui/PersonView';
import { TabMenu, type TabMenuProps } from '@/ui/TabMenu';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { useAuth } from '@/ui/use-auth';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { useQuery } from 'urql';
import { StringParam, useQueryParam } from 'use-query-params';

type Tabs = TabMenuProps['options'];

export default function ProfilePage() {
  const auth = useAuth();
  const [{ data }] = useQuery({ query: MyMembershipApplicationsDocument });
  const [variant, setVariant] = useQueryParam('person', StringParam);

  const [tabs, setTabs] = React.useState<Tabs>([]);
  useLayoutEffect(() => {
    const newTabs: Tabs = auth.persons.map(x => ({
      id: x.id,
      title: x.name,
      contents: () => <PersonView key={x.id} id={x.id} />
    }));

    newTabs.push({
      id: 'applications',
      title: 'Přihlášky člena',
      contents: () => <React.Fragment key="applications">
        {data?.membershipApplicationsList?.map(x => (
          <Dialog key={x.id}>
            <DialogTrigger.Edit text={`${x.firstName} ${x.lastName}`} />
            <DialogContent>
              <CreateMembershipApplicationForm data={x} />
            </DialogContent>
          </Dialog>
        ))}
        <Dialog>
          <DialogTrigger.Add text="Přihláška nového člena" />
          <DialogContent>
            <CreateMembershipApplicationForm />
          </DialogContent>
        </Dialog>
      </React.Fragment>
    });
    setTabs(newTabs)
  }, [auth.persons, data?.membershipApplicationsList]);

  return (
    <Layout requireUser>
      <TitleBar title="Můj profil">
        <Dialog>
          <DialogTrigger size="sm" text="Změnit heslo" />
          <DialogContent>
            <ChangePasswordForm />
          </DialogContent>
        </Dialog>
      </TitleBar>

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </Layout>
  );
};
