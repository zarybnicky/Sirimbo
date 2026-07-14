import { Layout } from '@/ui/Layout';
import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { ChangePasswordForm } from '@/ui/forms/ChangePasswordForm';
import { PersonView } from '@/ui/PersonView';
import { TabMenu, type TabMenuProps } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { useAuth } from '@/ui/use-auth';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';
import { useTenantConfig } from '@/ui/state/auth';
import { LockKeyhole } from 'lucide-react';
import { useActions } from '@/lib/actions';
import { NextSeo } from 'next-seo';

type Tabs = TabMenuProps['options'];

export default function ProfilePage() {
  const auth = useAuth();
  const { enableRegistration } = useTenantConfig();
  const [{ data }] = useQuery({
    query: MyMembershipApplicationsDocument,
    pause: !enableRegistration,
  });
  const [variant, setVariant] = useQueryState(
    'person',
    parseAsString.withOptions({ history: 'push' }),
  );
  const actions = useActions(
    [
      {
        id: 'profile.changePassword',
        group: 'primary',
        label: 'Změnit heslo',
        icon: LockKeyhole,
        render: () => <ChangePasswordForm />,
      },
    ],
    {},
  );

  const [tabs, setTabs] = React.useState<Tabs>([]);
  useLayoutEffect(() => {
    const newTabs: Tabs = auth.persons.map((x) => ({
      id: x.id,
      title: x.name,
      contents: () => <PersonView key={x.id} id={x.id} />,
    }));

    if (enableRegistration) {
      newTabs.push({
        id: 'applications',
        title: 'Přihlášky člena',
        contents: () => (
          <React.Fragment key="applications">
            {data?.membershipApplicationsList?.map((x) => (
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
        ),
      });
    }
    setTabs(newTabs);
  }, [auth.persons, data?.membershipApplicationsList, enableRegistration]);

  return (
    <Layout requireUser>
      <NextSeo title="Můj profil" />
      <PageHeader title="Můj profil" actions={actions} />

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </Layout>
  );
}
