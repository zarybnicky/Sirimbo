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
import React from 'react';
import { useQuery } from 'urql';
import { StringParam, useQueryParam } from 'use-query-params';
import { useAtomValue } from 'jotai';
import { tenantConfigAtom } from '@/ui/state/auth';
import { LockKeyhole } from 'lucide-react';
import { useActions } from '@/lib/actions';

type Tabs = TabMenuProps['options'];

export default function ProfilePage() {
  const auth = useAuth();
  const { enableRegistration } = useAtomValue(tenantConfigAtom);
  const [{ data }] = useQuery({
    query: MyMembershipApplicationsDocument,
    pause: !enableRegistration,
  });
  const [variant, setVariant] = useQueryParam('person', StringParam);
  const actions = useActions(
    [
      {
        id: 'profile.changePassword',
        primary: true,
        label: 'Změnit heslo',
        icon: LockKeyhole,
        visible: () => true,
        type: 'dialog' as const,
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
      <PageHeader title="Můj profil" actions={actions} />

      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </Layout>
  );
}
