'use client';

import { MyMembershipApplicationsDocument } from '@/graphql/CurrentUser';
import { useActions } from '@/lib/actions';
import { useTenantConfig } from '@/ui/state/auth';
import { ChangePasswordForm } from '@/ui/forms/ChangePasswordForm';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { PersonView } from '@/ui/PersonView';
import { TabMenu, type TabMenuProps } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { LockKeyhole } from 'lucide-react';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';

type Tabs = TabMenuProps['options'];

export function Profile() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableRegistration } = useTenantConfig();
  const [{ data }] = useQuery({
    query: MyMembershipApplicationsDocument,
    pause: authLoading || !auth.user || !enableRegistration,
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
  const tabs = React.useMemo<Tabs>(() => {
    const tabs: Tabs = auth.persons.map((person) => ({
      id: person.id,
      title: person.name,
      contents: () => <PersonView id={person.id} />,
    }));

    if (enableRegistration) {
      tabs.push({
        id: 'applications',
        title: 'Přihlášky člena',
        contents: () => (
          <>
            {data?.membershipApplicationsList?.map((application) => (
              <Dialog key={application.id}>
                <DialogTrigger.Edit
                  text={`${application.firstName} ${application.lastName}`}
                />
                <DialogContent>
                  <CreateMembershipApplicationForm data={application} />
                </DialogContent>
              </Dialog>
            ))}
            <Dialog>
              <DialogTrigger.Add text="Přihláška nového člena" />
              <DialogContent>
                <CreateMembershipApplicationForm />
              </DialogContent>
            </Dialog>
          </>
        ),
      });
    }

    return tabs;
  }, [auth.persons, data?.membershipApplicationsList, enableRegistration]);

  if (authLoading || !auth.user) return null;

  return (
    <>
      <PageHeader title="Můj profil" actions={actions} />
      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </div>
    </>
  );
}
