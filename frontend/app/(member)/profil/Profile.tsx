'use client';

import { MyMembershipApplicationsDocument } from '@/graphql/MembershipApplication';
import { useActions } from '@/lib/actions';
import { ChangePasswordForm } from '@/ui/forms/ChangePasswordForm';
import { CreateMembershipApplicationForm } from '@/ui/forms/CreateMembershipApplicationForm';
import { PersonView } from '@/ui/PersonView';
import { TabMenu, type TabMenuProps } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { useAuth, useAuthLoading, useTenantConfig } from '@/lib/auth';
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
    variables: { createdBy: auth.user?.id ?? '' },
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
      const applications = data?.membershipApplicationsList ?? [];
      tabs.push(
        ...applications.map((application) => ({
          id: `application-${application.id}`,
          title: `${application.firstName} ${application.lastName}`,
          contents: () => (
            <CreateMembershipApplicationForm
              data={application}
              onRemove={() => setVariant('new-application')}
            />
          ),
        })),
        {
          id: 'new-application',
          title: 'Nová přihláška',
          contents: () => (
            <CreateMembershipApplicationForm
              onCreate={(id) => setVariant(`application-${id}`)}
            />
          ),
        },
      );
    }

    return tabs;
  }, [
    auth.persons,
    data?.membershipApplicationsList,
    enableRegistration,
    setVariant,
  ]);

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
