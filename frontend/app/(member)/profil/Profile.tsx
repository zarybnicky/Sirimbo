'use client';

import { MyMembershipApplicationsDocument } from '@/graphql/MembershipApplication';
import { useActions } from '@/lib/actions';
import { ChangePasswordForm } from '@/ui/forms/ChangePasswordForm';
import { MembershipApplicationForm } from '@/ui/forms/MembershipApplicationForm.tsx';
import { PersonView } from '@/ui/PersonView';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { useAuth, useAuthLoading, useTenantConfig } from '@/lib/auth';
import { LockKeyhole } from 'lucide-react';
import { parseAsString, useQueryState } from 'nuqs';
import { useCallback } from 'react';
import { useQuery } from 'urql';

export function Profile() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableRegistration } = useTenantConfig();
  const [{ data }] = useQuery({
    query: MyMembershipApplicationsDocument,
    variables: { createdBy: auth.userId ?? '' },
    pause: authLoading || !auth.isLoggedIn || !enableRegistration,
  });
  const [variant, setVariant] = useQueryState(
    'person',
    parseAsString.withOptions({ history: 'push' }),
  );
  const onApplicationCreated = useCallback(
    (id: string) => setVariant(`application-${id}`),
    [setVariant],
  );
  const onApplicationRemoved = useCallback(
    () => setVariant('new-application'),
    [setVariant],
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
  if (authLoading || !auth.isLoggedIn) return null;

  return (
    <>
      <PageHeader title="Můj profil" actions={actions} />
      <div className="max-w-full">
        <TabMenu selected={variant} onSelect={setVariant}>
          {auth.persons.map((person) => (
            <Tab key={person.id} id={person.id} title={person.name}>
              <PersonView id={person.id} />
            </Tab>
          ))}
          {enableRegistration && (
            <>
              {data?.membershipApplicationsList?.map((application) => (
                <Tab
                  key={application.id}
                  id={`application-${application.id}`}
                  title={`${application.firstName} ${application.lastName}`}
                >
                  <MembershipApplicationForm
                    data={application}
                    onRemove={onApplicationRemoved}
                  />
                </Tab>
              ))}
              <Tab id="new-application" title="Nová přihláška">
                <MembershipApplicationForm
                  onCreate={onApplicationCreated}
                />
              </Tab>
            </>
          )}
        </TabMenu>
      </div>
    </>
  );
}
