'use client';

import { MembershipApplicationsDocument } from '@/graphql/MembershipApplication';
import { MembershipApplicationForm } from '@/ui/forms/MembershipApplicationForm.tsx';
import { PersonView } from '@/ui/PersonView';
import { PageHeader } from '@/ui/PageHeader';
import { Tab, TabMenu } from '@/ui/TabMenu';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { useAuth, useAuthLoading, useTenantConfig } from '@/lib/auth';
import { Check, ChevronDown } from 'lucide-react';
import { parseAsString, useQueryState } from 'nuqs';
import { useCallback } from 'react';
import { useQuery } from 'urql';

export function Profile() {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableRegistration } = useTenantConfig();
  const [{ data }] = useQuery({
    query: MembershipApplicationsDocument,
    variables: { condition: { status: 'SENT', createdBy: auth.userId ?? '' } },
    pause: !auth.userId,
  });
  const [variant, setVariant] = useQueryState(
    'person',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [selectedApplication, setSelectedApplication] = useQueryState(
    'application',
    parseAsString.withOptions({ history: 'push' }),
  );
  const onApplicationCreated = useCallback(
    (id: string) => setSelectedApplication(id),
    [setSelectedApplication],
  );
  const onApplicationRemoved = useCallback(
    () => setSelectedApplication('new-application'),
    [setSelectedApplication],
  );
  if (authLoading || !auth.isLoggedIn) return null;

  const applications = data?.membershipApplicationsList ?? [];
  const options = [
    ...auth.persons.map((x) => ({ id: x.id, label: x.name })),
    ...(enableRegistration || applications.length > 0
      ? [{ id: 'applications', label: 'Přihlášky' }]
      : []),
  ];
  const active = options.find((x) => x.id === variant) ?? options[0];
  const person = auth.persons.find((x) => x.id === active?.id);

  const renderTitle = (title: string) => {
    if (options.length < 2) return title;

    return (
      <DropdownMenu>
        <DropdownMenuTrigger className="inline-flex max-w-full items-center gap-2 rounded-md text-left hover:text-accent-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8">
          <span className="min-w-0">{title}</span>
          <ChevronDown className="size-5 shrink-0" aria-hidden="true" />
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start" className="max-w-[calc(100vw-2rem)]">
          {options.map((option) => (
            <DropdownMenuButton
              key={option.id}
              onSelect={() => void setVariant(option.id)}
              aria-current={option.id === active?.id ? 'page' : undefined}
              className="wrap-anywhere"
            >
              {option.label}
              {option.id === active?.id && (
                <Check className="ml-auto size-4 shrink-0" aria-hidden="true" />
              )}
            </DropdownMenuButton>
          ))}
        </DropdownMenuContent>
      </DropdownMenu>
    );
  };

  if (person) {
    return <PersonView key={person.id} id={person.id} renderTitle={renderTitle} />;
  }

  return (
    <>
      <PageHeader title={renderTitle(active?.label ?? 'Můj profil')} />

      {active?.id === 'applications' && (
        <TabMenu selected={selectedApplication} onSelect={setSelectedApplication}>
          {applications.map((x) => (
            <Tab key={x.id} id={x.id} title={`${x.firstName} ${x.lastName}`}>
              <MembershipApplicationForm data={x} onRemove={onApplicationRemoved} />
            </Tab>
          ))}
          {enableRegistration && (
            <Tab id="new-application" title="Nová přihláška">
              <MembershipApplicationForm onCreate={onApplicationCreated} />
            </Tab>
          )}
        </TabMenu>
      )}
    </>
  );
}
