'use client';

import {
  PendingMembershipApplicationsDocument,
  type MembershipApplicationFragment,
} from '@/graphql/MembershipApplication';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { formatAddress, moneyFormatter } from '@/ui/format';
import { MembershipApplicationForm } from '@/ui/forms/MembershipApplicationForm.tsx';
import { LocationForm } from '@/ui/forms/LocationForm';
import { TenantForm } from '@/ui/forms/TenantForm.tsx';
import { useAuth } from '@/lib/auth';
import { Pencil, PinIcon } from 'lucide-react';
import Link from 'next/link';
import Image from 'next/image';
import { useQuery } from 'urql';
import { ClubDocument, type ClubQuery } from '@/graphql/Tenant';
import { useActionMap, useActions } from '@/lib/actions';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { locationActions } from '@/lib/actions/location';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { ActionRow } from '@/ui/ActionRow';
import { CstsIdBackfillWidget } from '@/ui/CstsIdBackfillWidget';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';

type ClubData = NonNullable<ClubQuery['tenant']>;

export function Club() {
  const auth = useAuth();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data: tenant }] = useQuery({
    query: ClubDocument,
    variables: { showInLists: auth.isAdmin ? undefined : true },
  });
  const [{ data: applications }] = useQuery({
    query: PendingMembershipApplicationsDocument,
  });
  const tenantActions = useActions(
    [
      {
        id: 'tenant.edit',
        group: 'primary',
        label: 'Upravit klub',
        icon: Pencil,
        requireAdmin: true,
        render: () => <TenantForm />,
      },
    ],
    tenant?.tenant,
  );

  if (!tenant?.tenant) return null;
  const club = tenant.tenant;
  const pendingApplications = applications?.membershipApplicationsList ?? [];

  return (
    <>
      <PageHeader title="Klub" actions={tenantActions} />
      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="info" title="Informace">
          <ClubInformation club={club} />
        </Tab>
        <Tab id="trainers" title={`Trenéři (${club.tenantTrainersList.length})`}>
          <ClubTrainers club={club} />
        </Tab>
        <Tab
          id="administrators"
          title={`Správci (${club.tenantAdministratorsList.length})`}
          requireAdmin
        >
          <ClubAdministrators club={club} />
        </Tab>
        {pendingApplications.length > 0 && (
          <Tab
            id="applications"
            title={`Žádosti o členství (${pendingApplications.length})`}
            requireAdmin
          >
            <ClubApplications applications={pendingApplications} />
          </Tab>
        )}
        <Tab id="csts" title="ČSTS IDT" requireAdmin>
          <CstsIdBackfillWidget />
        </Tab>
      </TabMenu>
    </>
  );
}

const ClubInformation = React.memo(function ClubInformation({
  club,
}: {
  club: ClubData;
}) {
  const auth = useAuth();
  const locationActionMap = useActionMap(locationActions, club.locationsList);
  return (
    <>
      <h2 className="mb-2 text-lg font-bold">O klubu</h2>
      {club.description?.trim() ? (
        <RichTextView value={club.description} />
      ) : auth.isAdmin ? (
        <p className="text-sm text-neutral-10">(Nevyplněno)</p>
      ) : null}

      <div className="mt-4 flex flex-wrap items-baseline justify-between gap-4">
        <h2 className="mb-2 text-lg font-bold">Místa</h2>
        {auth.isAdmin && (
          <Dialog>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <LocationForm />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {club.locationsList.length > 0 ? (
        club.locationsList.map((item) => (
          <ActionRow key={item.id} actions={locationActionMap.get(item.id)!}>
            <Link
              className="flex min-w-0 items-center gap-3"
              href={`/lokality/${item.id}`}
            >
              <span className="relative flex h-14 w-20 shrink-0 items-center justify-center overflow-hidden rounded bg-neutral-3">
                {item.coverImage ? (
                  <Image
                    fill
                    unoptimized
                    src={item.coverImage.url}
                    alt=""
                    sizes="80px"
                    className="object-cover"
                  />
                ) : (
                  <PinIcon className="size-5 text-neutral-9" aria-hidden="true" />
                )}
              </span>
              <span className="min-w-0 text-sm">
                <span className="font-bold underline">{item.name}</span>
                <span className="block truncate text-neutral-11">
                  {formatAddress(item.address)}
                </span>
              </span>
            </Link>
          </ActionRow>
        ))
      ) : (
        <p className="text-sm text-neutral-10">Nejsou přidána žádná místa.</p>
      )}
    </>
  );
});

const ClubTrainers = React.memo(function ClubTrainers({ club }: { club: ClubData }) {
  const auth = useAuth();
  const trainerActionMap = useActionMap(tenantTrainerActions, club.tenantTrainersList);
  return (
    <>
      {club.tenantTrainersList.map((item) => (
        <ActionRow key={item.id} actions={trainerActionMap.get(item.id)!}>
          <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
            {!item.person ? (
              '?'
            ) : (
              <Link
                className="underline font-bold w-fit min-w-0"
                href={`/clenove/${item.person.id}`}
              >
                {item.person.name}
              </Link>
            )}
            {auth.isAdmin && (
              <div className="self-end">
                {moneyFormatter.format({
                  amount: item.memberPrice45MinAmount,
                  currency: item.currency,
                }) || '-'}{' '}
                {item.guestPrice45MinAmount &&
                item.memberPrice45MinAmount !== item.guestPrice45MinAmount
                  ? `(${moneyFormatter.format({ amount: item.guestPrice45MinAmount, currency: item.currency })})`
                  : ''}
                {' / 45min'}
              </div>
            )}
          </div>
        </ActionRow>
      ))}
    </>
  );
});

const ClubAdministrators = React.memo(function ClubAdministrators({
  club,
}: {
  club: ClubData;
}) {
  const administratorActionMap = useActionMap(
    tenantAdministratorActions,
    club.tenantAdministratorsList,
  );
  return (
    <>
      {club.tenantAdministratorsList.map((item) => (
        <ActionRow key={item.id} actions={administratorActionMap.get(item.id)!}>
          {!item.person ? (
            '?'
          ) : (
            <Link
              className="underline font-bold text-sm py-1"
              href={`/clenove/${item.person.id}`}
            >
              {item.person.name}
            </Link>
          )}
        </ActionRow>
      ))}
    </>
  );
});

const ClubApplications = React.memo(function ClubApplications({
  applications,
}: {
  applications: MembershipApplicationFragment[];
}) {
  return (
    <>
      {applications.map((item) => (
        <Dialog key={item.id}>
          <DialogTrigger.Edit
            className="my-2 justify-start"
            text={`${item.firstName} ${item.lastName}`}
          />
          <DialogContent>
            <MembershipApplicationForm data={item} />
          </DialogContent>
        </Dialog>
      ))}
    </>
  );
});
